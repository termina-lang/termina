-- | The discard statement, which takes the read of a fixed-location field only.
module Semantic.Negative.DiscardSpec (spec) where

import Test.Hspec
import Semantic.Errors
import Semantic.Common

-- | A resource with fixed-location fields, a regular field and a method, whose
-- procedure runs the given statement.
resource :: String -> String
resource stmt =
    "struct Regs {\n" ++
    "    data : u32;\n" ++
    "    status : u32;\n" ++
    "};\n" ++
    "function twice(x : u32) -> u32 {\n" ++
    "    return 2 * x;\n" ++
    "}\n" ++
    "interface IFifo {\n" ++
    "    procedure flush(&mut self, n : u32);\n" ++
    "};\n" ++
    "resource class CFifo provides IFifo {\n" ++
    "    regs : loc Regs;\n" ++
    "    status : loc u32;\n" ++
    "    table : loc [u32; 4];\n" ++
    "    count : u32;\n" ++
    "    method read(&priv self) -> u32 {\n" ++
    "        let v : u32 = self->regs.data;\n" ++
    "        return v;\n" ++
    "    }\n" ++
    "    procedure flush(&mut self, n : u32) {\n" ++
    "        " ++ stmt ++ "\n" ++
    "        self->count = self->count + n;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

isNotFixedLocationRead :: Maybe Error -> Bool
isNotFixedLocationRead = \case
  Just EDiscardNotFixedLocationRead -> True
  _ -> False

spec :: Spec
spec = describe "Semantic: discard statement" $ do

  it "accepts the read of a field of a fixed-location struct" $
    runNegativeTestTypeCheck (resource "_ = self->regs.data;") `shouldSatisfy` null

  it "accepts the read of a scalar fixed-location field" $
    runNegativeTestTypeCheck (resource "_ = self->status;") `shouldSatisfy` null

  it "accepts the read of an element of a fixed-location array" $
    runNegativeTestTypeCheck (resource "_ = self->table[n as usize];") `shouldSatisfy` null

  it "rejects the read of a regular field" $
    runNegativeTestTypeCheck (resource "_ = self->count;") `shouldSatisfy` isNotFixedLocationRead

  it "rejects a parameter" $
    runNegativeTestTypeCheck (resource "_ = n;") `shouldSatisfy` isNotFixedLocationRead

  it "rejects an operation on a fixed-location read" $
    runNegativeTestTypeCheck (resource "_ = self->regs.data + 1;") `shouldSatisfy` isNotFixedLocationRead

  it "rejects the value of a method" $
    runNegativeTestTypeCheck (resource "_ = self->read();") `shouldSatisfy` isNotFixedLocationRead

  it "rejects the value of a function" $
    runNegativeTestTypeCheck (resource "_ = twice(n);") `shouldSatisfy` isNotFixedLocationRead
