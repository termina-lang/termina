-- | References to a fixed-location field and to its parts.
module Semantic.Negative.FixedLocationReferenceSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors
import Semantic.Common

-- | A resource with a fixed-location struct and a fixed-location array, whose
-- procedure runs the given statement.
resource :: String -> String
resource stmt =
    "struct Desc {\n" ++
    "    master : [u8; 32];\n" ++
    "    size : u32;\n" ++
    "};\n" ++
    "function read32(v : &u32) -> u32 {\n" ++
    "    return *v;\n" ++
    "}\n" ++
    "function first(d : &[Desc; 2]) -> u32 {\n" ++
    "    return d[0].size;\n" ++
    "}\n" ++
    "function size_of(d : &Desc) -> u32 {\n" ++
    "    return d->size;\n" ++
    "}\n" ++
    "interface IR {\n" ++
    "    procedure get(&mut self, r : &mut u32);\n" ++
    "};\n" ++
    "resource class CR provides IR {\n" ++
    "    status : loc u32;\n" ++
    "    single : loc Desc;\n" ++
    "    tables : loc [Desc; 4];\n" ++
    "    procedure get(&mut self, r : &mut u32) {\n" ++
    "        " ++ stmt ++ "\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

isReferenceTo :: String -> Maybe Error -> Bool
isReferenceTo field = \case
  Just (EReferenceToFixedLocation f) -> f == field
  _ -> False

-- | The value assigned to a scalar fixed location is checked against the located type.
isScalarMismatch :: Maybe Error -> Bool
isScalarMismatch = \case
  Just (EAssignmentExprMismatch TUInt32 TBool) -> True
  _ -> False

spec :: Spec
spec = describe "Semantic: references to fixed-location fields" $ do

  it "accepts reading a field of a fixed-location struct" $
    runNegativeTestTypeCheck (resource "*r = self->single.size + self->tables[1].size;") `shouldSatisfy` null

  it "rejects a reference to a field of a fixed-location struct" $
    runNegativeTestTypeCheck (resource "*r = read32(&self->single.size);") `shouldSatisfy` isReferenceTo "single"

  it "rejects a reference to a whole fixed-location field" $
    runNegativeTestTypeCheck (resource "*r = size_of(&self->single);") `shouldSatisfy` isReferenceTo "single"

  it "rejects a reference to an element of a fixed-location array" $
    runNegativeTestTypeCheck (resource "*r = size_of(&self->tables[1]);") `shouldSatisfy` isReferenceTo "tables"

  it "rejects a slice of a fixed-location array" $
    runNegativeTestTypeCheck (resource "*r = first(&self->tables[0..2]);") `shouldSatisfy` isReferenceTo "tables"

  it "accepts writing a binary operation to a scalar fixed location" $
    runNegativeTestTypeCheck (resource "self->status = (*r << 8 : u32) | (self->status & 0xFF : u32);") `shouldSatisfy` null

  it "accepts writing an initializer to a whole fixed-location struct" $
    runNegativeTestTypeCheck (resource "self->single = {master = [0 : u8; 32], size = 1 : u32} : Desc;") `shouldSatisfy` null

  it "rejects writing a value of another type to a scalar fixed location" $
    runNegativeTestTypeCheck (resource "self->status = true;") `shouldSatisfy` isScalarMismatch
