-- | Option, status and result initializers as the arguments of a call.
module Semantic.Negative.MonadicArgumentSpec (spec) where

import Test.Hspec
import Semantic.Errors
import Semantic.Common

-- | A resource whose procedure passes the given argument to a procedure of a
-- port that takes an option of a box, and a function that takes an option.
resource :: String -> String
resource stmt =
    "function first(o : Option<u32>) -> u32 {\n" ++
    "    var r : u32;\n" ++
    "    match o {\n" ++
    "        case Some(v) => {\n" ++
    "            r = v;\n" ++
    "        }\n" ++
    "        case None => {\n" ++
    "            r = 0;\n" ++
    "        }\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n" ++
    "interface ISink {\n" ++
    "    procedure consume(&mut self, data : Option<box u32>);\n" ++
    "};\n" ++
    "interface IR {\n" ++
    "    procedure get(&mut self, b : box u32, r : &mut u32);\n" ++
    "};\n" ++
    "resource class CR provides IR {\n" ++
    "    snk : access ISink;\n" ++
    "    procedure get(&mut self, b : box u32, r : &mut u32) {\n" ++
    "        " ++ stmt ++ "\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

isParameterMismatch :: Maybe Error -> Bool
isParameterMismatch = \case
  Just (EMonadicVariantParameterTypeMismatch _ _) -> True
  _ -> False

isOptionMismatch :: Maybe Error -> Bool
isOptionMismatch = \case
  Just (EInvalidVariantForOption _) -> True
  _ -> False

spec :: Spec
spec = describe "Semantic: option, status and result initializers as arguments" $ do

  it "accepts an option of a value as the argument of a function" $
    runNegativeTestTypeCheck (resource "*r = first(Some(3));")
      `shouldSatisfy` null

  it "rejects a variant of another monadic type as the argument" $
    runNegativeTestTypeCheck (resource "*r = first(Success);")
      `shouldSatisfy` isOptionMismatch

  it "accepts an option of a box as the argument of a procedure" $
    runNegativeTestTypeCheck (resource "self->snk.consume(Some(b)); *r = 0;")
      `shouldSatisfy` null

  it "rejects an option of a value where the procedure takes an option of a box" $
    runNegativeTestTypeCheck (resource "self->snk.consume(Some(*r)); *r = 0;")
      `shouldSatisfy` isParameterMismatch
