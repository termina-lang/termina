-- | The types an atomic addition accepts, and the self reference it needs.
module Semantic.Negative.AtomicFetchAddSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors
import Semantic.Common
import Semantic.Types (SemanticAnn)

-- | A resource with an atomic and an atomic array of the given type, whose
-- procedure runs the given statement.
resource :: String -> String -> String -> String
resource ty self stmt =
    "interface IR {\n" ++
    "    procedure run(" ++ self ++ ", i : usize, prev : &mut " ++ ty ++ ");\n" ++
    "};\n" ++
    "resource class CR provides IR {\n" ++
    "    value : access AtomicAccess<" ++ ty ++ ">;\n" ++
    "    values : access AtomicArrayAccess<" ++ ty ++ ", 4>;\n" ++
    "    procedure run(" ++ self ++ ", i : usize, prev : &mut " ++ ty ++ ") {\n" ++
    "        " ++ stmt ++ "\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

isInvalidType :: String -> TerminaType SemanticAnn -> Maybe Error -> Bool
isInvalidType procName ty = \case
  Just (EAtomicFetchAddInvalidType p t) -> p == procName && t == ty
  _ -> False

isImmutableCall :: Maybe Error -> Bool
isImmutableCall = \case
  Just EInvalidAccessToProcedureFromImmutableSelfReference -> True
  _ -> False

spec :: Spec
spec = describe "Semantic: atomic additions" $ do

  it "accepts fetch_add on every unsigned integer type" $
    mapM_ (\ty -> runNegativeTestTypeCheck (resource ty "&mut self" "self->value.fetch_add(1, prev);") `shouldSatisfy` null)
      ["u8", "u16", "u32", "usize"]

  it "accepts fetch_add_index on every unsigned integer type" $
    mapM_ (\ty -> runNegativeTestTypeCheck (resource ty "&mut self" "self->values.fetch_add_index(i, 1, prev);") `shouldSatisfy` null)
      ["u8", "u16", "u32", "usize"]

  it "rejects fetch_add on a signed integer" $
    runNegativeTestTypeCheck (resource "i32" "&mut self" "self->value.fetch_add(1, prev);") `shouldSatisfy` isInvalidType "fetch_add" TInt32

  it "rejects fetch_add on a 64-bit unsigned integer" $
    runNegativeTestTypeCheck (resource "u64" "&mut self" "self->value.fetch_add(1, prev);") `shouldSatisfy` isInvalidType "fetch_add" TUInt64

  it "rejects fetch_add_index on a signed integer" $
    runNegativeTestTypeCheck (resource "i16" "&mut self" "self->values.fetch_add_index(i, 1, prev);") `shouldSatisfy` isInvalidType "fetch_add_index" TInt16

  it "rejects fetch_add through an immutable self reference" $
    runNegativeTestTypeCheck (resource "u32" "&self" "self->value.fetch_add(1, prev);") `shouldSatisfy` isImmutableCall

  it "rejects fetch_add_index through an immutable self reference" $
    runNegativeTestTypeCheck (resource "u32" "&self" "self->values.fetch_add_index(i, 1, prev);") `shouldSatisfy` isImmutableCall
