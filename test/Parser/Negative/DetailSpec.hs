{-# LANGUAGE LambdaCase #-}
-- | Parser-stage error *detail*: asserts the error constructor and payload, not
-- just the PE-NNN code. Complements 'Parser.Negative.CodeSpec'.
module Parser.Negative.DetailSpec (spec) where

import Parser.Common (parserStageError, moduleStageError)
import Parser.Errors (Error(..))
import Utils.Annotations (AnnotatedError(..))

import Test.Hspec

modA, modB :: String
modA = unlines ["import pkg.b;", "", "function fa() -> u32 { return 0 : u32; }"]
modB = unlines ["import pkg.a;", "", "function fb() -> u32 { return 0 : u32; }"]

spec :: Spec
spec = describe "Parser stage: error detail" $ do
  it "PE-001 wraps the underlying parse error" $
    moduleStageError "function f() -> { return; }" `shouldSatisfy` \case
      Just (AnnotatedError (EParseError _) _) -> True
      _ -> False
  it "PE-004 carries the detected import cycle" $
    parserStageError [("pkg/a", modA), ("pkg/b", modB)] `shouldSatisfy` \case
      Just (AnnotatedError (EImportedFilesLoop deps) _) -> not (null deps)
      _ -> False
