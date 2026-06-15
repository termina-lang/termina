{-# LANGUAGE OverloadedStrings #-}
-- | Parser-stage error codes (PE-NNN), asserted on the parser stage in
-- isolation (no type checking). PE-002 (module at the project root) and PE-003
-- (imported file not found) come from the on-disk module loader and are not
-- reachable from in-memory sources, so they are left uncovered on purpose.
module Parser.Negative.CodeSpec (spec) where

import Parser.Common (parserStageErrorCode)

import Test.Hspec

pe001 :: String
pe001 = unlines
  [ "function foo(param0 : u32) -> {"
  , "    return param0;"
  , "}" ]

pe004ModA, pe004ModB :: String
pe004ModA = unlines ["import pkg.b;", "", "function fa() -> u32 { return 0 : u32; }"]
pe004ModB = unlines ["import pkg.a;", "", "function fb() -> u32 { return 0 : u32; }"]

spec :: Spec
spec = describe "Parser stage: error-code coverage" $ do
  it "PE-001: parsing error" $
    parserStageErrorCode [("test", pe001)] `shouldBe` Just "PE-001"
  it "PE-004: cycle between project source files" $
    parserStageErrorCode [("pkg/a", pe004ModA), ("pkg/b", pe004ModB)]
      `shouldBe` Just "PE-004"
