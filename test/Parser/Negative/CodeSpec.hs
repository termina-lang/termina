{-# LANGUAGE OverloadedStrings #-}
-- | Parser-stage error codes (PE-NNN), asserted on the parser stage in
-- isolation (no type checking). The codes of the module loader, PE-002, PE-003
-- and PE-005 to PE-008, are tested in Configuration.SourcesSpec.
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

pe009 :: String
pe009 = unlines
  [ "function foo() {"
  , "    return;"
  , "    /// Nothing follows."
  , "}" ]

pe010 :: String
pe010 = unlines
  [ "struct S {"
  , "    /// Before."
  , "    x : u32; ///< After."
  , "};" ]

pe011 :: String
pe011 = unlines
  [ "/**"
  , " * @brief Does nothing."
  , " */"
  , "function foo() { return; }" ]

pe012 :: String
pe012 = unlines
  [ "/**"
  , " * @param"
  , " */"
  , "function foo() { return; }" ]

spec :: Spec
spec = describe "Parser stage: error-code coverage" $ do
  it "PE-011: unknown documentation tag" $
    parserStageErrorCode [("test", pe011)] `shouldBe` Just "PE-011"
  it "PE-012: malformed documentation tag" $
    parserStageErrorCode [("test", pe012)] `shouldBe` Just "PE-012"
  it "PE-001: parsing error" $
    parserStageErrorCode [("test", pe001)] `shouldBe` Just "PE-001"
  it "PE-004: cycle between project source files" $
    parserStageErrorCode [("pkg/a", pe004ModA), ("pkg/b", pe004ModB)]
      `shouldBe` Just "PE-004"
  it "PE-009: misplaced documentation comment" $
    parserStageErrorCode [("test", pe009)] `shouldBe` Just "PE-009"
  it "PE-010: element with two documentation comments" $
    parserStageErrorCode [("test", pe010)] `shouldBe` Just "PE-010"
