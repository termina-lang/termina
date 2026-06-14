module ConstFolding.ConstEvalSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

spec :: Spec
spec = do
  describe "ConstFolding: constant-evaluation errors" $ do

    it "CPE-005: constant integer overflow on cast" $ do
      let src = "function f() -> u8 {\n" ++
                "    return 256 : u16 as u8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-005")

    it "CPE-006: constant integer underflow" $ do
      let src = "function f() -> u8 {\n" ++
                "    return 0 : u8 - 1 : u8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-006")

    it "CPE-007: constant division by zero" $ do
      let src = "function f() -> u32 {\n" ++
                "    return 1 : u32 / 0 : u32;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-007")

    it "CPE-008: condition folds to a constant" $ do
      let src = "function f() {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (1 : u32 == 1 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-008")

    it "CPE-009: for loop with zero iterations" $ do
      let src = "function f() {\n" ++
                "    for i : usize in 3 : usize .. 3 : usize {\n" ++
                "    }\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-009")

    it "CPE-010: for loop with negative iterations" $ do
      let src = "function f() {\n" ++
                "    for i : usize in 5 : usize .. 3 : usize {\n" ++
                "    }\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-010")

    it "CPE-014: array index out of bounds with constant index" $ do
      let src = "const bad_idx : usize = 10;\n" ++
                "function f() {\n" ++
                "    var a : [u8; 4] = [0; 4];\n" ++
                "    a[bad_idx] = 0 : u8;\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-014")
