module ConstFolding.Negative.ConstEvalSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

spec :: Spec
spec = do
  describe "ConstFolding: constant-evaluation errors" $ do

    it "CFE-004: constant integer overflow on cast" $ do
      let src = "function f() -> u8 {\n" ++
                "    return 256 : u16 as u8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-004")

    it "CFE-005: constant integer underflow" $ do
      let src = "function f() -> u8 {\n" ++
                "    return 0 : u8 - 1 : u8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-005")

    it "CFE-006: constant division by zero" $ do
      let src = "function f() -> u32 {\n" ++
                "    return 1 : u32 / 0 : u32;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-006")

    it "CFE-008: for loop with zero iterations" $ do
      let src = "function f() {\n" ++
                "    for i : usize in 3 : usize .. 3 : usize {\n" ++
                "    }\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-008")

    it "CFE-009: for loop with negative iterations" $ do
      let src = "function f() {\n" ++
                "    for i : usize in 5 : usize .. 3 : usize {\n" ++
                "    }\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-009")

    it "CFE-013: array index out of bounds with constant index" $ do
      let src = "const bad_idx : usize = 10;\n" ++
                "function f() -> u8 {\n" ++
                "    var a : [u8; 4] = [0; 4];\n" ++
                "    a[bad_idx] = 0 : u8;\n" ++
                "    return a[0];\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-013")

    it "CFE-016: shift amount greater than or equal to the type width" $ do
      let src = "function f() -> u8 {\n" ++
                "    var x : u8 = 0 : u8;\n" ++
                "    x = x << 8 : usize;\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-016")

    it "CFE-017: comparison against a constant at the limit of the type range" $ do
      let src = "function f(x : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x < 0 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-017")
