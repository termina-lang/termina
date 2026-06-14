module ConstFolding.SizeSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

-- These mismatches live in ConstFolding (not the type checker) precisely when
-- the sizes are 'constexpr': the type checker defers the comparison until the
-- constants are evaluated. So every fixture sizes things with constexpr/const
-- identifiers that disagree, forcing the check into the folding pass.
spec :: Spec
spec = do
  describe "ConstFolding: array size and slice errors" $ do

    it "CPE-002: array fill initializer size mismatch" $ do
      let src = "constexpr n : usize = 4;\n" ++
                "constexpr k : usize = 5;\n" ++
                "function f() {\n" ++
                "    var a : [u8; n] = [0 : u8; k];\n" ++
                "    a[0] = 1 : u8;\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-002")

    it "CPE-002: array expression-list initializer size mismatch" $ do
      -- Both fill and expression-list initializer size mismatches report the
      -- same code (CPE-002, EArrayInitializerSizeMismatch).
      let src = "constexpr n : usize = 4;\n" ++
                "function f() {\n" ++
                "    var a : [u8; n] = {1 : u8, 2 : u8, 3 : u8};\n" ++
                "    a[0] = 0 : u8;\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-002")

    it "CPE-003: string initializer larger than the array" $ do
      let src = "constexpr n : usize = 4;\n" ++
                "function use_chars(_c : &[char; n]) {\n" ++
                "    return;\n" ++
                "}\n" ++
                "function f() {\n" ++
                "    let s : [char; n] = \"hello\";\n" ++
                "    use_chars(&s);\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-003")

    it "CPE-010: array slice out of bounds" $ do
      let src = "function take2(_data : &[u8; 2]) {\n" ++
                "    return;\n" ++
                "}\n" ++
                "const lo : usize = 0;\n" ++
                "const hi : usize = 5;\n" ++
                "function f() {\n" ++
                "    var a : [u8; 4] = [0 : u8; 4];\n" ++
                "    take2(&a[lo .. hi]);\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-010")

    it "CPE-011: array slice with lower bound above upper bound" $ do
      let src = "function take2(_data : &[u8; 2]) {\n" ++
                "    return;\n" ++
                "}\n" ++
                "const lo : usize = 3;\n" ++
                "const hi : usize = 1;\n" ++
                "function f() {\n" ++
                "    var a : [u8; 4] = [0 : u8; 4];\n" ++
                "    take2(&a[lo .. hi]);\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-011")

    it "CPE-012: array slice length does not match the expected size" $ do
      let src = "function take2(_data : &[u8; 2]) {\n" ++
                "    return;\n" ++
                "}\n" ++
                "const lo : usize = 0;\n" ++
                "const hi : usize = 3;\n" ++
                "function f() {\n" ++
                "    var a : [u8; 4] = [0 : u8; 4];\n" ++
                "    take2(&a[lo .. hi]);\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-012")

    it "CPE-015: referenced array size mismatch" $ do
      let src = "constexpr n : usize = 4;\n" ++
                "constexpr m : usize = 5;\n" ++
                "function take(_data : &[u8; m]) {\n" ++
                "    return;\n" ++
                "}\n" ++
                "function f() {\n" ++
                "    var a : [u8; n] = [0 : u8; n];\n" ++
                "    take(&a);\n" ++
                "    return;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-015")
