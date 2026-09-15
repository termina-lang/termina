-- | Constant propagation negative tests: a condition whose value is the same
-- every time it is evaluated (CPE-001), whichever of the four sources the
-- value comes from.
module ConstPropagation.Negative.CodeSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

spec :: Spec
spec = do
  describe "ConstPropagation: invariant control expressions" $ do

    it "CPE-001: condition built from literals alone" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (1 : u32 == 1 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: condition that reads a constant of the module" $ do
      let src = "const enabled : bool = true;\n" ++
                "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (enabled) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: condition that reads a local whose value is known" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var flag : bool = true;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: comparison of a local against the value it holds" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (x == 0 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: condition the branch it is nested in has already decided" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        if (flag) {\n" ++
                "            x = 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: comparison refined by the branch it is nested in" $ do
      let src = "function f(n : u32) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (n == 3 : u32) {\n" ++
                "        if (n == 3 : u32) {\n" ++
                "            x = 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: break condition of a loop that nothing in it changes" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var go : bool = true;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize while (go) {\n" ++
                "        x = x + 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-001: condition of an else-if that reads a constant" $ do
      let src = "const enabled : bool = false;\n" ++
                "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    } else if (enabled) {\n" ++
                "        x = 2 : u32;\n" ++
                "    } else {\n" ++
                "        x = x + 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")
