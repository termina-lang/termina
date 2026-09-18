-- | Scope check, positive tests: declarations that stay in the block that
-- declares them, each one for the reason its title gives.
module VarScope.Positive.StaySpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec

spec :: Spec
spec = do
  describe "VarScope: declarations that stay where they are" $ do

    it "used inside a branch and after it" $ do
      let src = "function f(c : bool) -> u32 {\n" ++
                "    var x : u32 = 1 : u32;\n" ++
                "    if (c) {\n" ++
                "        x = 2 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "used in two sibling branches" $ do
      let src = "function f(c : bool) -> u32 {\n" ++
                "    var x : u32 = 1 : u32;\n" ++
                "    var r : u32;\n" ++
                "    if (c) {\n" ++
                "        r = x;\n" ++
                "    } else {\n" ++
                "        r = x + 1 : u32;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "used in the condition that guards the branch" $ do
      let src = "function f(n : u32) -> u32 {\n" ++
                "    var x : u32 = n;\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    if (x > 0 : u32) {\n" ++
                "        r = x;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "initializer that calls a function" $ do
      let src = "function h(n : u32) -> u32 {\n" ++
                "    return n + 1 : u32;\n" ++
                "}\n" ++
                "function f(n : u32, c : bool) -> u32 {\n" ++
                "    var x : u32 = h(n);\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    if (c) {\n" ++
                "        r = x;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "initializer that reads a variable that may be written" $ do
      let src = "function f(n : u32, c : bool) -> u32 {\n" ++
                "    var y : u32 = n;\n" ++
                "    var x : u32 = y;\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    y = y + 1 : u32;\n" ++
                "    if (c) {\n" ++
                "        r = x;\n" ++
                "    }\n" ++
                "    return r + y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "value carried from one turn of a loop to the next" $ do
      let src = "function f(a : &[u32; 4]) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var s : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        s = s + x;\n" ++
                "        x = a[i];\n" ++
                "    }\n" ++
                "    return s;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "loop whose turn reads it through a reference before assigning it" $ do
      let src = "function g(v : &mut u32) {\n" ++
                "    *v = *v + 1 : u32;\n" ++
                "    return;\n" ++
                "}\n" ++
                "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var s : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        g(&mut x);\n" ++
                "        s = s + x;\n" ++
                "    }\n" ++
                "    return s;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "used in the break condition of a loop" $ do
      let src = "function f(a : &[u32; 4]) -> u32 {\n" ++
                "    var go : bool = true;\n" ++
                "    var s : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize while go {\n" ++
                "        s = s + a[i];\n" ++
                "        go = s < 10 : u32;\n" ++
                "    }\n" ++
                "    return s;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing
