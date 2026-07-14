-- | Evaluation-order tests (SEF-002, Rule 13.2 core): within one group of
-- sibling subexpressions (a call's arguments, a binary operator's operands) at
-- most one may carry a persistent side effect. A single effect, or effects on
-- separate statements, are fine.
module SideEffects.OrderingSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Data.Text (pack)
import Test.Hspec

-- @bump@ is an effectful, value-returning function (it mutates through a &mut
-- parameter); @add@ is pure. The driver body is supplied per case.
prog :: String -> String
prog body =
    "function bump(a : &mut u32) -> u32 {\n" ++
    "    *a = 1 : u32;\n" ++
    "    return 0 : u32;\n" ++
    "}\n" ++
    "function add(x : u32, y : u32) -> u32 {\n" ++
    "    return x + y;\n" ++
    "}\n" ++
    "function trigger() -> u32 {\n" ++
    body ++
    "}\n"

-- | Like 'prog' but the driver returns @bool@ (for && / || cases).
progBool :: String -> String
progBool body =
    "function bump(a : &mut u32) -> u32 {\n" ++
    "    *a = 1 : u32;\n" ++
    "    return 0 : u32;\n" ++
    "}\n" ++
    "function trigger() -> bool {\n" ++
    body ++
    "}\n"

spec :: Spec
spec = do
  describe "SEF-002: at most one side effect per sibling group" $ do
    it "rejects two effectful arguments to one call" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    var b : u32 = 0 : u32;\n\
        \    return add(bump(&mut a), bump(&mut b));\n")
        `shouldBe` Just (pack "SEF-002")
    it "rejects two effectful operands of a binary operator" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    var b : u32 = 0 : u32;\n\
        \    return bump(&mut a) + bump(&mut b);\n")
        `shouldBe` Just (pack "SEF-002")

  describe "SEF-002: a single side effect is fine" $ do
    it "accepts one effectful argument (the other pure)" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    return add(bump(&mut a), 5 : u32);\n")
        `shouldBe` Nothing
    it "accepts one effectful operand of a binary operator" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    return bump(&mut a) + 5 : u32;\n")
        `shouldBe` Nothing

  describe "SEF-003: a side effect and an access to the same object interfere" $ do
    it "rejects mutating and reading the same object in one call" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    return add(bump(&mut a), a);\n")
        `shouldBe` Just (pack "SEF-003")
    it "rejects mutating and reading the same object across binary operands" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    return bump(&mut a) + a;\n")
        `shouldBe` Just (pack "SEF-003")
    it "accepts mutating and reading different objects" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    var b : u32 = 0 : u32;\n\
        \    return add(bump(&mut a), b);\n")
        `shouldBe` Nothing
    it "detects the mutation of a &mut self method against a read of self" $
      compileErrorCode
        "function add(x : u32, y : u32) -> u32 { return x + y; }\n\
        \interface IR { procedure run(&mut self); };\n\
        \resource class R provides IR {\n\
        \    counter : u32;\n\
        \    method bump(&mut self) -> u32 {\n\
        \        self->counter = self->counter + 1 : u32;\n\
        \        return self->counter;\n\
        \    }\n\
        \    procedure run(&mut self) {\n\
        \        self->counter = add(self->bump(), self->counter);\n\
        \        return;\n\
        \    }\n\
        \};\n"
        `shouldBe` Just (pack "SEF-003")
    it "detects a side effect inside an array index" $
      compileErrorCode
        "function idx(a : &mut u32) -> usize { *a = 1 : u32; return 0 : usize; }\n\
        \function add(x : u32, y : u32) -> u32 { return x + y; }\n\
        \function trigger() -> u32 {\n\
        \    var x : u32 = 0 : u32;\n\
        \    var arr : [u32; 2] = {0 : u32, 0 : u32};\n\
        \    return add(arr[idx(&mut x)], x);\n\
        \}\n"
        `shouldBe` Just (pack "SEF-003")

  describe "SEF-004: no side effect in an initializer list" $ do
    it "rejects an effectful element in an array initializer" $
      compileErrorCode (prog
        "    var a : u32 = 0 : u32;\n\
        \    var arr : [u32; 2] = {bump(&mut a), 1 : u32};\n\
        \    return arr[0 : usize];\n")
        `shouldBe` Just (pack "SEF-004")

  describe "SEF-005/006: no side effect in the right operand of && / ||" $ do
    it "rejects an effect in the right operand of &&" $
      compileErrorCode (progBool
        "    var a : u32 = 0 : u32;\n\
        \    var flag : bool = true;\n\
        \    return flag && (bump(&mut a) == 0 : u32);\n")
        `shouldBe` Just (pack "SEF-005")
    it "rejects an effect in the right operand of ||" $
      compileErrorCode (progBool
        "    var a : u32 = 0 : u32;\n\
        \    var flag : bool = true;\n\
        \    return flag || (bump(&mut a) == 0 : u32);\n")
        `shouldBe` Just (pack "SEF-006")
    it "accepts an effect in the left operand of &&" $
      compileErrorCode (progBool
        "    var a : u32 = 0 : u32;\n\
        \    var flag : bool = true;\n\
        \    return (bump(&mut a) == 0 : u32) && flag;\n")
        `shouldBe` Nothing
