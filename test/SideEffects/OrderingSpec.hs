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

-- | A resource with a field declared @loc@, which lives at a fixed address
-- the program does not own and which the generated code reaches through a
-- pointer to volatile.
progLocation :: String -> String
progLocation body =
    "struct Registers {\n" ++
    "    status : u32;\n" ++
    "};\n" ++
    "\n" ++
    "interface DriverInterface {\n" ++
    "    procedure check(&mut self, flag : bool, ret : &mut bool);\n" ++
    "};\n" ++
    "\n" ++
    "resource class Driver provides DriverInterface {\n" ++
    "    registers : loc Registers;\n" ++
    "\n" ++
    "    procedure check(&mut self, flag : bool, ret : &mut bool) {\n" ++
    body ++
    "        return;\n" ++
    "    }\n" ++
    "\n" ++
    "};\n"

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

  -- | An array access with an index the compiler does not know lowers to the
  -- bounds check, which raises an exception when the index falls outside the
  -- array, so it is an effect although Termina writes it as a read.
  describe "SEF-005/006: the bounds check of an array access is an effect" $ do
    it "rejects an array access with a variable index in the right operand of &&" $
      compileErrorCode (progBool
        "    var arr : [u32; 4] = {0 : u32, 0 : u32, 0 : u32, 0 : u32};\n\
        \    var i : usize = 0 : usize;\n\
        \    var flag : bool = true;\n\
        \    return flag && (arr[i] == 0 : u32);\n")
        `shouldBe` Just (pack "SEF-005")
    it "rejects an array access with a variable index in the right operand of ||" $
      compileErrorCode (progBool
        "    var arr : [u32; 4] = {0 : u32, 0 : u32, 0 : u32, 0 : u32};\n\
        \    var i : usize = 0 : usize;\n\
        \    var flag : bool = true;\n\
        \    return flag || (arr[i] == 0 : u32);\n")
        `shouldBe` Just (pack "SEF-006")
    it "accepts an array access with a constant index, which is checked at compile time" $
      compileErrorCode (progBool
        "    var arr : [u32; 4] = {0 : u32, 0 : u32, 0 : u32, 0 : u32};\n\
        \    var flag : bool = true;\n\
        \    return flag && (arr[0 : usize] == 0 : u32);\n")
        `shouldBe` Nothing
    it "accepts an array access with a variable index in the left operand of &&" $
      compileErrorCode (progBool
        "    var arr : [u32; 4] = {0 : u32, 0 : u32, 0 : u32, 0 : u32};\n\
        \    var i : usize = 0 : usize;\n\
        \    var flag : bool = true;\n\
        \    return (arr[i] == 0 : u32) && flag;\n")
        `shouldBe` Nothing

  -- | A field declared @loc@ is reached through a pointer to volatile, so the
  -- access is kept where it is written and two reads of it may give different
  -- values.
  describe "SEF-005/006: reading a loc field is an effect" $ do
    it "rejects a read of a loc field in the right operand of ||" $
      compileErrorCode (progLocation
        "        *ret = flag || (self->registers.status == 0 : u32);\n")
        `shouldBe` Just (pack "SEF-006")
    it "rejects a read of a loc field in the right operand of &&" $
      compileErrorCode (progLocation
        "        *ret = flag && (self->registers.status == 0 : u32);\n")
        `shouldBe` Just (pack "SEF-005")
    it "accepts a read of a loc field in the left operand of ||" $
      compileErrorCode (progLocation
        "        *ret = (self->registers.status == 0 : u32) || flag;\n")
        `shouldBe` Nothing

  -- | A method that takes @&mut self@ writes the state of its class without
  -- passing it as an argument, so the mutation is in the receiver and not in
  -- the arguments of the call.
  describe "SEF-005/006: a call to a mutable-self method is an effect" $ do
    it "rejects a mutable-self method in the right operand of &&" $
      compileErrorCode
        "interface IR { procedure run(&mut self, flag : bool, ret : &mut bool); };\n\
        \resource class R provides IR {\n\
        \    counter : u32;\n\
        \    method bump(&mut self) -> u32 {\n\
        \        self->counter = self->counter + 1 : u32;\n\
        \        return self->counter;\n\
        \    }\n\
        \    procedure run(&mut self, flag : bool, ret : &mut bool) {\n\
        \        *ret = flag && (self->bump() == 0 : u32);\n\
        \        return;\n\
        \    }\n\
        \};\n"
        `shouldBe` Just (pack "SEF-005")
    it "accepts a method that takes self immutably and carries nothing" $
      compileErrorCode
        "interface IR { procedure run(&mut self, flag : bool, ret : &mut bool); };\n\
        \resource class R provides IR {\n\
        \    counter : u32;\n\
        \    method peek(&self) -> u32 {\n\
        \        return self->counter;\n\
        \    }\n\
        \    procedure run(&mut self, flag : bool, ret : &mut bool) {\n\
        \        *ret = flag && (self->peek() == 0 : u32);\n\
        \        return;\n\
        \    }\n\
        \};\n"
        `shouldBe` Nothing

  -- | A call carries what its callee carries: the effect is in the body of the
  -- member or the function, and the expression that calls it is where it
  -- happens or does not.
  describe "SEF-005/006: a call carries the effect of what it reaches" $ do
    it "rejects a method that reads a loc field of its class" $
      compileErrorCode
        "struct Registers {\n\
        \    status : u32;\n\
        \};\n\
        \interface DriverInterface {\n\
        \    procedure check(&mut self, flag : bool, ret : &mut bool);\n\
        \};\n\
        \resource class Driver provides DriverInterface {\n\
        \    registers : loc Registers;\n\
        \    method is_ready(&self) -> bool {\n\
        \        return self->registers.status == 0 : u32;\n\
        \    }\n\
        \    procedure check(&mut self, flag : bool, ret : &mut bool) {\n\
        \        *ret = flag || self->is_ready();\n\
        \        return;\n\
        \    }\n\
        \};\n"
        `shouldBe` Just (pack "SEF-006")
    it "rejects a function whose body indexes an array with a variable index" $
      compileErrorCode
        "function pick(i : usize) -> u32 {\n\
        \    var arr : [u32; 4] = {0 : u32, 0 : u32, 0 : u32, 0 : u32};\n\
        \    return arr[i];\n\
        \}\n\
        \function trigger(flag : bool, i : usize) -> bool {\n\
        \    return flag && (pick(i) == 0 : u32);\n\
        \}\n"
        `shouldBe` Just (pack "SEF-005")
    it "accepts a function whose body carries nothing" $
      compileErrorCode
        "function pick(i : usize) -> u32 {\n\
        \    return i as u32;\n\
        \}\n\
        \function trigger(flag : bool, i : usize) -> bool {\n\
        \    return flag && (pick(i) == 0 : u32);\n\
        \}\n"
        `shouldBe` Nothing

  -- | The generated loop joins the range of the iterator and the guard with an
  -- &&, so the guard sits where the right operand sits and cannot carry an
  -- effect either, although the source writes no && at all.
  describe "SEF-007: no side effect in the guard of a loop" $ do
    it "rejects an array access with a variable index in the guard" $
      compileErrorCode
        "function trigger(n : usize) -> u32 {\n\
        \    var arr : [u32; 4] = {0 : u32, 0 : u32, 0 : u32, 0 : u32};\n\
        \    var total : u32 = 0 : u32;\n\
        \    for i : usize in 0 .. 4 while (arr[n] == 0 : u32) {\n\
        \        total = total + (i as u32);\n\
        \    }\n\
        \    return total;\n\
        \}\n"
        `shouldBe` Just (pack "SEF-007")
    it "accepts a guard that only reads variables" $
      compileErrorCode
        "function trigger() -> u32 {\n\
        \    var total : u32 = 0 : u32;\n\
        \    for i : usize in 0 .. 4 while (total == 0 : u32) {\n\
        \        total = total + (i as u32);\n\
        \    }\n\
        \    return total;\n\
        \}\n"
        `shouldBe` Nothing
