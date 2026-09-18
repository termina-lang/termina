-- | Scope check, negative tests: a local whose uses all fall inside one inner
-- block is to be declared in that block (VSE-001). The shapes include the ones
-- cppcheck reports on the generated C and one it does not, a reference taken
-- inside the block.
module VarScope.Negative.CodeSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

kindEnum :: String
kindEnum = "enum Kind { A, B };\n"

-- | A task that loads from an atomic array, which is the only place a load
-- into a local can be written.
atomicProgram :: String -> String
atomicProgram body =
    "resource arr : AtomicArray<u32; 4> = { values = [0 : u32; 4] };\n" ++
    "task class T {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    arr_port : access AtomicArrayAccess<u32; 4>;\n" ++
    "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
    body ++
    "        let ret : Status<i32> = Success;\n" ++
    "        return ret;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : T = { timer_port <- timer, arr_port <-> arr };\n"

spec :: Spec
spec = do
  describe "VarScope: declarations that can move to an inner block" $ do

    it "VSE-001: used only in the branch of an if" $ do
      let src = "function f(c : bool) -> u32 {\n" ++
                "    var x : u32 = 1 : u32;\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    if (c) {\n" ++
                "        r = x;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

    it "VSE-001: used only in one case of a match" $ do
      let src = kindEnum ++
                "function f(k : Kind) -> u32 {\n" ++
                "    var x : u32 = 1 : u32;\n" ++
                "    var r : u32;\n" ++
                "    match k {\n" ++
                "        case A => { r = x; }\n" ++
                "        case B => { r = 2 : u32; }\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

    it "VSE-001: used in several cases that fall inside one branch" $ do
      let src = kindEnum ++
                "function f(c : bool, k : Kind) -> u32 {\n" ++
                "    var x : u32;\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    if (c) {\n" ++
                "        match k {\n" ++
                "            case A => { x = 1 : u32; r = x; }\n" ++
                "            case B => { x = 2 : u32; r = x; }\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

    it "VSE-001: initializer that reads a parameter passed by value" $ do
      let src = "function f(n : u32, c : bool) -> u32 {\n" ++
                "    var x : u32 = n + 1 : u32;\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    if (c) {\n" ++
                "        r = x;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

    it "VSE-001: passed by reference inside the branch, which cppcheck does not report" $ do
      let src = "function g(v : &mut u32) {\n" ++
                "    *v = 1 : u32;\n" ++
                "    return;\n" ++
                "}\n" ++
                "function f(c : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var r : u32 = 0 : u32;\n" ++
                "    if (c) {\n" ++
                "        g(&mut x);\n" ++
                "        r = x;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

    it "VSE-001: into a loop whose every turn assigns it first" $ do
      let src = "function f(a : &[u32; 4]) -> u32 {\n" ++
                "    var x : u32;\n" ++
                "    var s : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        x = a[i];\n" ++
                "        s = s + x;\n" ++
                "    }\n" ++
                "    return s;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

    it "VSE-001: into a loop whose every turn loads it from an atomic first" $ do
      let src = atomicProgram $
                "        var v : u32 = 0 : u32;\n" ++
                "        var s : u32 = 0 : u32;\n" ++
                "        for i : usize in 0 : usize .. 4 : usize {\n" ++
                "            if (i > 1 : usize) {\n" ++
                "                self->arr_port.load_index(i, &mut v);\n" ++
                "                s = s + v;\n" ++
                "            }\n" ++
                "        }\n" ++
                "        self->arr_port.store_index(0 : usize, s);\n"
      compileErrorCode src `shouldBe` Just (pack "VSE-001")

