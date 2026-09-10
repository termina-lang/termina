-- | End-to-end tests for the mutable-reference aliasing check (SEF-001): two
-- @&mut@ references to the same object within a single expression are rejected,
-- while distinct objects, and the same object across separate statements, are
-- fine. Driven through the whole pipeline, which now runs the side-effect pass.
module SideEffects.AliasingSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Data.Text (pack)
import Test.Hspec

-- Two functions taking mutable references, and a driver whose body is supplied
-- per case (each case declares exactly the objects it uses, so the unused-
-- variable check does not fire first).
prog :: String -> String
prog triggerBody =
    "function set2(a : &mut u32, b : &mut u32) {\n" ++
    "    *a = 0 : u32;\n" ++
    "    *b = 1 : u32;\n" ++
    "    return;\n" ++
    "}\n" ++
    "function set1(a : &mut u32) {\n" ++
    "    *a = 0 : u32;\n" ++
    "    return;\n" ++
    "}\n" ++
    "function trigger() {\n" ++
    triggerBody ++
    "    return;\n" ++
    "}\n"

-- A struct plus functions taking a whole-struct reference and a field
-- reference, to exercise overlapping (prefix) access paths.
progStruct :: String -> String
progStruct triggerBody =
    "struct S {\n" ++
    "    a : u32;\n" ++
    "    b : u32;\n" ++
    "};\n" ++
    "function useS(s : &mut S, v : &mut u32) {\n" ++
    "    (*s).a = *v;\n" ++
    "    return;\n" ++
    "}\n" ++
    "function set2(a : &mut u32, b : &mut u32) {\n" ++
    "    *a = 0 : u32;\n" ++
    "    *b = 1 : u32;\n" ++
    "    return;\n" ++
    "}\n" ++
    "function trigger() {\n" ++
    triggerBody ++
    "    return;\n" ++
    "}\n"

-- A resource with a &mut self method and a procedure that drives it, to
-- exercise the implicit mutable borrow of the receiver.
progMethod :: String -> String
progMethod runBody =
    "interface IR {\n" ++
    "    procedure run(&mut self);\n" ++
    "};\n" ++
    "resource class R provides IR {\n" ++
    "    counter : u32;\n" ++
    "    method update(&mut self, v : &mut u32) {\n" ++
    "        *v = self->counter;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "    procedure run(&mut self) {\n" ++
    runBody ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

spec :: Spec
spec = do
  describe "SEF-001: implicit &mut self receiver aliases a &mut argument" $ do
    it "rejects self->update(&mut self->counter)" $
      compileErrorCode (progMethod
        "        self->update(&mut self->counter);\n")
        `shouldBe` Just (pack "SEF-001")
    it "accepts self->update(&mut x) on an unrelated local" $
      compileErrorCode (progMethod
        "        var x : u32 = 0 : u32;\n\
        \        self->update(&mut x);\n")
        `shouldBe` Nothing

  describe "SEF-001: overlapping mutable references (prefix)" $ do
    it "rejects &mut s and &mut s.a (whole object and its field)" $
      compileErrorCode (progStruct
        "    var s : S = {a = 0 : u32, b = 0 : u32};\n\
        \    useS(&mut s, &mut s.a);\n")
        `shouldBe` Just (pack "SEF-001")
    it "accepts &mut s.a and &mut s.b (distinct fields)" $
      compileErrorCode (progStruct
        "    var s : S = {a = 0 : u32, b = 0 : u32};\n\
        \    set2(&mut s.a, &mut s.b);\n")
        `shouldBe` Nothing

  describe "SEF-001: two mutable references to the same object" $ do
    it "rejects &mut x passed twice to one call" $
      compileErrorCode (prog
        "    var x : u32 = 0 : u32;\n\
        \    set2(&mut x, &mut x);\n")
        `shouldBe` Just (pack "SEF-001")

  describe "SEF-001: well-formed mutable references are accepted" $ do
    it "accepts &mut x and &mut y (distinct objects)" $
      compileErrorCode (prog
        "    var x : u32 = 0 : u32;\n\
        \    var y : u32 = 0 : u32;\n\
        \    set2(&mut x, &mut y);\n")
        `shouldBe` Nothing
    it "accepts &mut x in two separate statements (per-expression scope)" $
      compileErrorCode (prog
        "    var x : u32 = 0 : u32;\n\
        \    set1(&mut x);\n\
        \    set1(&mut x);\n")
        `shouldBe` Nothing
