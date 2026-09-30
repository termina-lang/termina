module Semantic.Positive.ConstAggregateSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

-- A global const struct or enumeration keeps its bare type and is bound with
-- Immutable access, like a const array: its fields can be read and matched on,
-- and writing to a field is rejected as a write to a constant. A constant
-- cannot hold a box, which the option, status and result types rule out.

point :: String
point =
  "struct Point {\n" ++
  "    x : u32;\n" ++
  "    y : u32;\n" ++
  "};\n" ++
  "const P : Point = {x = 1 : u32, y = 2 : u32};\n"

mode :: String
mode =
  "enum Mode {\n" ++
  "    Off,\n" ++
  "    On(u8)\n" ++
  "};\n" ++
  "const M : Mode = Mode::On(3 : u8);\n"

-- | Scalars read from constants with a constant path, used where a constant
-- expression is required.
constantPaths :: String
constantPaths =
  "struct Config {\n" ++
  "    len : usize;\n" ++
  "    sizes : [usize; 2];\n" ++
  "};\n" ++
  "const CFG : Config = {len = 4 : usize, sizes = {2 : usize, 3 : usize}};\n" ++
  "const L : usize = CFG.len;\n" ++
  "const S : usize = CFG.sizes[1];\n" ++
  "const ON : bool = M is Mode::On;\n" ++
  "function sum() -> usize {\n" ++
  "    var buffer : [u8; CFG.len] = [0 : u8; CFG.len];\n" ++
  "    var total : usize = S;\n" ++
  "    for i : usize in 0 : usize .. CFG.sizes[0] {\n" ++
  "        buffer[i] = 1 : u8;\n" ++
  "        total = total + i;\n" ++
  "    }\n" ++
  "    return total + L + buffer[0] as usize;\n" ++
  "}"

spec :: Spec
spec = do
  describe "Global const structs and enumerations" $ do

    it "Takes a field or an element of a constant as a constant expression" $
      compileErrorCode (mode ++ constantPaths) `shouldBe` Nothing

    it "Slices, references, tests and matches on constants in a function body" $
      compileErrorCode (point ++ mode ++
        "const A : [u32; 4] = {5 : u32, 6 : u32, 7 : u32, 8 : u32};\n" ++
        "const O : Option<u32> = Some(4 : u32);\n" ++
        "function sum2(v : &[u32; 2]) -> u32 { return (*v)[0] + (*v)[1]; }\n" ++
        "function px(p : &Point) -> u32 { return p->x; }\n" ++
        "function flags(b : bool, c : bool) -> u32 {\n" ++
        "    var r : u32 = 0 : u32;\n" ++
        "    if b { r = 1 : u32; }\n" ++
        "    if c { r = r + 2 : u32; }\n" ++
        "    return r;\n" ++
        "}\n" ++
        "function g() -> u32 {\n" ++
        "    var r : u32 = sum2(&A[1 : usize .. 3 : usize]) + px(&P) + flags(M is Mode::Off, O is Some);\n" ++
        "    match O {\n" ++
        "        case Some(v) => { r = r + v; }\n" ++
        "        case None => { r = 1 : u32; }\n" ++
        "    }\n" ++
        "    return r;\n" ++
        "}")
        `shouldBe` Nothing

    it "Takes the cast of a constant as a constant expression" $
      compileErrorCode (
        "const W : u8 = 3 : u8;\n" ++
        "function g() -> usize {\n" ++
        "    var buffer : [u8; W as usize] = [0 : u8; W as usize];\n" ++
        "    var r : usize = buffer[0] as usize;\n" ++
        "    for j : usize in 0 : usize .. W as usize {\n" ++
        "        r = r + j;\n" ++
        "    }\n" ++
        "    return r;\n" ++
        "}")
        `shouldBe` Nothing

    it "Rejects an element of a constant array read with a variable index as a loop bound" $
      compileErrorCode (
        "const A : [usize; 4] = {5 : usize, 6 : usize, 7 : usize, 8 : usize};\n" ++
        "function g(i : usize) -> usize {\n" ++
        "    var r : usize = 0 : usize;\n" ++
        "    for j : usize in 0 : usize .. A[i] {\n" ++
        "        r = r + j;\n" ++
        "    }\n" ++
        "    return r;\n" ++
        "}")
        `shouldBe` Just (pack "SE-003")

    it "Reads an element of a constant array with a variable index" $
      compileErrorCode (point ++
        "const PS : [Point; 2] = [P; 2];\n" ++
        "function read_element(i : usize) -> u32 {\n" ++
        "    var r : u32 = 0 : u32;\n" ++
        "    if i < 2 : usize {\n" ++
        "        r = PS[i].x;\n" ++
        "    }\n" ++
        "    return r;\n" ++
        "}")
        `shouldBe` Nothing

    it "Reads a field of a const struct" $
      compileErrorCode (point ++
        "function read_field() -> u32 {\n" ++
        "    return P.y;\n" ++
        "}")
        `shouldBe` Nothing

    it "Matches on a const enumeration" $
      compileErrorCode (mode ++
        "function read_mode() -> u8 {\n" ++
        "    var r : u8 = 0 : u8;\n" ++
        "    match M {\n" ++
        "        case On(n) => {\n" ++
        "            r = n;\n" ++
        "        }\n" ++
        "        case Off => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return r;\n" ++
        "}")
        `shouldBe` Nothing

    it "Rejects writing to a field of a const struct" $
      compileErrorCode (point ++
        "function write_field() {\n" ++
        "    P.x = 9 : u32;\n" ++
        "    return;\n" ++
        "}")
        `shouldBe` Just (pack "SE-080")

    it "Rejects a const option of a box" $
      compileErrorCode "const O : Option<box u32> = None;\n"
        `shouldBe` Just (pack "SE-181")

    it "Rejects a const status of a box" $
      compileErrorCode "const S : Status<box u32> = Success;\n"
        `shouldBe` Just (pack "SE-184")

    it "Rejects a const result of a box" $
      compileErrorCode "const R : Result<box u32; i32> = Ok(1 : u32);\n"
        `shouldBe` Just (pack "SE-183")
