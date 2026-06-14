module Codegen.Statement.InitializerSpec (spec) where

import Codegen.Common

import Test.Hspec
import Data.Text

-- | Battery covering the two initialization regimes:
--   * Declarations "T x = <init>;" lower to a single (possibly nested) C
--     initializer: arrays as positional lists, structs/variants as designated
--     lists, strings as string literals, fills as for loops.
--   * Assignments to an already-declared variable lower element-wise (a C
--     initializer list is only valid in a declaration).

-- Declarations -------------------------------------------------------------

-- | Arrays: explicit list, fill, string and a nested array.
declArrays :: String
declArrays =
  "function ta() {\n" ++
  "    var a : [u8; 3] = {1 : u8, 2 : u8, 3 : u8};\n" ++
  "    var f : [u8; 3] = [0 : u8; 3];\n" ++
  "    var s : [char; 6] = \"hello\";\n" ++
  "    var m : [[u8; 2]; 2] = { {1:u8, 2:u8}, {3:u8, 4:u8} };\n" ++
  "    return;\n" ++
  "}\n"

-- | Structs: a struct, a struct with an array field, and an array of structs.
declStructs :: String
declStructs =
  "struct Point { x : u32; y : u32; };\n" ++
  "struct Box { v : [u8; 2]; };\n" ++
  "function tb() {\n" ++
  "    var p : Point = {x = 1 : u32, y = 2 : u32} : Point;\n" ++
  "    var b : Box = {v = {1:u8, 2:u8}} : Box;\n" ++
  "    var arr : [Point; 2] = { {x=1:u32, y=2:u32}:Point, {x=3:u32, y=4:u32}:Point };\n" ++
  "    return;\n" ++
  "}\n"

-- | Option (Some/None) and enum variants (with and without parameters).
declVariants :: String
declVariants =
  "enum Color { Red, Pair(u8, u8) };\n" ++
  "function tc() {\n" ++
  "    var o : Option<u32> = Some(5 : u32);\n" ++
  "    var n : Option<u32> = None;\n" ++
  "    var c : Color = Color::Pair(1:u8, 2:u8);\n" ++
  "    var r : Color = Color::Red;\n" ++
  "    return;\n" ++
  "}\n"

-- Assignments to already-declared variables --------------------------------

-- | Arrays: list, fill and string assigned to an existing array (element-wise).
assignArrays :: String
assignArrays =
  "function td() {\n" ++
  "    var a : [u8; 3] = {0:u8, 0:u8, 0:u8};\n" ++
  "    a = {1 : u8, 2 : u8, 3 : u8};\n" ++
  "    a = [9 : u8; 3];\n" ++
  "    var s : [char; 6] = \"aaaaa\";\n" ++
  "    s = \"hello\";\n" ++
  "    return;\n" ++
  "}\n"

-- | Struct field-wise assignment, struct copy (structs are copyable), and
-- Option assignment.
assignAggregates :: String
assignAggregates =
  "struct Point { x : u32; y : u32; };\n" ++
  "function te() {\n" ++
  "    var p : Point = {x = 0:u32, y = 0:u32} : Point;\n" ++
  "    p = {x = 1:u32, y = 2:u32} : Point;\n" ++
  "    var p2 : Point = {x=0:u32, y=0:u32} : Point;\n" ++
  "    p2 = p;\n" ++
  "    var o : Option<u32> = None;\n" ++
  "    o = Some(7 : u32);\n" ++
  "    return;\n" ++
  "}\n"

-- | Status: declaration ({ ... }) and assignment (element-wise), both for the
-- payload-less (Success) and payload-carrying (Failure) variants.
statusInitAssign :: String
statusInitAssign =
  "function ts() {\n" ++
  "    var su : Status<i32> = Success;\n" ++
  "    var fa : Status<i32> = Failure(3 : i32);\n" ++
  "    fa = Success;\n" ++
  "    su = Failure(2 : i32);\n" ++
  "    return;\n" ++
  "}\n"

spec :: Spec
spec = do
  describe "Initialization in declarations (single C initializer)" $ do
    it "Arrays: list, fill, string and nested array" $
      renderSource declArrays `shouldBe`
        pack "\n#include \"test.h\"\n\nvoid ta() {\n    \n    uint8_t a[3U] = { 1U, 2U, 3U };\n\n    uint8_t f[3U] = { 0U, 0U, 0U };\n\n    char s[6U] = { 'h', 'e', 'l', 'l', 'o' };\n\n    uint8_t m[2U][2U] = { { 1U, 2U }, { 3U, 4U } };\n\n    return;\n\n}\n"
    it "Structs: struct, struct with array field, and array of structs" $
      renderSource declStructs `shouldBe`
        pack "\n#include \"test.h\"\n\nvoid tb() {\n    \n    Point p = { .x = 1U, .y = 2U };\n\n    Box b = { .v = { 1U, 2U } };\n\n    Point arr[2U] = { { .x = 1U, .y = 2U }, { .x = 3U, .y = 4U } };\n\n    return;\n\n}\n"
    it "Option (Some/None) and enum variants (with/without parameters)" $
      renderSource declVariants `shouldBe`
        pack "\n#include \"test.h\"\n\nvoid tc() {\n    \n    __option_uint32_t o = { .__variant = Some, .Some = { .__0 = 5U } };\n\n    __option_uint32_t n = { .__variant = None };\n\n    Color c = { .__variant = Color__Pair, .Pair = { .__0 = 1U, .__1 = 2U } };\n\n    Color r = { .__variant = Color__Red };\n\n    return;\n\n}\n"
  describe "Assignment to an already-declared variable (element-wise)" $ do
    it "Arrays: list, fill and string assignment" $
      renderSource assignArrays `shouldBe`
        pack "\n#include \"test.h\"\n\nvoid td() {\n    \n    uint8_t a[3U] = { 0U, 0U, 0U };\n\n    a[0U] = 1U;\n    a[1U] = 2U;\n    a[2U] = 3U;\n\n    for (size_t __i0 = 0U; __i0 < 3U; __i0 = __i0 + 1U) {\n        a[__i0] = 9U;\n    }\n\n    char s[6U] = { 'a', 'a', 'a', 'a', 'a' };\n\n    s[0U] = 'h';\n    s[1U] = 'e';\n    s[2U] = 'l';\n    s[3U] = 'l';\n    s[4U] = 'o';\n    s[5U] = '\\0';\n\n    return;\n\n}\n"
    it "Struct field-wise assignment, struct copy and Option assignment" $
      renderSource assignAggregates `shouldBe`
        pack "\n#include \"test.h\"\n\nvoid te() {\n    \n    Point p = { .x = 0U, .y = 0U };\n\n    p.x = 1U;\n    p.y = 2U;\n\n    Point p2 = { .x = 0U, .y = 0U };\n\n    p2 = p;\n\n    __option_uint32_t o = { .__variant = None };\n\n    o.__variant = Some;\n    o.Some.__0 = 7U;\n\n    return;\n\n}\n"
  describe "Status declaration and assignment" $
    it "Success/Failure declaration ({ ... }) and assignment (element-wise)" $
      renderSource statusInitAssign `shouldBe`
        pack "\n#include \"test.h\"\n\nvoid ts() {\n    \n    __status_int32_t su = { .__variant = Success };\n\n    __status_int32_t fa = { .__variant = Failure, .Failure = { .__0 = 3L } };\n\n    fa.__variant = Success;\n\n    su.__variant = Failure;\n    su.Failure.__0 = 2L;\n\n    return;\n\n}\n"
