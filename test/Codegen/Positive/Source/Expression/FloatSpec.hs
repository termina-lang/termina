module Codegen.Positive.Source.Expression.FloatSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

-- | Exercises f32/f64 declarations, decimal and scientific literals, and
-- floating-point arithmetic, checking the generated C types (float32_t /
-- float64_t), the 'f' suffix on f32 literals and the preserved notation.
test0 :: String
test0 = "function test0() {\n" ++
        "    var foo : f32 = 0.0 : f32;\n" ++
        "    var bar : f64 = 1.0e-3 : f64;\n" ++
        "    foo = foo + 3.14 : f32;\n" ++
        "    bar = bar * 2.0 : f64;\n" ++
        "    return;\n" ++
        "}"

-- | Nested floating-point arithmetic needs no per-step casts: same-type float
-- arithmetic does not promote (unlike integer promotion to int), so (a + b) + c
-- is emitted as a + b + c.
test1 :: String
test1 = "function test1() {\n" ++
        "    var a : f32 = 1.0 : f32;\n" ++
        "    var b : f32 = 2.0 : f32;\n" ++
        "    var c : f32 = 3.0 : f32;\n" ++
        "    a = (a + b) + c;\n" ++
        "    return;\n" ++
        "}"

-- | Regression: arrays of floats must declare and initialise correctly.
test_array :: String
test_array = "function test_array() {\n" ++
        "    var arr : [f32; 3] = {1.0 : f32, 2.0 : f32, 3.0 : f32};\n" ++
        "    arr[1] = arr[0] + 1.0 : f32;\n" ++
        "    return;\n" ++
        "}"

-- | Regression: Option<f32> exercises genTypeSpecName for floats, which used
-- to fall through to an internal error.
test_option :: String
test_option = "function test_option() {\n" ++
        "    var o : Option<f32> = None;\n" ++
        "    o = Some(1.5 : f32);\n" ++
        "    return;\n" ++
        "}"

-- | Casts between integer and floating-point types and between f32 and f64.
test_cast :: String
test_cast = "function test_cast() {\n" ++
        "    var i : i32 = 5 : i32;\n" ++
        "    var f : f32 = 0.0 : f32;\n" ++
        "    var d : f64 = 0.0 : f64;\n" ++
        "    f = i as f32;\n" ++
        "    i = f as i32;\n" ++
        "    d = f as f64;\n" ++
        "    f = d as f32;\n" ++
        "    return;\n" ++
        "}"

-- | Relational operators on floats produce a boolean.
test_relational :: String
test_relational = "function test_relational() {\n" ++
        "    var f : f32 = 1.0 : f32;\n" ++
        "    var g : f32 = 2.0 : f32;\n" ++
        "    var b : bool = f < g;\n" ++
        "    b = f >= g;\n" ++
        "    return;\n" ++
        "}"

-- | The Prelude function f32_to_bits lowers to a direct C call of the same name
-- (its body is provided by the OSAL).
test_bits :: String
test_bits = "function test_bits(x : f32) -> u32 {\n" ++
        "    return f32_to_bits(x);\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing floating-point expressions" $ do
    it "Declares a function with floating-point arithmetic" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void test0();\n" ++
              "\n" ++
              "#endif\n")
    it "Generates floating-point arithmetic" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void test0() {\n" ++
              "    \n" ++
              "    float32_t foo = 0.0f;\n" ++
              "\n" ++
              "    float64_t bar = 1.0e-3;\n" ++
              "\n" ++
              "    foo = foo + 3.14f;\n" ++
              "\n" ++
              "    bar = bar * 2.0;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Emits no per-step cast in nested float arithmetic of function test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void test1() {\n" ++
              "    \n" ++
              "    float32_t a = 1.0f;\n" ++
              "\n" ++
              "    float32_t b = 2.0f;\n" ++
              "\n" ++
              "    float32_t c = 3.0f;\n" ++
              "\n" ++
              "    a = a + b + c;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Generates an array of f32 in function test_array" $ do
      renderSource test_array `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void test_array() {\n" ++
              "    \n" ++
              "    float32_t arr[3U] = { 1.0f, 2.0f, 3.0f };\n" ++
              "\n" ++
              "    arr[1U] = arr[0U] + 1.0f;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Generates Option<f32> in function test_option" $ do
      renderSource test_option `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void test_option() {\n" ++
              "    \n" ++
              "    __option_float32_t o = { .__variant = None };\n" ++
              "\n" ++
              "    o.__variant = Some;\n" ++
              "    o.Some.__0 = 1.5f;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Generates int<->float and f32<->f64 casts in function test_cast" $ do
      renderSource test_cast `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void test_cast() {\n" ++
              "    \n" ++
              "    int32_t i = 5L;\n" ++
              "\n" ++
              "    float32_t f = 0.0f;\n" ++
              "\n" ++
              "    float64_t d = 0.0;\n" ++
              "\n" ++
              "    f = (float32_t)i;\n" ++
              "\n" ++
              "    i = (int32_t)f;\n" ++
              "\n" ++
              "    d = (float64_t)f;\n" ++
              "\n" ++
              "    f = (float32_t)d;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Generates float relational operators in function test_relational" $ do
      renderSource test_relational `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void test_relational() {\n" ++
              "    \n" ++
              "    float32_t f = 1.0f;\n" ++
              "\n" ++
              "    float32_t g = 2.0f;\n" ++
              "\n" ++
              "    _Bool b = f < g;\n" ++
              "\n" ++
              "    b = f >= g;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Lowers Prelude f32_to_bits to a direct C call in function test_bits" $ do
      renderSource test_bits `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "uint32_t test_bits(float32_t x) {\n" ++
              "    \n" ++
              "    return f32_to_bits(x);\n" ++
              "\n" ++
              "}\n")
