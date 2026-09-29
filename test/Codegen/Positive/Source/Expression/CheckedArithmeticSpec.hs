module Codegen.Positive.Source.Expression.CheckedArithmeticSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

-- | A signed operation goes to the function of the OSAL that checks it, and the
-- divisor of an unsigned division that is not a constant is checked not to be
-- zero. A constant divisor needs no check, except -1 in a signed division.
test0 :: String
test0 = "function div_i32(a : i32, b : i32) -> i32 {\n" ++
        "    return a / b;\n" ++
        "}\n" ++
        "\n" ++
        "function mod_i32(a : i32, b : i32) -> i32 {\n" ++
        "    return a % b;\n" ++
        "}\n" ++
        "\n" ++
        "function div_i32_const(a : i32) -> i32 {\n" ++
        "    return a / 4;\n" ++
        "}\n" ++
        "\n" ++
        "function div_i32_minus_one(a : i32) -> i32 {\n" ++
        "    return a / -1;\n" ++
        "}\n" ++
        "\n" ++
        "function sub_i64(a : i64, b : i64) -> i64 {\n" ++
        "    return a - b;\n" ++
        "}\n" ++
        "\n" ++
        "function div_i8(a : i8, b : i8) -> i8 {\n" ++
        "    return a / b;\n" ++
        "}\n" ++
        "\n" ++
        "function div_u32_const(a : u32) -> u32 {\n" ++
        "    return a / 4;\n" ++
        "}\n"

spec :: Spec
spec = do
  describe "Pretty printing checked arithmetic" $ do
    it "Checks the signed operations and the divisors that are not constant" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "int32_t div_i32(const int32_t a, const int32_t b) {\n" ++
              "    \n" ++
              "    return termina__check__div_i32(a, b);\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "int32_t mod_i32(const int32_t a, const int32_t b) {\n" ++
              "    \n" ++
              "    return termina__check__mod_i32(a, b);\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "int32_t div_i32_const(const int32_t a) {\n" ++
              "    \n" ++
              "    return a / 4L;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "int32_t div_i32_minus_one(const int32_t a) {\n" ++
              "    \n" ++
              "    return termina__check__div_i32(a, -(1L));\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "int64_t sub_i64(const int64_t a, const int64_t b) {\n" ++
              "    \n" ++
              "    return termina__check__sub_i64(a, b);\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "int8_t div_i8(const int8_t a, const int8_t b) {\n" ++
              "    \n" ++
              "    return termina__check__div_i8(a, b);\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "uint32_t div_u32_const(const uint32_t a) {\n" ++
              "    \n" ++
              "    return a / 4U;\n" ++
              "\n" ++
              "}\n")
