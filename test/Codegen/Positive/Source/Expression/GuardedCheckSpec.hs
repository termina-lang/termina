module Codegen.Positive.Source.Expression.GuardedCheckSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

-- | An index that the left operand of @&&@ keeps inside the array is emitted
-- without the bounds check, and the same index without the guard keeps it.
test0 :: String
test0 = "function guarded(buf : &[u32; 4], i : usize) -> bool {\n" ++
        "    return i < 4 : usize && buf[i] > 0 : u32;\n" ++
        "}\n" ++
        "\n" ++
        "function unguarded(buf : &[u32; 4], i : usize) -> bool {\n" ++
        "    return buf[i] > 0 : u32;\n" ++
        "}\n"

-- | A divisor that the left operand keeps away from zero and a shift amount
-- that it keeps below the width are emitted without their checks. A bound at
-- the width itself leaves the check of the shift.
test1 :: String
test1 = "function divided(y : u32, x : u32) -> bool {\n" ++
        "    return x != 0 : u32 && y / x > 1 : u32;\n" ++
        "}\n" ++
        "\n" ++
        "function divided_or(y : u32, x : u32) -> bool {\n" ++
        "    return x == 0 : u32 || y % x == 0 : u32;\n" ++
        "}\n" ++
        "\n" ++
        "function shifted(v : u32, s : u32) -> bool {\n" ++
        "    return s < 32 : u32 && v << s > 0 : u32;\n" ++
        "}\n" ++
        "\n" ++
        "function shifted_wide(v : u32, s : u32) -> bool {\n" ++
        "    return s <= 32 : u32 && v << s > 0 : u32;\n" ++
        "}\n"

spec :: Spec
spec = do
  describe "Pretty printing guarded checks" $ do
    it "Leaves out the bounds check of an index that a guard keeps inside the array" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "_Bool guarded(const uint32_t buf[4U], const size_t i) {\n" ++
              "    \n" ++
              "    return i < 4U && buf[i] > 0U;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "_Bool unguarded(const uint32_t buf[4U], const size_t i) {\n" ++
              "    \n" ++
              "    return buf[termina__check__array_index(4U, i)] > 0U;\n" ++
              "\n" ++
              "}\n")
    it "Leaves out the checks of a guarded divisor and a guarded shift amount" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "_Bool divided(const uint32_t y, const uint32_t x) {\n" ++
              "    \n" ++
              "    return x != 0U && (uint32_t)(y / x) > 1U;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "_Bool divided_or(const uint32_t y, const uint32_t x) {\n" ++
              "    \n" ++
              "    return x == 0U || (uint32_t)(y % x) == 0U;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "_Bool shifted(const uint32_t v, const uint32_t s) {\n" ++
              "    \n" ++
              "    return s < 32U && (uint32_t)(v << s) > 0U;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "_Bool shifted_wide(const uint32_t v, const uint32_t s) {\n" ++
              "    \n" ++
              "    return s <= 32U\n" ++
              "           && (uint32_t)(v << termina__check__shift_amount(32U, s)) > 0U;\n" ++
              "\n" ++
              "}\n")
