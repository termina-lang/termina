module Codegen.Positive.Source.Expression.GuardedIndexSpec (spec) where

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

spec :: Spec
spec = do
  describe "Pretty printing guarded array indices" $ do
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
