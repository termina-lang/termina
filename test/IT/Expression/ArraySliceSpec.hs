module IT.Expression.ArraySliceSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "function add_one(input : &mut [u32; 5]) {\n" ++ 
        "    for i : usize in 0 : usize .. 5 : usize {\n" ++
        "        (*input)[i] = (*input)[i] + 1 : u32;\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "function slice_test0(array0 : &mut [[u32; 10]; 5]) {\n" ++
        "    add_one(&mut(*array0)[0][0..6]);\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing array slicing expressions" $ do
    it "Prints declaration of function slice_test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void add_one(uint32_t input[5U]);\n" ++
              "\n" ++
              "void slice_test0(uint32_t array0[5U][10U]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function slice_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void add_one(uint32_t input[5U]) {\n" ++
              "    \n" ++
              "    for (size_t i = 0U; i < 5U; i = i + 1U) {\n" ++
              "        \n" ++
              "        input[i] = input[i] + 1U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void slice_test0(uint32_t array0[5U][10U]) {\n" ++
              "    \n" ++
              "    add_one(&array0[0U][0U]);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")