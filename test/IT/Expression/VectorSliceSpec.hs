module IT.Expression.VectorSliceSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "function slice_test0() {\n" ++
        "    var vector0 : [u32; 10] = [0 : u32; 10];\n" ++
        "    vector0[3 : usize .. 10 : usize][1 : usize] = 10 : u32;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "function slice_test1(vector0 : & [[[u32; 3]; 5]; 10]) {\n" ++
        "    (*vector0)[3 : usize .. 8 : usize] = [[[10 : u32; 3]; 5]; 5];\n" ++
        "    return;\n" ++
        "}"

test2 :: String
test2 = "function add_one(input : & [u32; 5]) {\n" ++ 
        "    for i : usize in 0 : usize .. 5 : usize {\n" ++
        "        (*input)[i] = (*input)[i] + 1 : u32;\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "function slice_test2(vector0 : & [[u32; 5]; 10]) {\n" ++
        "    add_one(&(*vector0)[2 : usize .. 3 : usize][0 : usize]);\n" ++
        "    return;\n" ++
        "}"

test3 :: String
test3 = "function add_two(input : [u32; 5]) {\n" ++ 
        "    for i : usize in 0 : usize .. 5 : usize {\n" ++
        "        input[i] = input[i] + 2 : u32;\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "function slice_test3(vector0 : & [[u32; 5]; 10]) {\n" ++
        "    add_two((*vector0)[2 : usize .. 3 : usize][0 : usize]);\n" ++
        "    return;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile [pack "test"] [] tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile [pack "test"] tast

spec :: Spec
spec = do
  describe "Pretty printing vector slicing expressions" $ do
    it "Prints declaration of function slice_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void slice_test0();\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function slice_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void slice_test0() {\n" ++
              "\n" ++
              "    uint32_t vector0[10];\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        vector0[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    (&vector0[3])[1] = 10;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function slice_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void slice_test1(uint32_t vector0[10][5][3]);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function slice_test0" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void slice_test1(uint32_t vector0[10][5][3]) {\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 5; __i0 = __i0 + 1) {\n" ++
              "        for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
              "            for (size_t __i2 = 0; __i2 < 3; __i2 = __i2 + 1) {\n" ++
              "                (&vector0[3])[__i0][__i1][__i2] = 10;\n" ++
              "            }\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n") 
    it "Prints declaration of function slice_test2" $ do
      renderHeader test2 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void add_one(uint32_t input[5]);\n" ++
              "\n" ++
              "void slice_test2(uint32_t vector0[10][5]);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function slice_test2" $ do
      renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void add_one(uint32_t input[5]) {\n" ++
              "\n" ++
              "    {\n" ++
              "        size_t __start = 0;\n" ++
              "        size_t __end = 5;\n" ++
              "\n" ++
              "        for (size_t i = __start; i < __end; i = i + 1) {\n" ++
              "            \n" ++
              "            input[i] = input[i] + 1;\n" ++
              "\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void slice_test2(uint32_t vector0[10][5]) {\n" ++
              "\n" ++
              "    add_one((&vector0[2])[0]);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function slice_test3" $ do
      renderHeader test3 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t array[5];\n" ++
              "} __param_add_two_input_t;\n" ++
              "\n" ++
              "void add_two(__param_add_two_input_t input);\n" ++
              "\n" ++
              "void slice_test3(uint32_t vector0[10][5]);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function slice_test3" $ do
      renderSource test3 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void add_two(__param_add_two_input_t input) {\n" ++
              "\n" ++
              "    {\n" ++
              "        size_t __start = 0;\n" ++
              "        size_t __end = 5;\n" ++
              "\n" ++
              "        for (size_t i = __start; i < __end; i = i + 1) {\n" ++
              "            \n" ++
              "            input.array[i] = input.array[i] + 2;\n" ++
              "\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void slice_test3(uint32_t vector0[10][5]) {\n" ++
              "\n" ++
              "    add_two(*((__param_add_two_input_t *)(&vector0[2])[0]));\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n") 