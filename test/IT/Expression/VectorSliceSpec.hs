module IT.Expression.VectorSliceSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn slice_test0() {\n" ++
        "    var vector0 : [u32; 10 : u32] = [0 : u32; 10 : u32];\n" ++
        "    vector0[3 : u32..10 : u32][1 : u32] = 10 : u32;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "fn slice_test1(vector0 : & [[[u32; 3 : u32]; 5 : u32]; 10 : u32]) {\n" ++
        "    (*vector0)[3 : u32 .. 8 : u32] = [[[10 : u32; 3 : u32]; 5 : u32]; 5 : u32];\n" ++
        "    return;\n" ++
        "}"

test2 :: String
test2 = "fn add_one(input : & [u32; 5 : u32]) {\n" ++ 
        "    for i in 0 : u32 .. 5 : u32 {\n" ++
        "        (*input)[i] = (*input)[i] + 1 : u32;\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "fn slice_test2(vector0 : & [[u32; 5 : u32]; 10 : u32]) {\n" ++
        "    add_one(&(*vector0)[2 : u32 .. 3 : u32][0 : u32]);\n" ++
        "    return;\n" ++
        "}"

test3 :: String
test3 = "fn add_two(input : [u32; 5 : u32]) {\n" ++ 
        "    for i in 0 : u32 .. 5 : u32 {\n" ++
        "        input[i] = input[i] + 2 : u32;\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "fn slice_test3(vector0 : & [[u32; 5 : u32]; 10 : u32]) {\n" ++
        "    add_two((*vector0)[2 : u32 .. 3 : u32][0 : u32]);\n" ++
        "    return;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile tast

spec :: Spec
spec = do
  describe "Pretty printing vector slicing expressions" $ do
    it "Prints declaration of function slice_test0" $ do
      renderHeader test0 `shouldBe`
        pack "void slice_test0();\n"
    it "Prints definition of function slice_test0" $ do
      renderSource test0 `shouldBe`
        pack ("void slice_test0() {\n" ++
              "    \n" ++
              "    uint32_t vector0[10];\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            vector0[__i0] = 0;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    (&vector0[3])[1] = 10;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function slice_test1" $ do
      renderHeader test1 `shouldBe`
        pack "void slice_test1(uint32_t vector0[10][5][3]);\n"
    it "Prints definition of function slice_test0" $ do
      renderSource test1 `shouldBe`
        pack ("void slice_test1(uint32_t vector0[10][5][3]) {\n" ++
              "    \n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < 5; __i0 = __i0 + 1) {\n" ++
              "            for (uint32_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
              "                for (uint32_t __i2 = 0; __i2 < 3; __i2 = __i2 + 1) {\n" ++
              "                    (&vector0[3])[__i0][__i1][__i2] = 10;\n" ++
              "                }\n" ++
              "            }\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n") 
    it "Prints declaration of function slice_test2" $ do
      renderHeader test2 `shouldBe`
        pack ("void add_one(uint32_t input[5]);\n" ++
              "\n" ++
              "void slice_test2(uint32_t vector0[10][5]);\n")
    it "Prints definition of function slice_test2" $ do
      renderSource test2 `shouldBe`
        pack ("void add_one(uint32_t input[5]) {\n" ++
              "    \n" ++
              "    {\n" ++
              "        uint32_t __start = 0;\n" ++
              "        uint32_t __end = 5;\n" ++
              "\n" ++
              "        for (uint32_t i = __start; i < __end; i = i + 1) {\n" ++
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
              "    \n" ++
              "    add_one((&vector0[2])[0]);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function slice_test3" $ do
      renderHeader test3 `shouldBe`
        pack ("typedef struct {\n" ++
              "    uint32_t array[5];\n" ++
              "} __param_add_two_input_t;\n" ++
              "\n" ++
              "void add_two(__param_add_two_input_t input);\n" ++
              "\n" ++
              "void slice_test3(uint32_t vector0[10][5]);\n")
    it "Prints definition of function slice_test3" $ do
      renderSource test3 `shouldBe`
        pack ("void add_two(__param_add_two_input_t input) {\n" ++
              "    \n" ++
              "    {\n" ++
              "        uint32_t __start = 0;\n" ++
              "        uint32_t __end = 5;\n" ++
              "\n" ++
              "        for (uint32_t i = __start; i < __end; i = i + 1) {\n" ++
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
              "    \n" ++
              "    add_two(*((__param_add_two_input_t *)(&vector0[2])[0]));\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n") 