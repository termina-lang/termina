module IT.Expression.ArrayIndexSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "function array_test0() {\n" ++
        "    var foo : usize = 0;\n" ++
        "    var array0 : [u32; 10 : usize] = [0 : u32; 10 : usize];\n" ++
        "    var array1 : [[i64; 5]; 10] = [[0; 5 : usize]; 10];\n" ++
        "    array0[3 : usize] = 10 : u32;\n" ++
        "    array0[foo] = 1024 : u32;\n" ++
        "    array1[3][4] = 1024;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "function array_test1(p_array0 : &mut [u32; 10]) {\n" ++
        "    var foo : u32 = 0 : u32;\n" ++
        "    (*p_array0)[3 : usize] = 10 : u32;\n" ++
        "    (*p_array0)[foo as usize] = 1024 : u32;\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing array index expressions" $ do
    it "Prints declaration of function array_test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void array_test0();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function array_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void array_test0() {\n" ++
              "    \n" ++
              "    size_t foo = 0;\n" ++
              "\n" ++
              "    uint32_t array0[10];\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        array0[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    int64_t array1[10][5];\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
              "            array1[__i0][__i1] = 0;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    array0[3] = 10;\n" ++
              "\n" ++
              "    array0[foo] = 1024;\n" ++
              "\n" ++
              "    array1[3][4] = 1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function array_test1" $ do
      renderHeader False test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void array_test1(uint32_t p_array0[10]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function array_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void array_test1(uint32_t p_array0[10]) {\n" ++
              "    \n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    p_array0[3] = 10;\n" ++
              "\n" ++
              "    p_array0[(size_t)foo] = 1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")   