module IT.Statement.ForLoopSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "function for_loop_test0(array0 : & [u16; 10]) -> u16 {\n" ++
        "    var total : u16 = 0 : u16;\n" ++
        "    for i : usize in 0 : usize .. 10 : usize {\n" ++
        "        total = total + array0[i];\n" ++  
        "    }\n" ++
        "    return total;\n" ++
        "}"

test1 :: String
test1 = "function for_loop_test1(array0 : & [u16; 10]) -> bool {\n" ++
        "    var found : bool = false;\n" ++
        "    for i : usize in 0 : usize .. 10 : usize while found == false {\n" ++
        "        if (*array0)[i] == 1024 : u16 {\n" ++
        "            found = true;\n" ++
        "        }\n" ++   
        "    }\n" ++
        "    return found;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints declaration of function for_loop_test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint16_t for_loop_test0(const uint16_t array0[10U]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function for_loop_test0_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint16_t for_loop_test0(const uint16_t array0[10U]) {\n" ++
              "    \n" ++
              "    uint16_t total = 0U;\n" ++
              "\n" ++
              "    for (size_t i = 0U; i < 10U; i = i + 1U) {\n" ++
              "        \n" ++
              "        total = total + array0[__termina_array__index(10U, i)];\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return total;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function for_loop_test0_test1" $ do
      renderHeader False test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "_Bool for_loop_test1(const uint16_t array0[10U]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function assignment_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "_Bool for_loop_test1(const uint16_t array0[10U]) {\n" ++
              "    \n" ++
              "    _Bool found = 0;\n" ++
              "\n" ++
              "    for (size_t i = 0U; i < 10U && found == 0; i = i + 1U) {\n" ++
              "        \n" ++
              "        if (array0[__termina_array__index(10U, i)] == 1024U) {\n" ++
              "            \n" ++
              "            found = 1;\n" ++
              "\n" ++
              "        }\n" ++  
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return found;\n" ++
              "\n" ++
              "}\n")    