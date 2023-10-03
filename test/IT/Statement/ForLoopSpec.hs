module IT.Statement.ForLoopSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn for_loop_test0(array0 : [u16; 10 : u32]) -> u16 {\n" ++
        "    var total : u16 = 0 : u16;\n" ++
        "    for i in 0 : u32 .. 10 : u32 {\n" ++
        "        total = total + array0[i];\n" ++  
        "    }\n" ++
        "    return total;\n" ++
        "}"

test1 :: String
test1 = "fn for_loop_test1(array0 : & [u16; 10 : u32]) -> bool {\n" ++
        "    var found : bool = false;\n" ++
        "    for i in 0 : u32 .. 10 : u32 while found == false {\n" ++
        "        if (*array0)[i] == 1024 : u16 {\n" ++
        "            found = true;\n" ++
        "        }\n" ++   
        "    }\n" ++
        "    return found;\n" ++
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
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints declaration of function for_loop_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("typedef struct {\n" ++
              "    uint16_t array[10];\n" ++
              "} __param_for_loop_test0_array0_t;\n" ++
              "\n" ++
              "uint16_t for_loop_test0(__param_for_loop_test0_array0_t array0);\n")
    it "Prints definition of function for_loop_test0_test0" $ do
      renderSource test0 `shouldBe`
        pack ("uint16_t for_loop_test0(__param_for_loop_test0_array0_t array0) {\n" ++
              "    \n" ++
              "    uint16_t total = 0;\n" ++
              "\n" ++
              "    {\n" ++
              "        uint32_t __start = 0;\n" ++
              "        uint32_t __end = 10;\n" ++
              "\n" ++
              "        for (uint32_t i = __start; i < __end; i = i + 1) {\n" ++
              "            \n" ++
              "            total = total + array0.array[i];\n" ++
              "\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return total;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function for_loop_test0_test1" $ do
      renderHeader test1 `shouldBe`
        pack "_Bool for_loop_test1(uint16_t array0[10]);\n"
    it "Prints definition of function assignment_test1" $ do
      renderSource test1 `shouldBe`
        pack ("_Bool for_loop_test1(uint16_t array0[10]) {\n" ++
              "    \n" ++
              "    _Bool found = 0;\n" ++
              "\n" ++
              "    {\n" ++
              "        uint32_t __start = 0;\n" ++
              "        uint32_t __end = 10;\n" ++
              "\n" ++
              "        for (uint32_t i = __start; i < __end && (found == 0); i = i + 1) {\n" ++
              "            \n" ++
              "            if (array0[i] == 1024) {\n" ++
              "\n" ++
              "                found = 1;\n" ++
              "\n" ++
              "            }\n" ++  
              "\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return found;\n" ++
              "\n" ++
              "}\n")    