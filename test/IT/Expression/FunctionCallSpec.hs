module IT.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import Parser.Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

test0 :: String
test0 = "function func_test0_0(a : u16) -> u16 {\n" ++
        "    return (a + (1 : u16));\n" ++
        "}\n" ++
        "\n" ++
        "function func_test0_1(a : u16) -> u16 {\n" ++
        "    var foo : u16 = func_test0_0(2 : u16);\n" ++
        "    return (foo * (2 : u16));\n" ++
        "}"

test1 :: String
test1 = "function func_test1_0() -> [u32; 10] {\n" ++
        "    var foo : [u32; 10] = [1024 : u32; 10];\n" ++
        "    return foo;\n" ++
        "}\n" ++
        "\n" ++
        "function func_test1_1() -> u32 {\n" ++
        "    var bar0 : [u32; 10] = [0 : u32; 10];\n" ++
        "    bar0 = func_test1_0();\n" ++
        "    var bar1 : [u32; 10] = func_test1_0();\n" ++
        "    return bar0[1 : usize] + bar1[2 : usize];\n" ++
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
  describe "Pretty printing function call expressions" $ do
    it "Prints declaration of functions of test0" $ do
      renderHeader test0 `shouldBe`
        pack ("uint16_t func_test0_0(uint16_t a);\n\n" ++
              "uint16_t func_test0_1(uint16_t a);\n")
    it "Prints definition of functions of test0" $ do
      renderSource test0 `shouldBe`
        pack ("uint16_t func_test0_0(uint16_t a) {\n" ++
              "\n" ++
              "    return a + 1;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "uint16_t func_test0_1(uint16_t a) {\n" ++
              "\n" ++
              "    uint16_t foo = func_test0_0(2);\n" ++
              "\n" ++
              "    return foo * 2;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of functions test0 and test1" $ do
      renderHeader test1 `shouldBe`
        pack ("typedef struct {\n" ++
              "    uint32_t array[10];\n" ++
              "} __ret_func_test1_0_t;\n" ++
              "\n" ++
              "__ret_func_test1_0_t func_test1_0();\n" ++
              "\n" ++
              "uint32_t func_test1_1();\n")
    it "Prints definition of functions test0 and test1" $ do
      renderSource test1 `shouldBe`
        pack ("__ret_func_test1_0_t func_test1_0() {\n" ++
              "\n" ++
              "    uint32_t foo[10];\n" ++
              "\n" ++
              "    {\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            foo[__i0] = 1024;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return *((__ret_func_test1_0_t *)foo);\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "uint32_t func_test1_1() {\n" ++
              "\n" ++
              "    uint32_t bar0[10];\n" ++
              "\n" ++
              "    {\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            bar0[__i0] = 0;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    *((__ret_func_test1_0_t *)bar0) = *((__ret_func_test1_0_t *)func_test1_0().array);\n" ++
              "\n" ++
              "    uint32_t bar1[10];\n" ++
              "\n" ++
              "    {\n" ++
              "        *((__ret_func_test1_0_t *)bar1) = *((__ret_func_test1_0_t *)func_test1_0().array);\n" ++
              "    }\n" ++
              "\n" ++
              "    return bar0[1] + bar1[2];\n" ++
              "\n" ++
              "}\n")    