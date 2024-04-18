module IT.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import qualified Data.Map as M
import Control.Monad.Reader
import Generator.Module
import Generator.LanguageC.Printer
import System.Path
import Modules.Modules

test0 :: String
test0 = "function func_test0_0(a : u16) -> u16 {\n" ++
        "    return a + 1;\n" ++
        "}\n" ++
        "\n" ++
        "function func_test0_1(a : u16) -> u16 {\n" ++
        "    var foo : u16 = func_test0_0(2);\n" ++
        "    return foo * 2;\n" ++
        "}"

test1 :: String
test1 = "function func_test1_0() -> [u32; 10] {\n" ++
        "    var foo : [u32; 10] = [1024 : u32; 10];\n" ++
        "    return foo;\n" ++
        "}\n" ++
        "\n" ++
        "function func_test1_1() -> u32 {\n" ++
        "    var bar0 : [u32; 10] = [0; 10];\n" ++
        "    bar0 = func_test1_0();\n" ++
        "    var bar1 : [u32; 10] = func_test1_0();\n" ++
        "    return bar0[1] + bar1[2];\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> 
        case runReaderT (genHeaderFile False (fragment "test") SrcFile [] tast) M.empty of
          Left err -> pack $ show err
          Right cHeaderFile -> render $ runReader (pprint cHeaderFile) (CPrinterConfig False False)

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> 
        case runReaderT (genSourceFile (fragment "test") tast) M.empty of
          Left err -> pack $ show err
          Right cHeaderFile -> render $ runReader (pprint cHeaderFile) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing function call expressions" $ do
    it "Prints declaration of functions of test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint16_t func_test0_0(uint16_t a);\n\n" ++
              "uint16_t func_test0_1(uint16_t a);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of functions of test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint16_t func_test0_0(uint16_t a) {\n" ++
              "    \n" ++
              "    return a + 1;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "uint16_t func_test0_1(uint16_t a) {\n" ++
              "    \n" ++
              "    uint16_t foo = func_test0_0(2);\n" ++
              "\n" ++
              "    return foo * 2;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of functions test0 and test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "__wrapper_uint32__10_t func_test1_0();\n" ++
              "\n" ++
              "uint32_t func_test1_1();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of functions test0 and test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "__wrapper_uint32__10_t func_test1_0() {\n" ++
              "    \n" ++
              "    uint32_t foo[10];\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        foo[__i0] = 1024;\n" ++
              "    }\n" ++
              "\n" ++
              "    return *(__wrapper_uint32__10_t *)foo;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "uint32_t func_test1_1() {\n" ++
              "    \n" ++
              "    uint32_t bar0[10];\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        bar0[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    *(__wrapper_uint32__10_t *)bar0 = func_test1_0().array;\n" ++
              "\n" ++
              "    uint32_t bar1[10];\n" ++
              "    *(__wrapper_uint32__10_t *)bar1 = func_test1_0().array;\n" ++
              "\n" ++
              "    return bar0[1] + bar1[2];\n" ++
              "\n" ++
              "}\n")    