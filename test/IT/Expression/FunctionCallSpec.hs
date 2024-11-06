module IT.Expression.FunctionCallSpec (spec) where

import IT.Common

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Semantic.Monad
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.BasicBlocks

test0 :: String
test0 = "function func_test0_0(a : u16) -> u16 {\n" ++
        "    return a + 1;\n" ++
        "}\n" ++
        "\n" ++
        "function func_test0_1(a : u16) -> u16 {\n" ++
        "    var foo : u16 = func_test0_0(2);\n" ++
        "    return foo * 2;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing function call expressions" $ do
    it "Prints declaration of functions of test0" $ do
      renderHeader False test0 `shouldBe`
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