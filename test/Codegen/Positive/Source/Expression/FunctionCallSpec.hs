module Codegen.Positive.Source.Expression.FunctionCallSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

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
    it "Declares the called functions" $ do
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
    it "Generates a function call expression" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint16_t func_test0_0(uint16_t a) {\n" ++
              "    \n" ++
              "    return a + 1U;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "uint16_t func_test0_1(uint16_t a) {\n" ++
              "    \n" ++
              "    uint16_t foo = func_test0_0(2U);\n" ++
              "\n" ++
              "    return foo * 2U;\n" ++
              "\n" ++
              "}\n")    