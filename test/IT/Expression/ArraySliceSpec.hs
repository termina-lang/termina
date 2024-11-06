module IT.Expression.ArraySliceSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Monad
import Text.Parsec
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.BasicBlocks

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
              "void add_one(uint32_t input[5]);\n" ++
              "\n" ++
              "void slice_test0(uint32_t array0[5][10]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function slice_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void add_one(uint32_t input[5]) {\n" ++
              "    \n" ++
              "    for (size_t i = 0; i < 5; i = i + 1) {\n" ++
              "        \n" ++
              "        input[i] = input[i] + 1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void slice_test0(uint32_t array0[5][10]) {\n" ++
              "    \n" ++
              "    add_one(&array0[0][0]);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")