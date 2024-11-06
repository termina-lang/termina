module IT.Expression.RelationalSpec (spec) where

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
test0 = "function relational_test0(foo : u16) {\n" ++
        "    var res : bool = false;\n" ++
        "    res = foo == 1024 : u16;\n" ++
        "    res = 1024 : u16 == foo;\n" ++
        "    res = foo != 1024 : u16;\n" ++
        "    res = 1024 : u16 != foo;\n" ++
        "    res = foo > 1024 : u16;\n" ++
        "    res = 1024 : u16 > foo;\n" ++
        "    res = foo >= 1024 : u16;\n" ++
        "    res = 1024 : u16 >= foo;\n" ++
        "    res = foo < 1024 : u16;\n" ++
        "    res = 1024 : u16 < foo;\n" ++
        "    res = foo <= 1024 : u16;\n" ++
        "    res = true && false;\n" ++
        "    res = true || false;\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing relational expressions" $ do
    it "Prints declaration of function relational_test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void relational_test0(uint16_t foo);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void relational_test0(uint16_t foo) {\n" ++
              "    \n" ++
              "    _Bool res = 0;\n" ++
              "\n" ++
              "    res = foo == 1024;\n" ++
              "\n" ++
              "    res = 1024 == foo;\n" ++
              "\n" ++
              "    res = foo != 1024;\n" ++
              "\n" ++
              "    res = 1024 != foo;\n" ++ 
              "\n" ++
              "    res = foo > 1024;\n" ++
              "\n" ++
              "    res = 1024 > foo;\n" ++
              "\n" ++
              "    res = foo >= 1024;\n" ++
              "\n" ++
              "    res = 1024 >= foo;\n" ++
              "\n" ++
              "    res = foo < 1024;\n" ++
              "\n" ++
              "    res = 1024 < foo;\n" ++
              "\n" ++
              "    res = foo <= 1024;\n" ++
              "\n" ++
              "    res = 1 && 0;\n" ++
              "\n" ++
              "    res = 1 || 0;\n" ++ 
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")