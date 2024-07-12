module IT.Expression.RelationalSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Monad
import Text.Parsec
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer

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

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenHeaderFile False "test" [] tast M.empty of
          Left err -> pack $ show err
          Right cHeaderFile -> runCPrinter cHeaderFile

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenSourceFile "test" tast of
          Left err -> pack $ show err
          Right cSourceFile -> runCPrinter cSourceFile

spec :: Spec
spec = do
  describe "Pretty printing relational expressions" $ do
    it "Prints declaration of function relational_test0" $ do
      renderHeader test0 `shouldBe`
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