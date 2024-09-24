module IT.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Semantic.Monad
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.Common
import Control.Monad.Except

test0 :: String
test0 = "function func_test0_0(a : u16) -> u16 {\n" ++
        "    return a + 1;\n" ++
        "}\n" ++
        "\n" ++
        "function func_test0_1(a : u16) -> u16 {\n" ++
        "    var foo : u16 = func_test0_0(2);\n" ++
        "    return foo * 2;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runExcept (genBBModule tast) of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenHeaderFile False "test" [] bbAST M.empty of
              Left err -> pack $ show err
              Right cHeaderFile -> runCPrinter cHeaderFile

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile "test" bbAST of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter cSourceFile

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