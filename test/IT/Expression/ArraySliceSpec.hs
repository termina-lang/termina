module IT.Expression.ArraySliceSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Monad
import Text.Parsec
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.Common

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

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
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
  describe "Pretty printing array slicing expressions" $ do
    it "Prints declaration of function slice_test0" $ do
      renderHeader test0 `shouldBe`
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