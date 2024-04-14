module IT.Statement.ForLoopSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec
import qualified Data.Map as M
import Control.Monad.Reader
import Generator.Module
import Generator.LanguageC.Printer
import System.Path
import Modules.Modules

test0 :: String
test0 = "function for_loop_test0(array0 : [u16; 10]) -> u16 {\n" ++
        "    var total : u16 = 0 : u16;\n" ++
        "    for i : usize in 0 : usize .. 10 : usize {\n" ++
        "        total = total + array0[i];\n" ++  
        "    }\n" ++
        "    return total;\n" ++
        "}"

test1 :: String
test1 = "function for_loop_test1(array0 : & [u16; 10]) -> bool {\n" ++
        "    var found : bool = false;\n" ++
        "    for i : usize in 0 : usize .. 10 : usize while found == false {\n" ++
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
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints declaration of function for_loop_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint16_t for_loop_test0(__wrapper_uint16__10_t array0);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function for_loop_test0_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint16_t for_loop_test0(__wrapper_uint16__10_t array0) {\n" ++
              "    \n" ++
              "    uint16_t total = 0;\n" ++
              "\n" ++
              "    for (size_t i = 0; i < 10; i = i + 1) {\n" ++
              "        \n" ++
              "        total = total + array0.array[i];\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return total;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function for_loop_test0_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "_Bool for_loop_test1(const uint16_t array0[10]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function assignment_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "_Bool for_loop_test1(const uint16_t array0[10]) {\n" ++
              "    \n" ++
              "    _Bool found = 0;\n" ++
              "\n" ++
              "    for (size_t i = 0; i < 10 && found == 0; i = i + 1) {\n" ++
              "        \n" ++
              "        if (array0[i] == 1024) {\n" ++
              "            \n" ++
              "            found = 1;\n" ++
              "\n" ++
              "        }\n" ++  
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return found;\n" ++
              "\n" ++
              "}\n")    