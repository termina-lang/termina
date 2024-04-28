module IT.Function.ConstParamSpec (spec) where

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
test0 = "function test0<const N : u16>() -> u16 {\n" ++
        "    var foo : u16 = N;\n" ++
        "    foo = foo + 1024;\n" ++
        "    return foo;\n" ++
        "}"

test1 :: String
test1 = "function test1<const N : usize>(array0 : &[u16; N]) -> u16 {\n" ++
        "    var foo : u16 = 0;\n" ++
        "    foo = foo + (*array0)[0];\n" ++
        "    return foo;\n" ++
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
  describe "Pretty printing functions with const parameters" $ do
    it "Prints declaration of function test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint16_t test0(uint16_t N);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint16_t test0(uint16_t N) {\n" ++
              "    \n" ++
              "    uint16_t foo = N;\n" ++ 
              "\n" ++
              "    foo = foo + 1024;\n" ++
              "\n" ++
              "    return foo;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint16_t test1(size_t N, const uint16_t array0[N]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test0" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint16_t test1(size_t N, const uint16_t array0[N]) {\n" ++
              "    \n" ++
              "    uint16_t foo = 0;\n" ++ 
              "\n" ++
              "    foo = foo + array0[0];\n" ++
              "\n" ++
              "    return foo;\n" ++
              "\n" ++
              "}\n")    