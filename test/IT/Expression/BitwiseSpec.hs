module IT.Expression.BitwiseSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import qualified Data.Map as M
import Control.Monad.Reader
import Generator.Module
import PPrinter
import Generator.LanguageC.Printer
import System.Path
import Modules.Modules

test0 :: String
test0 = "function bitwise_test0(foo : u16) {\n" ++
        "    var bar : u8 = 0 : u8;\n" ++
        "    foo = foo << 8 : u8;\n" ++
        "    bar = 8 : u8 << foo;\n" ++
        "    foo = foo >> 8 : u8;\n" ++
        "    8 : u8 << foo;\n" ++
        "    foo = foo & 1024: u16;\n" ++
        "    1024 : u16 & foo;\n" ++
        "    foo = foo | 1024: u16;\n" ++
        "    1024 : u16 | foo;\n" ++
        "    foo = foo ^ 1024: u16;\n" ++
        "    1024 : u16 ^ foo;\n" ++
        "    return;\n" ++
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
  describe "Pretty printing bitwise shifting expressions" $ do
    it "Prints declaration of function bitwise_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void bitwise_test0(uint16_t foo);\n" ++
              "\n" ++
              "#endif")
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void bitwise_test0(uint16_t foo) {\n" ++
              "    \n" ++
              "    uint8_t bar = 0;\n" ++ 
              "\n" ++
              "    foo = foo << 8;\n" ++
              "\n" ++
              "    bar = 8 << foo;\n" ++ 
              "\n" ++
              "    foo = foo >> 8;\n" ++ 
              "\n" ++
              "    8 << foo;\n" ++
              "\n" ++
              "    foo = foo & 1024;\n" ++
              "\n" ++
              "    1024 & foo;\n" ++
              "\n" ++
              "    foo = foo | 1024;\n" ++
              "\n" ++
              "    1024 | foo;\n" ++
              "\n" ++
              "    foo = foo ^ 1024;\n" ++
              "\n" ++
              "    1024 ^ foo;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")    