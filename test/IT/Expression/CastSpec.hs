module IT.Expression.CastSpec (spec) where

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
test0 = "function casting_test0() {\n" ++
        "    var bar_u8 : u8 = 0;\n" ++
        "    var bar_u16 : u16 = 0;\n" ++
        "    var bar_u32 : u32 = 0;\n" ++
        "    var bar_i8 : i8 = 0;\n" ++
        "    var bar_i16 : i16 = 0;\n" ++
        "    var bar_i32 : i32 = 0;\n" ++
        "    bar_u8 = ((0xFFFF0000 : u32) as u8);\n" ++
        "    bar_u16 = (bar_u8 as u16);\n" ++
        "    bar_u32 = (bar_u8 as u32) + (bar_i8 as u32);\n" ++
        "    bar_i16 = bar_i16 * (bar_u8 as i16);\n" ++
        "    bar_i32 = bar_i8 as i32 * (bar_i16 as i32) * bar_u8 as i32 * (bar_u16 as i32);\n" ++
        "    return;\n" ++
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
  describe "Pretty printing casting expressions" $ do
    it "Prints declaration of function casting_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void casting_test0();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void casting_test0() {\n" ++
              "    \n" ++
              "    uint8_t bar_u8 = 0;\n" ++
              "\n" ++
              "    uint16_t bar_u16 = 0;\n" ++
              "\n" ++
              "    uint32_t bar_u32 = 0;\n" ++
              "\n" ++
              "    int8_t bar_i8 = 0;\n" ++
              "\n" ++
              "    int16_t bar_i16 = 0;\n" ++
              "\n" ++
              "    int32_t bar_i32 = 0;\n" ++
              "\n" ++
              "    bar_u8 = (uint8_t)0xFFFF0000;\n" ++
              "\n" ++
              "    bar_u16 = (uint16_t)bar_u8;\n" ++
              "\n" ++
              "    bar_u32 = (uint32_t)bar_u8 + (uint32_t)bar_i8;\n" ++
              "\n" ++
              "    bar_i16 = bar_i16 * (int16_t)bar_u8;\n" ++
              "\n" ++
              "    bar_i32 = (int32_t)((int32_t)((int32_t)bar_i8 * (int32_t)bar_i16) * (int32_t)bar_u8) * (int32_t)bar_u16;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    