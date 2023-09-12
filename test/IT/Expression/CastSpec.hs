module IT.Expression.CastSpec (spec) where

import Test.Hspec
import Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

test0 :: String
test0 = "fn casting_test0() {\n" ++
        "    var bar_u8 : u8 = 0 : u8;\n" ++
        "    var bar_u16 : u16 = 0 : u16;\n" ++
        "    var bar_u32 : u32 = 0 : u32;\n" ++
        "    var bar_i8 : i8 = 0 : i8;\n" ++
        "    var bar_i16 : i16 = 0 : i16;\n" ++
        "    var bar_i32 : i32 = 0 : i32;\n" ++
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
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile tast

spec :: Spec
spec = do
  describe "Pretty printing casting expressions" $ do
    it "Prints declaration of function casting_test0" $ do
      renderHeader test0 `shouldBe`
        pack "void casting_test0();"
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("void casting_test0() {\n" ++
              "    \n" ++
              "    uint8_t bar_u8 = (uint8_t)0;\n" ++
              "\n" ++
              "    uint16_t bar_u16 = (uint16_t)0;\n" ++
              "\n" ++
              "    uint32_t bar_u32 = (uint32_t)0;\n" ++
              "\n" ++
              "    int8_t bar_i8 = (int8_t)0;\n" ++
              "\n" ++
              "    int16_t bar_i16 = (int16_t)0;\n" ++
              "\n" ++
              "    int32_t bar_i32 = (int32_t)0;\n" ++
              "\n" ++
              "    bar_u8 = ((uint8_t)((uint32_t)4294901760));\n" ++
              "\n" ++
              "    bar_u16 = ((uint16_t)bar_u8);\n" ++
              "\n" ++
              "    bar_u32 = ((uint32_t)bar_u8) + ((uint32_t)bar_i8);\n" ++
              "\n" ++
              "    bar_i16 = bar_i16 * ((int16_t)bar_u8);\n" ++
              "\n" ++
              "    bar_i32 = (int32_t)((int32_t)((int32_t)bar_i8 * ((int32_t)bar_i16)) * (int32_t)bar_u8) * ((int32_t)bar_u16);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")    