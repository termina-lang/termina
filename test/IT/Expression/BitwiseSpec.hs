module IT.Expression.BitwiseSpec (spec) where

import Test.Hspec
import Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

test0 :: String
test0 = "fn bitwise_test0(foo : u16) {\n" ++
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
  describe "Pretty printing bitwise shifting expressions" $ do
    it "Prints declaration of function bitwise_test0" $ do
      renderHeader test0 `shouldBe`
        pack "void bitwise_test0(uint16_t foo);"
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("void bitwise_test0(uint16_t foo) {\n" ++
              "    \n" ++
              "    uint8_t bar = (uint8_t)0;\n" ++ 
              "\n" ++
              "    foo = foo << (uint8_t)8;\n" ++
              "\n" ++
              "    bar = (uint8_t)8 << foo;\n" ++ 
              "\n" ++
              "    foo = foo >> (uint8_t)8;\n" ++ 
              "\n" ++
              "    (uint8_t)8 << foo;\n" ++
              "\n" ++
              "    foo = foo & (uint16_t)1024;\n" ++
              "\n" ++
              "    (uint16_t)1024 & foo;\n" ++
              "\n" ++
              "    foo = foo | (uint16_t)1024;\n" ++
              "\n" ++
              "    (uint16_t)1024 | foo;\n" ++
              "\n" ++
              "    foo = foo ^ (uint16_t)1024;\n" ++
              "\n" ++
              "    (uint16_t)1024 ^ foo;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")    