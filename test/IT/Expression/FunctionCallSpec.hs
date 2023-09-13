module IT.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

test0 :: String
test0 = "fn test0(a : u16) -> u16 {\n" ++
        "    return (a + (1 : u16));\n" ++
        "}\n" ++
        "\n" ++
        "fn test1(a : u16) -> u16 {\n" ++
        "    var foo : u16 = test0(2 : u16);\n" ++
        "    return (foo * (2 : u16));\n" ++
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
  describe "Pretty printing function call expressions" $ do
    it "Prints declaration of functions test0 and test1" $ do
      renderHeader test0 `shouldBe`
        pack ("uint16_t test0(uint16_t a);\n" ++
              "uint16_t test1(uint16_t a);")
    it "Prints definition of functions test0 and test1" $ do
      renderSource test0 `shouldBe`
        pack ("uint16_t test0(uint16_t a) {\n" ++
              "    \n" ++
              "    return;\n" ++
              "\n" ++
              "}")    