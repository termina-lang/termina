module IT.Expression.RelationalSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn relational_test0(foo : u16) {\n" ++
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
  describe "Pretty printing relational expressions" $ do
    it "Prints declaration of function relational_test0" $ do
      renderHeader test0 `shouldBe`
        pack "void relational_test0(uint16_t foo);\n"
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("void relational_test0(uint16_t foo) {\n" ++
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