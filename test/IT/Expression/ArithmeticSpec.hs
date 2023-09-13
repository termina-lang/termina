module IT.Expression.ArithmeticSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn test0() {\n" ++
        "    var foo : u16 = 0 : u16;\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    1024 : u16 + foo;\n" ++
        "    foo = foo - 1024 : u16;\n" ++
        "    1024 : u16 - foo;\n" ++
        "    foo = foo * 1024 : u16;\n" ++
        "    1024 : u16 * foo;\n" ++
        "    foo = foo / 1024 : u16;\n" ++
        "    1024 : u16 / foo;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "fn test1(foo : 'dyn u16) {\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    1024 : u16 + foo;\n" ++
        "    foo = foo - 1024 : u16;\n" ++
        "    1024 : u16 - foo;\n" ++
        "    foo = foo * 1024 : u16;\n" ++
        "    1024 : u16 * foo;\n" ++
        "    foo = foo / 1024 : u16;\n" ++
        "    1024 : u16 / foo;\n" ++
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
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints declaration of function test0" $ do
      renderHeader test0 `shouldBe`
        pack "void test0();"
    it "Prints definition of function test0" $ do
      renderSource test0 `shouldBe`
        pack ("void test0() {\n" ++
              "    \n" ++
              "    uint16_t foo = (uint16_t)0;\n" ++ 
              "\n" ++
              "    foo = foo + (uint16_t)1024;\n" ++
              "\n" ++
              "    (uint16_t)1024 + foo;\n" ++ 
              "\n" ++
              "    foo = foo - (uint16_t)1024;\n" ++ 
              "\n" ++
              "    (uint16_t)1024 - foo;\n" ++
              "\n" ++
              "    foo = foo * (uint16_t)1024;\n" ++
              "\n" ++
              "    (uint16_t)1024 * foo;\n" ++
              "\n" ++
              "    foo = foo / (uint16_t)1024;\n" ++
              "\n" ++
              "    (uint16_t)1024 / foo;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")    
    it "Prints declaration of function test1" $ do
     renderHeader test1 `shouldBe`
       pack "void test1(__dyn_t foo);"
    it "Prints definition of function test1" $ do
     renderSource test1 `shouldBe`
       pack ("void test1(__dyn_t foo) {\n" ++
             "    \n" ++
             "    *((uint16_t *)foo.datum) = *((uint16_t *)foo.datum) + (uint16_t)1024;\n" ++
             "\n" ++
             "    (uint16_t)1024 + *((uint16_t *)foo.datum);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.datum) = *((uint16_t *)foo.datum) - (uint16_t)1024;\n" ++
             "\n" ++
             "    (uint16_t)1024 - *((uint16_t *)foo.datum);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.datum) = *((uint16_t *)foo.datum) * (uint16_t)1024;\n" ++
             "\n" ++
             "    (uint16_t)1024 * *((uint16_t *)foo.datum);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.datum) = *((uint16_t *)foo.datum) / (uint16_t)1024;\n" ++
             "\n" ++
             "    (uint16_t)1024 / *((uint16_t *)foo.datum);\n" ++
             "\n" ++
             "    return;\n" ++
             "\n" ++
             "}")
