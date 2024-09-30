module IT.Expression.ArithmeticSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Monad
import Text.Parsec
import qualified Data.Map as M
import ControlFlow.BasicBlocks
import Generator.CodeGen.Module
import Generator.LanguageC.Printer

test0 :: String
test0 = "function test0() {\n" ++
        "    var foo : u16 = 0 : u16;\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    foo = 1024 : u16 + foo;\n" ++
        "    foo = foo - 1024 : u16;\n" ++
        "    foo = 1024 : u16 - foo;\n" ++
        "    foo = foo * 1024 : u16;\n" ++
        "    foo = 1024 : u16 * foo;\n" ++
        "    foo = foo / 1024 : u16;\n" ++
        "    foo = 1024 : u16 / foo;\n" ++
        "    foo = foo % 1024 : u16;\n" ++
        "    foo = 1024 : u16 % foo;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "interface test_iface {\n" ++
        "    procedure test1(&mut self, foo : box u16);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure test1(&mut self, foo : box u16) {\n" ++
        "        foo = foo + 1024 : u16;\n" ++
        "        foo = 1024 : u16 + foo;\n" ++
        "        foo = foo - 1024 : u16;\n" ++
        "        foo = 1024 : u16 - foo;\n" ++
        "        foo = foo * 1024 : u16;\n" ++
        "        foo = 1024 : u16 * foo;\n" ++
        "        foo = foo / 1024 : u16;\n" ++
        "        foo = 1024 : u16 / foo;\n" ++
        "        foo = foo % 1024 : u16;\n" ++
        "        foo = 1024 : u16 % foo;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

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
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints declaration of function test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void test0();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void test0() {\n" ++
              "    \n" ++
              "    uint16_t foo = 0;\n" ++ 
              "\n" ++
              "    foo = foo + 1024;\n" ++
              "\n" ++
              "    foo = 1024 + foo;\n" ++ 
              "\n" ++
              "    foo = foo - 1024;\n" ++ 
              "\n" ++
              "    foo = 1024 - foo;\n" ++
              "\n" ++
              "    foo = foo * 1024;\n" ++
              "\n" ++
              "    foo = 1024 * foo;\n" ++
              "\n" ++
              "    foo = foo / 1024;\n" ++
              "\n" ++
              "    foo = 1024 / foo;\n" ++
              "\n" ++
              "    foo = foo % 1024;\n" ++
              "\n" ++
              "    foo = 1024 % foo;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function test1" $ do
     renderHeader test1 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* test1)(void * const, __termina_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__test1(void * const __this, __termina_box_t foo);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test1" $ do
     renderSource test1 `shouldBe`
       pack ("\n" ++
             "#include \"test.h\"\n" ++
             "\n" ++ 
             "void id0__test1(void * const __this, __termina_box_t foo) {\n" ++
             "    \n" ++
             "    id0 * self = (id0 *)__this;\n" ++
             "\n" ++
             "    __termina_resource__lock(&self->__resource);\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data + 1024;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024 + *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data - 1024;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024 - *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data * 1024;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024 * *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data / 1024;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024 / *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data % 1024;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024 % *(uint16_t *)foo.data;\n" ++
             "\n" ++
              "    __termina_resource__unlock(&self->__resource);\n" ++
             "\n" ++
             "    return;\n" ++
             "\n" ++
             "}\n")
