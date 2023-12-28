module IT.Expression.ArithmeticSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec
import Prettyprinter
import Modules.Printing
import qualified Data.Map as M

test0 :: String
test0 = "function test0() {\n" ++
        "    var foo : u16 = 0 : u16;\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    1024 : u16 + foo;\n" ++
        "    foo = foo - 1024 : u16;\n" ++
        "    1024 : u16 - foo;\n" ++
        "    foo = foo * 1024 : u16;\n" ++
        "    1024 : u16 * foo;\n" ++
        "    foo = foo / 1024 : u16;\n" ++
        "    1024 : u16 / foo;\n" ++
        "    foo = foo % 1024 : u16;\n" ++
        "    1024 : u16 % foo;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "resource class id0 {\n" ++
        "    procedure test1(&priv self, foo : dyn u16) {\n" ++
        "        foo = foo + 1024 : u16;\n" ++
        "        1024 : u16 + foo;\n" ++
        "        foo = foo - 1024 : u16;\n" ++
        "        1024 : u16 - foo;\n" ++
        "        foo = foo * 1024 : u16;\n" ++
        "        1024 : u16 * foo;\n" ++
        "        foo = foo / 1024 : u16;\n" ++
        "        1024 : u16 / foo;\n" ++
        "        foo = foo % 1024 : u16;\n" ++
        "        1024 : u16 % foo;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile False M.empty (pretty "__TEST_H__") emptyDoc tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile (pretty "test") tast

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
              "#endif // __TEST_H__\n")
    it "Prints definition of function test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void test0() {\n" ++
              "\n" ++
              "    uint16_t foo = 0;\n" ++ 
              "\n" ++
              "    foo = foo + 1024;\n" ++
              "\n" ++
              "    1024 + foo;\n" ++ 
              "\n" ++
              "    foo = foo - 1024;\n" ++ 
              "\n" ++
              "    1024 - foo;\n" ++
              "\n" ++
              "    foo = foo * 1024;\n" ++
              "\n" ++
              "    1024 * foo;\n" ++
              "\n" ++
              "    foo = foo / 1024;\n" ++
              "\n" ++
              "    1024 / foo;\n" ++
              "\n" ++
              "    foo = foo % 1024;\n" ++
              "\n" ++
              "    1024 % foo;\n" ++
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
              "    __termina_resource_t __resource_id;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__test1(id0 * const self, __termina_dyn_t foo);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function test1" $ do
     renderSource test1 `shouldBe`
       pack ("\n" ++
             "#include \"test.h\"\n" ++
             "\n" ++ 
             "void id0__test1(id0 * const self, __termina_dyn_t foo) {\n" ++
             "\n" ++
             "    __termina__resource_lock(&self->__resource_id);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.data) = *((uint16_t *)foo.data) + 1024;\n" ++
             "\n" ++
             "    1024 + *((uint16_t *)foo.data);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.data) = *((uint16_t *)foo.data) - 1024;\n" ++
             "\n" ++
             "    1024 - *((uint16_t *)foo.data);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.data) = *((uint16_t *)foo.data) * 1024;\n" ++
             "\n" ++
             "    1024 * *((uint16_t *)foo.data);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.data) = *((uint16_t *)foo.data) / 1024;\n" ++
             "\n" ++
             "    1024 / *((uint16_t *)foo.data);\n" ++
             "\n" ++
             "    *((uint16_t *)foo.data) = *((uint16_t *)foo.data) % 1024;\n" ++
             "\n" ++
             "    1024 % *((uint16_t *)foo.data);\n" ++
             "\n" ++
              "    __termina__resource_unlock(&self->__resource_id);\n" ++
             "\n" ++
             "    return;\n" ++
             "\n" ++
             "}\n")
