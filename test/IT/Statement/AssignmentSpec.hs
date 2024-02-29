module IT.Statement.AssignmentSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec
import Prettyprinter
import Modules.Printing
import qualified Data.Map as M

test0 :: String
test0 = "function assignment_test0() {\n" ++
        "    var foo0 : u32 = 0 : u32;\n" ++
        "    var foo1 : u32 = 0 : u32;\n" ++
        "    foo1 = foo0;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "resource class id0 {\n" ++
        "    procedure assignment_test1(&priv self, dyn_var0 : dyn u32) {\n" ++
        "        var opt : Option<dyn u32> = None;\n" ++
        "        opt = Some(dyn_var0);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test2 :: String
test2 = "resource class id0 {\n" ++
        "    procedure assignment_test2(&priv self, dyn_var0 : dyn u32, dyn_var1 : dyn u32) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        dyn_var0 = foo;\n" ++
        "        foo = dyn_var1;\n" ++
        "        dyn_var1 = dyn_var0;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test3 :: String
test3 = "resource class id0 {\n" ++
        "    procedure assignment_test3(&priv self, dyn_var0 : dyn [u32; 10],\n" ++
        "                               dyn_var1 : dyn [u32; 10]) {\n" ++
        "        var foo : [u32; 10] = [0 : u32; 10];\n" ++
        "        dyn_var0 = foo;\n" ++
        "        foo = dyn_var1;\n" ++
        "        dyn_var1 = dyn_var0;\n" ++
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
    it "Prints declaration of function assignment_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void assignment_test0();\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function assignment_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void assignment_test0() {\n" ++
              "\n" ++
              "    uint32_t foo0 = 0;\n" ++
              "\n" ++
              "    uint32_t foo1 = 0;\n" ++
              "\n" ++
              "    foo1 = foo0;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++ 
              "}\n")    
    it "Prints declaration of function assignment_test1" $ do
     renderHeader test1 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina__resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__assignment_test1(id0 * const self, __termina__dyn_t dyn_var0);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function assignment_test1" $ do
     renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__assignment_test1(id0 * const self, __termina__dyn_t dyn_var0) {\n" ++
              "\n" ++
              "    __termina__resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    __option__dyn_t opt;\n" ++
              "\n" ++
              "    opt.__variant = None;\n" ++
              "\n" ++
              "    opt.__variant = Some;\n" ++
              "    opt.Some.__0 = dyn_var0;\n" ++
              "\n" ++
              "    __termina__resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")  
    it "Prints declaration of function assignment_test2" $ do
     renderHeader test2 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina__resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__assignment_test2(id0 * const self, __termina__dyn_t dyn_var0,\n" ++
              "                           __termina__dyn_t dyn_var1);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function assignment_test2" $ do
     renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__assignment_test2(id0 * const self, __termina__dyn_t dyn_var0,\n" ++
              "                           __termina__dyn_t dyn_var1) {\n" ++
              "\n" ++
              "    __termina__resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    *((uint32_t *)dyn_var0.data) = foo;\n" ++
              "\n" ++
              "    foo = *((uint32_t *)dyn_var1.data);\n" ++
              "\n" ++
              "    *((uint32_t *)dyn_var1.data) = *((uint32_t *)dyn_var0.data);\n" ++
              "\n" ++
              "    __termina__resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")  
    it "Prints declaration of function assignment_test3" $ do
     renderHeader test3 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina__resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__assignment_test3(id0 * const self, __termina__dyn_t dyn_var0,\n" ++
              "                           __termina__dyn_t dyn_var1);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function assignment_test2" $ do
     renderSource test3 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__assignment_test3(id0 * const self, __termina__dyn_t dyn_var0,\n" ++
              "                           __termina__dyn_t dyn_var1) {\n" ++
              "\n" ++
              "    __termina__resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    uint32_t foo[10];\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        foo[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        ((uint32_t *)dyn_var0.data)[__i0] = foo[__i0];\n" ++
              "    }\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        foo[__i0] = ((uint32_t *)dyn_var1.data)[__i0];\n" ++
              "    }\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        ((uint32_t *)dyn_var1.data)[__i0] = ((uint32_t *)dyn_var0.data)[__i0];\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina__resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")  