module IT.Statement.AssignmentSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn assignment_test0() {\n" ++
        "    var foo0 : u32 = 0 : u32;\n" ++
        "    var foo1 : u32 = 0 : u32;\n" ++
        "    foo1 = foo0;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "fn assignment_test1(dyn_var0 : 'dyn u32) {\n" ++
        "    var option : Option<'dyn u32> = None;\n" ++
        "    option = Some(dyn_var0);\n" ++
        "    return;\n" ++
        "}"

test2 :: String
test2 = "fn assignment_test2(dyn_var0 : 'dyn u32, dyn_var1 : 'dyn u32) {\n" ++
        "    var foo : u32 = 0 : u32;\n" ++
        "    dyn_var0 = foo;\n" ++
        "    foo = dyn_var1;\n" ++
        "    dyn_var1 = dyn_var0;\n" ++
        "    return;\n" ++
        "}"

test3 :: String
test3 = "fn assignment_test3(dyn_var0 : 'dyn [u32; 10 : u32], dyn_var1 : 'dyn [u32; 10 : u32]) {\n" ++
        "    var foo : [u32; 10 : u32] = [0 : u32; 10 : u32];\n" ++
        "    dyn_var0 = foo;\n" ++
        "    foo = dyn_var1;\n" ++
        "    dyn_var1 = dyn_var0;\n" ++
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
    it "Prints declaration of function assignment_test0" $ do
      renderHeader test0 `shouldBe`
        pack "void assignment_test0();\n"
    it "Prints definition of function assignment_test0" $ do
      renderSource test0 `shouldBe`
        pack ("void assignment_test0() {\n" ++
              "    \n" ++
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
       pack "void assignment_test1(__termina_dyn_t dyn_var0);\n"
    it "Prints definition of function assignment_test1" $ do
     renderSource test1 `shouldBe`
        pack ("void assignment_test1(__termina_dyn_t dyn_var0) {\n" ++
              "    \n" ++
              "    __termina_option_dyn_t option;\n" ++
              "\n" ++
              "    {\n" ++
              "        option.__variant = None;\n" ++
              "    }\n" ++
              "\n" ++
              "    {\n" ++
              "        option.__variant = Some;\n" ++
              "        option.__Some.__0 = dyn_var0;\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")  
    it "Prints declaration of function assignment_test2" $ do
     renderHeader test2 `shouldBe`
       pack "void assignment_test2(__termina_dyn_t dyn_var0, __termina_dyn_t dyn_var1);\n"
    it "Prints definition of function assignment_test2" $ do
     renderSource test2 `shouldBe`
        pack ("void assignment_test2(__termina_dyn_t dyn_var0, __termina_dyn_t dyn_var1) {\n" ++
              "    \n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    *((uint32_t *)dyn_var0.data) = foo;\n" ++
              "\n" ++
              "    foo = *((uint32_t *)dyn_var1.data);\n" ++
              "\n" ++
              "    *((uint32_t *)dyn_var1.data) = *((uint32_t *)dyn_var0.data);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")  
    it "Prints declaration of function assignment_test3" $ do
     renderHeader test3 `shouldBe`
       pack "void assignment_test3(__termina_dyn_t dyn_var0, __termina_dyn_t dyn_var1);\n"
    it "Prints definition of function assignment_test2" $ do
     renderSource test3 `shouldBe`
        pack ("void assignment_test3(__termina_dyn_t dyn_var0, __termina_dyn_t dyn_var1) {\n" ++
              "    \n" ++
              "    uint32_t foo[10];\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            foo[__i0] = 0;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            ((uint32_t *)dyn_var0.data)[__i0] = foo[__i0];\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            foo[__i0] = ((uint32_t *)dyn_var1.data)[__i0];\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            ((uint32_t *)dyn_var1.data)[__i0] = ((uint32_t *)dyn_var0.data)[__i0];\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")  