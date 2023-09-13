module IT.Expression.VectorIndexSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn vector_test0() {\n" ++
        "    var foo : u32 = 0 : u32;\n" ++
        "    var vector0 : [u32; 10 : u32] = [0 : u32; 10 : u32];\n" ++
        "    var vector1 : [[i64; 5 : u32]; 10 : u16] = [[0 : i64; 5 : u32]; 10 : u16];\n" ++
        "    vector0[3 : u32] = 10 : u32;\n" ++
        "    vector0[foo] = 1024 : u32;\n" ++
        "    vector1[3 : u16][4 : u32] = 1024 : i64;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "fn vector_test1(p_vector0 : & [u32; 10 : u32]) {\n" ++
        "    var foo : u32 = 0 : u32;\n" ++
        "    (*p_vector0)[3 : u32] = 10 : u32;\n" ++
        "    (*p_vector0)[foo] = 1024 : u32;\n" ++
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
  describe "Pretty printing vector index expressions" $ do
    it "Prints declaration of function vector_test0" $ do
      renderHeader test0 `shouldBe`
        pack "void vector_test0();\n"
    it "Prints definition of function vector_test0" $ do
      renderSource test0 `shouldBe`
        pack ("void vector_test0() {\n" ++
              "    \n" ++
              "    uint32_t foo = (uint32_t)0;\n" ++
              "\n" ++
              "    uint32_t vector0[10];\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
              "            vector0[__i0] = (uint32_t)0;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    int64_t vector1[10][5];\n" ++
              "\n" ++
              "    {\n" ++
              "        for (uint16_t __i0 = 0; __i0 < (uint16_t)10; __i0 = __i0 + (uint16_t)1) {\n" ++
              "            for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
              "                vector1[__i0][__i1] = (int64_t)0;\n" ++
              "            }\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    vector0[(uint32_t)3] = (uint32_t)10;\n" ++
              "\n" ++
              "    vector0[foo] = (uint32_t)1024;\n" ++
              "\n" ++
              "    vector1[(uint16_t)3][(uint32_t)4] = (int64_t)1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function vector_test1" $ do
      renderHeader test1 `shouldBe`
        pack "void vector_test1(uint32_t p_vector0[10]);\n"
    it "Prints definition of function vector_test1" $ do
      renderSource test1 `shouldBe`
        pack ("void vector_test1(uint32_t p_vector0[10]) {\n" ++
              "    \n" ++
              "    uint32_t foo = (uint32_t)0;\n" ++
              "\n" ++
              "    (p_vector0)[(uint32_t)3] = (uint32_t)10;\n" ++
              "\n" ++
              "    (p_vector0)[foo] = (uint32_t)1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")   