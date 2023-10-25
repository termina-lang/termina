module IT.Expression.VectorIndexSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "function vector_test0() {\n" ++
        "    var foo : usize = 0 : usize;\n" ++
        "    var vector0 : [u32; 10] = [0 : u32; 10];\n" ++
        "    var vector1 : [[i64; 5]; 10] = [[0 : i64; 5]; 10];\n" ++
        "    vector0[3 : usize] = 10 : u32;\n" ++
        "    vector0[foo] = 1024 : u32;\n" ++
        "    vector1[3 : usize][4 : usize] = 1024 : i64;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "function vector_test1(p_vector0 : & [u32; 10]) {\n" ++
        "    var foo : u32 = 0 : u32;\n" ++
        "    (*p_vector0)[3 : usize] = 10 : u32;\n" ++
        "    (*p_vector0)[foo as usize] = 1024 : u32;\n" ++
        "    return;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile [pack "test"] [] tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile [pack "test"] tast

spec :: Spec
spec = do
  describe "Pretty printing vector index expressions" $ do
    it "Prints declaration of function vector_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void vector_test0();\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function vector_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void vector_test0() {\n" ++
              "\n" ++
              "    size_t foo = 0;\n" ++
              "\n" ++
              "    uint32_t vector0[10];\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        vector0[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    int64_t vector1[10][5];\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
              "            vector1[__i0][__i1] = 0;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    vector0[3] = 10;\n" ++
              "\n" ++
              "    vector0[foo] = 1024;\n" ++
              "\n" ++
              "    vector1[3][4] = 1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Prints declaration of function vector_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void vector_test1(uint32_t p_vector0[10]);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function vector_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void vector_test1(uint32_t p_vector0[10]) {\n" ++
              "\n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    p_vector0[3] = 10;\n" ++
              "\n" ++
              "    p_vector0[(size_t)foo] = 1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")   