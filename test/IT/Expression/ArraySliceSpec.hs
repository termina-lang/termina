module IT.Expression.ArraySliceSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec
import qualified Data.Map as M
import Control.Monad.Reader
import Generator.Module
import Generator.LanguageC.Printer
import System.Path
import Modules.Modules

test0 :: String
test0 = "function add_one(input : &mut [u32; 5]) {\n" ++ 
        "    for i : usize in 0 : usize .. 5 : usize {\n" ++
        "        (*input)[i] = (*input)[i] + 1 : u32;\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "function slice_test0(vector0 : &mut [[u32; 10]; 5]) {\n" ++
        "    add_one(&mut(*vector0)[0][0..6]);\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "function add_two(input : &[u32; 5]) -> [u32; 5] {\n" ++ 
        "    var output : [u32; 5] = [0; 5];\n" ++
        "    for i : usize in 0 : usize .. 5 : usize {\n" ++
        "        output[i] = (*input)[i] + 2;\n" ++
        "    }\n" ++
        "    return output;\n" ++
        "}\n" ++
        "\n" ++
        "function slice_test1(vector0 : & [[u32; 10]; 5]) {\n" ++
        "    var foo : [u32; 5] = [0; 5];\n" ++
        "    foo = add_two(&(*vector0)[2][2..8]);\n" ++
        "    return;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> 
        case runReaderT (genHeaderFile False (fragment "test") SrcFile [] tast) M.empty of
          Left err -> pack $ show err
          Right cHeaderFile -> render $ runReader (pprint cHeaderFile) (CPrinterConfig False False)

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> 
        case runReaderT (genSourceFile (fragment "test") tast) M.empty of
          Left err -> pack $ show err
          Right cHeaderFile -> render $ runReader (pprint cHeaderFile) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing vector slicing expressions" $ do
    it "Prints declaration of function slice_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void add_one(uint32_t input[5]);\n" ++
              "\n" ++
              "void slice_test0(uint32_t vector0[5][10]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function slice_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void add_one(uint32_t input[5]) {\n" ++
              "    \n" ++
              "    for (size_t i = 0; i < 5; i = i + 1) {\n" ++
              "        \n" ++
              "        input[i] = input[i] + 1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void slice_test0(uint32_t vector0[5][10]) {\n" ++
              "    \n" ++
              "    add_one(&vector0[0][0]);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function slice_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "__wrapper_uint32__5_t add_two(const uint32_t input[5]);\n" ++
              "\n" ++
              "void slice_test1(const uint32_t vector0[5][10]);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function slice_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "__wrapper_uint32__5_t add_two(const uint32_t input[5]) {\n" ++
              "    \n" ++
              "    uint32_t output[5];\n" ++
              "    for (size_t __i0 = 0; __i0 < 5; __i0 = __i0 + 1) {\n" ++
              "        output[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    for (size_t i = 0; i < 5; i = i + 1) {\n" ++
              "        \n" ++
              "        output[i] = input[i] + 2;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return *(__wrapper_uint32__5_t *)output;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void slice_test1(const uint32_t vector0[5][10]) {\n" ++
              "    \n" ++
              "    uint32_t foo[5];\n" ++
              "    for (size_t __i0 = 0; __i0 < 5; __i0 = __i0 + 1) {\n" ++
              "        foo[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    *(__wrapper_uint32__5_t *)foo = *(__wrapper_uint32__5_t *)add_two(&vector0[2][2]).array;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n") 