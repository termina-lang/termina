module IT.Option.OptionSpec (spec) where

import Test.Hspec ( shouldBe, it, describe, Spec )
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Semantic.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Core.AST
import Generator.Option
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import Generator.CodeGen.Application.Option

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "\n" ++
        "  method run(&mut self) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
        "\n" ++
        "    var opt : Option<u32> = None;\n" ++
        "\n" ++
        "    opt = Some(0 : u32);\n" ++
        "\n" ++
        "    match opt {\n" ++
        "        case Some(integer) => {\n" ++
        "            let foo : u32 = integer + 1 : u32\n;" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test1 :: String
test1 = "function test1() {\n" ++
        "    var foo : [u32; 10] = [0 : u32; 10];\n" ++
        "    var optionFoo : Option<[u32; 10]> = None;\n" ++
        "    optionFoo = Some(foo);\n" ++
        "    match optionFoo {\n" ++
        "        case Some(value) => {\n" ++
        "            foo[0] = value[0];\n" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}"

test0OptionMap :: OptionMap
test0OptionMap = M.fromList [(UInt32, S.fromList [Option UInt32])]

test1OptionMap :: OptionMap
test1OptionMap = M.fromList [(UInt32, S.fromList [Option (Array UInt32 (K (TInteger 10 DecRepr)))])]

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenHeaderFile True "test" [] tast M.empty of
          Left err -> pack $ show err
          Right cHeaderFile -> runCPrinter cHeaderFile

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenSourceFile "test" tast of
          Left err -> pack $ show err
          Right cSourceFile -> runCPrinter cSourceFile

renderOption :: OptionMap -> Text
renderOption optionMap =   case runGenOptionHeaderFile optionMap of
    Left err -> pack $ show err
    Right cOptionsFile -> runCPrinter cOptionsFile

spec :: Spec
spec = do
  describe "Pretty printing pool methods" $ do
    it "Prints header file of test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_task_t __task;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "Result CHousekeeping__run(CHousekeeping * const self);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints option header file of test0" $ do
      renderOption test0OptionMap `shouldBe`
        pack ("#ifndef __OPTION_H__\n" ++
              "#define __OPTION_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "} __option_uint32_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __option_uint32_params_t Some;\n" ++
              "    __enum_option_t __variant;\n" ++
              "} __option_uint32_t;\n" ++
              "\n" ++
              "#endif\n")
    it "Prints option header file of test1" $ do
      renderOption test1OptionMap `shouldBe`
        pack ("#ifndef __OPTION_H__\n" ++
              "#define __OPTION_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0[10];\n" ++
              "} __option_uint32__10_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __option_uint32__10_params_t Some;\n" ++
              "    __enum_option_t __variant;\n" ++
              "} __option_uint32__10_t;\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void test1() {\n" ++
              "    \n" ++
              "    uint32_t foo[10];\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        foo[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    __option_uint32__10_t optionFoo;\n" ++
              "    optionFoo.__variant = None;\n" ++
              "\n" ++
              "    optionFoo.__variant = Some;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        optionFoo.Some.__0[__i0] = foo[__i0];\n" ++
              "    }\n" ++
              "\n" ++   
              "    if (optionFoo.__variant == None) {\n" ++
              "        \n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++   
              "        __option_uint32__10_params_t __Some = optionFoo.Some;\n" ++
              "\n" ++
              "        foo[0] = __Some.__0[0];\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")