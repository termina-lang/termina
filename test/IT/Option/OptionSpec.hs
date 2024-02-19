module IT.Option.OptionSpec (spec) where

import Test.Hspec ( shouldBe, it, describe, Spec )
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Prettyprinter
import Modules.Printing
import qualified Data.Map as M
import qualified Data.Set as S
import AST.Core
import Semantic.Option
import PPrinter.Application.Option
import PPrinter

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "\n" ++
        "  method run(&priv self) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
        "\n" ++
        "    var opt : Option<u32> = None;\n" ++
        "\n" ++
        "    opt = Some(0 : u32);\n" ++
        "\n" ++
        "    match opt {\n" ++
        "        case Some(integer) => {\n" ++
        "            integer = integer + 1 : u32\n;" ++
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
        "            value[0 : usize] = 1 : u32;\n" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}"

test0OptionMap :: OptionMap
test0OptionMap = M.fromList [(UInt32, S.fromList [Option UInt32])]

test1OptionMap :: OptionMap
test1OptionMap = M.fromList [(UInt32, S.fromList [Option (Vector UInt32 (K 10))])]

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast ->
        ppHeaderFile True test0OptionMap (pretty "__TEST_H__")
          emptyDoc
          tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile (pretty "test") tast

renderOption :: OptionMap -> Text
renderOption = render . ppSimpleOptionTypesFile

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
              "    __termina_task_t __task_id;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "Result CHousekeeping__run(CHousekeeping * const self);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
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
              "\n" ++
              "    __option_uint32_params_t Some;\n" ++
              "\n" ++
              "    __enum_option_t __variant;\n" ++
              "\n" ++
              "} __option_uint32_t;\n" ++
              "\n" ++
              "#endif // __OPTION_H__\n")
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
              "\n" ++
              "    __option_uint32__10_params_t Some;\n" ++
              "\n" ++
              "    __enum_option_t __variant;\n" ++
              "\n" ++
              "} __option_uint32__10_t;\n" ++
              "\n" ++
              "#endif // __OPTION_H__\n")
    it "Prints definition of function test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void test1() {\n" ++
              "\n" ++
              "    uint32_t foo[10];\n" ++
              "\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        foo[__i0] = 0;\n" ++
              "    }\n" ++
              "\n" ++
              "    __option_uint32__10_t optionFoo;\n" ++
              "\n" ++
              "    optionFoo.__variant = None;\n" ++
              "\n" ++
              "    optionFoo.__variant = Some;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        optionFoo.Some.__0[__i0] = foo[__i0];\n" ++
              "    }\n" ++
              "\n" ++   
              "    if (optionFoo.__variant == None) {\n" ++
              "\n" ++
              "        \n" ++
              "    } else {\n" ++
              "\n" ++   
              "        __option_dyn_t __optionFoo__Some = optionFoo.Some.__0;\n" ++
              "\n" ++
              "        __optionFoo__Some[0] = 1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")