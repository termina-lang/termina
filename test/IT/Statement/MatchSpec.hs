module IT.Statement.MatchSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Monad
import Text.Parsec
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.Common
import Control.Monad.Except

test0 :: String
test0 = "interface test_iface {\n" ++
        "    procedure match_test0(&mut self, option0 : Option<box u32>);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure match_test0(&mut self, option0 : Option<box u32>) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        match option0 {\n" ++
        "            case Some(value) => {\n" ++
        "                foo = value;\n" ++
        "            }\n" ++
        "            case None => {\n" ++
        "                foo = 0 : u32;\n" ++
        "            }\n" ++
        "        }\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test1 :: String
test1 = "interface test_iface {\n" ++
        "    procedure match_test1(&mut self, option0 : Option<box u32>);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure match_test1(&mut self, option0 : Option<box u32>) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        match option0 {\n" ++
        "            case None => {\n" ++
        "            }\n" ++
        "            case Some(value) => {\n" ++
        "                foo = value;\n" ++
        "            }\n" ++
        "        }\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test2 :: String
test2 = "enum Message {\n" ++
        "    In (u32, u32),\n" ++
        "    Out (u32),\n" ++
        "    Stop,\n" ++
        "    Reset\n" ++
        "};\n" ++
        "\n" ++
        "function match_test1() -> u32 {\n" ++
        "    var ret : u32 = 0 : u32;\n" ++
        "    var msg : Message = Message::In(10 : u32, 10 : u32);\n" ++
        "    match msg {\n" ++
        "        case In(param0, param1) => {\n" ++
        "            ret = param0 + param1;\n" ++
        "        }\n" ++
        "        case Out(result) => {\n" ++
        "            ret = result;\n" ++
        "        }\n" ++
        "        case Stop => {\n" ++
        "            ret = 0 : u32;\n" ++
        "        }\n" ++
        "        case Reset => {\n" ++
        "            ret = 1 : u32;\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runExcept (genBBModule tast) of
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
        case runExcept (genBBModule tast) of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile "test" bbAST of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter cSourceFile

spec :: Spec
spec = do
  describe "Pretty printing match statements" $ do
    it "Prints declaration of function match_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* match_test0)(void * const, __option_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test0(void * const __this, __option_box_t option0);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of procedure match_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__match_test0(void * const __this, __option_box_t option0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __termina_resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "        \n" ++
              "        foo = 0;\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        __option_box_params_t __Some = option0.Some;\n" ++
              "\n" ++
              "        foo = *(uint32_t *)__Some.__0.data;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina_resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of procedure match_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* match_test1)(void * const, __option_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++              
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test1(void * const __this, __option_box_t option0);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of procedure match_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__match_test1(void * const __this, __option_box_t option0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __termina_resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "        \n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        __option_box_params_t __Some = option0.Some;\n" ++
              "\n" ++
              "        foo = *(uint32_t *)__Some.__0.data;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina_resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function match_test2" $ do
      renderHeader test2 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef enum {\n" ++
              "    Message__In,\n" ++
              "    Message__Out,\n" ++
              "    Message__Stop,\n" ++
              "    Message__Reset\n" ++
              "} __enum_Message_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "    uint32_t __1;\n" ++
              "} __enum_Message__In_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "} __enum_Message__Out_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __enum_Message_t __variant;\n" ++
              "    union {\n" ++
              "        __enum_Message__In_params_t In;\n" ++
              "        __enum_Message__Out_params_t Out;\n" ++
              "    };\n" ++
              "} Message;\n" ++
              "\n" ++
              "uint32_t match_test1();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function match_test1" $ do
      renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test1() {\n" ++
              "    \n" ++
              "    uint32_t ret = 0;\n" ++
              "\n" ++
              "    Message msg;\n" ++
              "    msg.__variant = Message__In;\n" ++
              "    msg.In.__0 = 10;\n" ++
              "    msg.In.__1 = 10;\n" ++
              "\n" ++
              "    if (msg.__variant == Message__Stop) {\n" ++
              "        \n" ++
              "        ret = 0;\n" ++
              "\n" ++
              "    } else if (msg.__variant == Message__Reset) {\n" ++
              "        \n" ++
              "        ret = 1;\n" ++
              "\n" ++
              "    } else if (msg.__variant == Message__Out) {\n" ++
              "        \n" ++
              "        __enum_Message__Out_params_t __Out = msg.Out;\n" ++
              "\n" ++
              "        ret = __Out.__0;\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        __enum_Message__In_params_t __In = msg.In;\n" ++
              "\n" ++
              "        ret = __In.__0 + __In.__1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")