module IT.Statement.MatchSpec (spec) where

import Test.Hspec
import PPrinter
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "function match_test0(option0 : Option<dyn u32>) -> u32 {\n" ++
        "    var ret : u32 = 0 : u32;\n" ++
        "    match option0 {\n" ++
        "        case Some(value) => {\n" ++
        "            ret = value;\n" ++
        "        }\n" ++
        "        case None => {\n" ++
        "            ret = 0 : u32;\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

test1 :: String
test1 = "function match_test1(option0 : Option<dyn u32>) -> u32 {\n" ++
        "    var ret : u32 = 0 : u32;\n" ++
        "    match option0 {\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "        case Some(value) => {\n" ++
        "            ret = value;\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

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
  describe "Pretty printing match statements" $ do
    it "Prints declaration of function match_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint32_t match_test0(__termina_option_dyn_t option0);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function match_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test0(__termina_option_dyn_t option0) {\n" ++
              "\n" ++
              "    uint32_t ret = 0;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "\n" ++
              "        ret = 0;\n" ++
              "\n" ++
              "    } else {\n" ++
              "\n" ++
              "        __termina_option_dyn_t __option0_Some = option0.Some.__0;\n" ++
              "\n" ++
              "        ret = *((uint32_t *)__option0_Some.data);\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of function match_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint32_t match_test1(__termina_option_dyn_t option0);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function match_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test1(__termina_option_dyn_t option0) {\n" ++
              "\n" ++
              "    uint32_t ret = 0;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "\n" ++
              "        \n" ++
              "    } else {\n" ++
              "\n" ++
              "        __termina_option_dyn_t __option0_Some = option0.Some.__0;\n" ++
              "\n" ++
              "        ret = *((uint32_t *)__option0_Some.data);\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
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
              "    __Message_In,\n" ++
              "    __Message_Out,\n" ++
              "    __Message_Stop,\n" ++
              "    __Message_Reset\n" ++
              "} __enum_Message_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "    uint32_t __1;\n" ++
              "} __enum_Message_In_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "} __enum_Message_Out_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "\n" ++
              "    __enum_Message_t __variant;\n" ++
              "\n" ++
              "    union {\n" ++
              "        __enum_Message_In_params_t In;\n" ++
              "        __enum_Message_Out_params_t Out;\n" ++
              "    };\n" ++
              "\n" ++
              "} Message;\n" ++
              "\n" ++
              "uint32_t match_test1();\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of function match_test1" $ do
      renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test1() {\n" ++
              "\n" ++
              "    uint32_t ret = 0;\n" ++
              "\n" ++
              "    Message msg;\n" ++
              "\n" ++
              "    msg.__variant = In;\n" ++
              "    msg.In.__0 = 10;\n" ++
              "    msg.In.__1 = 10;\n" ++
              "\n" ++
              "    if (msg.__variant == __Message_Stop) {\n" ++
              "\n" ++
              "        ret = 0;\n" ++
              "\n" ++
              "    } else if (msg.__variant == __Message_Reset) {\n" ++
              "\n" ++
              "        ret = 1;\n" ++
              "\n" ++
              "    } else if (msg.__variant == __Message_Out) {\n" ++
              "\n" ++
              "        __enum_Message_Out_params_t __msg_Out = msg.Out;\n" ++
              "\n" ++
              "        ret = __msg_Out.__0;\n" ++
              "\n" ++
              "    } else {\n" ++
              "\n" ++
              "        __enum_Message_In_params_t __msg_In = msg.In;\n" ++
              "\n" ++
              "        ret = __msg_In.__0 + __msg_In.__1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")