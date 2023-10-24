module IT.TypeDef.TaskSpec (spec) where

import Test.Hspec
import Parser.Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

test0 :: String
test0 = "enum Message {\n" ++
        "    In (u32, u32),\n" ++
        "    Out (u32),\n" ++
        "    Stop,\n" ++
        "    Reset\n" ++
        "};\n" ++
        "\n" ++
        "task class CHousekeeping {\n" ++
        "  interval : u32;\n" ++
        "  message_pool : port Pool<Message; 10>;\n" ++
        "\n" ++
        "  viewer check_interval(&self, limit : u32) -> bool {\n" ++ 
        "    var ret : bool = true;\n" ++     
        "    if (self->interval > limit) {\n" ++
        "      ret = false;\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "  method run(&priv self) -> TaskRet {\n" ++
        "\n" ++        
        "    var ret : TaskRet = TaskRet::Continue;\n" ++
        "\n" ++
        "    self->interval = self->interval + 1 : u32;\n" ++
        "\n" ++
        "    var alloc_msg : Option<dyn Message> = None;\n" ++
        "    self->message_pool.alloc(&mut alloc_msg);\n" ++
        "    match alloc_msg {\n" ++
        "        case Some(msg) => {\n" ++
        "            free(msg);\n" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "\n" ++
        "    var check : bool = (*self).check_interval(10 : u32);\n" ++
        "\n" ++
        "    if (check == false) {\n" ++
        "      ret = TaskRet::Abort;\n" ++
        "    }\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

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
  describe "Pretty printing class methods" $ do
    it "Prints declaration of class TMChannel without no_handler" $ do
      renderHeader test0 `shouldBe`
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
              "        __enum_Message_In_params_t __In;\n" ++
              "        __enum_Message_Out_params_t __Out;\n" ++
              "    };\n" ++
              "\n" ++
              "} Message;\n" ++
              "\n" ++   
              "typedef struct {\n" ++
              "    __termina_pool_t * message_pool;\n" ++
              "    uint32_t interval;\n" ++
              "    __termina_task_id_t __task_id;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "TaskRet __CHousekeeping_run(CHousekeeping * self);\n" ++
              "\n" ++
              "_Bool check_interval(CHousekeeping * self, uint32_t limit);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "TaskRet __CHousekeeping_run(CHousekeeping * self) {\n" ++
              "\n" ++
              "    TaskRet ret;\n" ++
              "\n" ++
              "    {\n" ++
              "        ret.__variant = Continue;\n" ++
              "    }\n" ++
              "\n" ++
              "    self->interval = self->interval + 1;\n" ++
              "\n" ++
              "    __termina_option_dyn_t alloc_msg;\n" ++
              "\n" ++
              "    {\n" ++
              "        alloc_msg.__variant = None;\n" ++
              "    }\n" ++ 
              "\n" ++   
              "    __termina_pool_alloc(self->message_pool, &alloc_msg);\n"  ++
              "\n"  ++   
              "    if (alloc_msg.__variant == None) {\n"  ++
              "\n"  ++   
              "        \n"  ++           
              "    } else {\n" ++
              "\n" ++   
              "        __termina_option_dyn_t __alloc_msg__Some = alloc_msg.__Some;\n" ++
              "\n" ++
              "        __termina_pool_free(__alloc_msg__Some);\n" ++
              "\n" ++  
              "    }\n" ++
              "\n" ++
              "    _Bool check = __CHousekeeping_check_interval(self, 10);\n" ++
              "\n" ++
              "    if (check == 0) {\n" ++
              "\n" ++
              "        {\n" ++
              "            ret.__variant = Abort;\n" ++
              "        }\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++   
              "    return ret;\n" ++
              "\n" ++   
              "}\n" ++
              "\n" ++
              "\n" ++
              "_Bool __CHousekeeping_check_interval(CHousekeeping * self, uint32_t limit) {\n" ++
              "\n" ++
              "    _Bool ret = 1;\n" ++
              "\n" ++
              "    if (self->interval > limit) {\n" ++
              "\n" ++
              "        ret = 0;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n\n")
