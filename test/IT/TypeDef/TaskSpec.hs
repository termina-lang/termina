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
  describe "Pretty printing class methods" $ do
    it "Prints declaration of class TMChannel without no_handler" $ do
      renderHeader test0 `shouldBe`
        pack ("typedef enum {\n" ++
              "    In,\n" ++
              "    Out,\n" ++
              "    Stop,\n" ++
              "    Reset\n" ++
              "} __enum_Message;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "\n" ++   
              "    __enum_Message __variant;\n" ++
              "    \n" ++       
              "    union {\n" ++
              "        struct {\n" ++
              "            uint32_t __0;\n" ++
              "            uint32_t __1;\n" ++
              "        } __In;\n" ++
              "        struct {\n" ++
              "            uint32_t __0;\n" ++
              "        } __Out;\n" ++
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
              "TaskRet __CHousekeeping_run(CHousekeeping * self);\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("TaskRet __CHousekeeping_run(CHousekeeping * self) {\n" ++
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
              "        __termina_pool_free(alloc_msg.__Some.__0);\n" ++
              "\n" ++  
              "    }\n" ++
              "\n" ++   
              "    return ret;\n" ++
              "\n" ++   
              "}\n\n")
