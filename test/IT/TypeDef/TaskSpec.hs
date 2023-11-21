module IT.TypeDef.TaskSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Prettyprinter
import Modules.Printing

test0 :: String
test0 = "struct Message {\n" ++
        "    sender_id : u32;\n" ++
        "    destination_id : u32;\n" ++
        "    urgent : bool;\n" ++
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
        "            msg.urgent = false;\n" ++
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
      Right tast -> ppHeaderFile (pretty "__TEST_H__") emptyDoc tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile (pretty "test") tast

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
              "typedef struct {\n" ++
              "    uint32_t sender_id;\n" ++
              "    uint32_t destination_id;\n" ++
              "    _Bool urgent;\n" ++
              "} Message;\n" ++
              "\n" ++   
              "typedef struct {\n" ++
              "    __termina_pool_t * message_pool;\n" ++
              "    uint32_t interval;\n" ++
              "    __termina_task_t __task_id;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "TaskRet CHousekeeping__run(CHousekeeping * const self);\n" ++
              "\n" ++
              "_Bool CHousekeeping__check_interval(const CHousekeeping * const self,\n" ++
              "                                    uint32_t limit);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "TaskRet CHousekeeping__run(CHousekeeping * const self) {\n" ++
              "\n" ++
              "    TaskRet ret;\n" ++
              "\n" ++
              "    ret.__variant = TaskRet__Continue;\n" ++
              "\n" ++
              "    self->interval = self->interval + 1;\n" ++
              "\n" ++
              "    __termina_option_dyn_t alloc_msg;\n" ++
              "\n" ++
              "    alloc_msg.__variant = None;\n" ++
              "\n" ++   
              "    __termina__pool_alloc(self->message_pool, &alloc_msg);\n"  ++
              "\n"  ++   
              "    if (alloc_msg.__variant == None) {\n"  ++
              "\n"  ++   
              "        \n"  ++           
              "    } else {\n" ++
              "\n" ++   
              "        __termina_option_dyn_t __alloc_msg__Some = alloc_msg.Some.__0;\n" ++
              "\n" ++
              "        *((Message *)__alloc_msg__Some.data).urgent = 0;\n" ++
              "\n" ++
              "        __termina__pool_free(__alloc_msg__Some);\n" ++
              "\n" ++  
              "    }\n" ++
              "\n" ++
              "    _Bool check = CHousekeeping__check_interval(self, 10);\n" ++
              "\n" ++
              "    if (check == 0) {\n" ++
              "\n" ++
              "        ret.__variant = TaskRet__Abort;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++   
              "    return ret;\n" ++
              "\n" ++   
              "}\n" ++
              "\n" ++
              "\n" ++
              "_Bool CHousekeeping__check_interval(const CHousekeeping * const self,\n" ++
              "                                    uint32_t limit) {\n" ++
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
