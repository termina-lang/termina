module IT.TypeDef.TaskSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import qualified Data.Map as M
import Control.Monad.Reader
import Generator.Module
import Generator.LanguageC.Printer
import System.Path
import Modules.Modules

test0 :: String
test0 = "struct Message {\n" ++
        "    sender_id : u32;\n" ++
        "    destination_id : Option<u32>;\n" ++
        "    urgent : bool;\n" ++
        "};\n" ++
        "\n" ++
        "task class CHousekeeping {\n" ++
        "  interval : u32;\n" ++
        "  message_pool : access Allocator<Message>;\n" ++
        "  timer : sink TimeVal triggers timeout;\n" ++
        "\n" ++
        "  viewer check_interval(&self, limit : u32) -> bool {\n" ++
        "    var ret : bool = true;\n" ++
        "    if (self->interval > limit) {\n" ++
        "      ret = false;\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "  action timeout(&mut self, current : TimeVal) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
        "\n" ++
        "    self->interval = self->interval + 1;\n" ++
        "\n" ++
        "    var alloc_msg : Option<dyn Message> = None;\n" ++
        "    self->message_pool.alloc(&mut alloc_msg);\n" ++
        "    match alloc_msg {\n" ++
        "        case Some (msg) => {\n" ++
        "            self->message_pool.free(msg);\n" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "\n" ++
        "    var check : bool = self->check_interval(10);\n" ++
        "\n" ++
        "    if (check == false) {\n" ++
        "      ret = Result::Error;\n" ++
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
      Right tast -> 
        case runReaderT (genHeaderFile True (fragment "test") SrcFile [] tast) M.empty of
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
  describe "Pretty printing class methods" $ do
    it "Prints declaration of class TMChannel without no_handler" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t sender_id;\n" ++
              "    __option_uint32_t destination_id;\n" ++
              "    _Bool urgent;\n" ++
              "} Message;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_task_t __task;\n" ++
              "    __termina_sink_port_t timer;\n" ++
              "    __termina_pool_t * message_pool;\n" ++
              "    uint32_t interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(CHousekeeping * const self, TimeVal current);\n" ++
              "\n" ++
              "_Bool CHousekeeping__check_interval(const CHousekeeping * const self,\n" ++
              "                                    uint32_t limit);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(CHousekeeping * const self, TimeVal current) {\n" ++
              "    \n" ++
              "    Result ret;\n" ++
              "    ret.__variant = Result__Ok;\n" ++
              "\n" ++
              "    self->interval = self->interval + 1;\n" ++
              "\n" ++
              "    __option_dyn_t alloc_msg;\n" ++
              "    alloc_msg.__variant = None;\n" ++
              "\n" ++
              "    __termina_pool__alloc(self->message_pool, &alloc_msg);\n"  ++
              "\n"  ++
              "    if (alloc_msg.__variant == None) {\n"  ++
              "        \n"  ++
              "\n"  ++
              "    } else {\n" ++
              "        \n" ++
              "        __option_dyn_params_t __Some = alloc_msg.Some;\n" ++
              "\n" ++
              "        __termina_pool__free(self->message_pool, __Some.__0);\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    _Bool check = CHousekeeping__check_interval(self, 10);\n" ++
              "\n" ++
              "    if (check == 0) {\n" ++
              "        \n" ++
              "        ret.__variant = Result__Error;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "_Bool CHousekeeping__check_interval(const CHousekeeping * const self,\n" ++
              "                                    uint32_t limit) {\n" ++
              "    \n" ++
              "    _Bool ret = 1;\n" ++
              "\n" ++
              "    if (self->interval > limit) {\n" ++
              "        \n" ++
              "        ret = 0;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")
