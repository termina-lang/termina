module IT.TypeDef.ResourceSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Semantic.Monad
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.BasicBlocks

test0 :: String
test0 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test1 :: String
test1 = "interface UARTDriverInterface {\n" ++
        "    procedure get_status(&mut self, ret : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class UARTDriver provides UARTDriverInterface {\n" ++
        "  status : loc u32;\n" ++
        "\n" ++
        "  procedure get_status(&mut self, ret : &mut u32) {\n" ++
        "    *ret = self->status;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
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
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile "test" bbAST of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter cSourceFile

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
              "    void * __that;\n" ++
              "    void (* get_tm_sent_packets)(void * const, uint32_t * const);\n" ++
              "} TMChannelInterface;\n" ++
              "\n" ++              
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "    uint32_t tm_sent_packets;\n" ++
              "} TMChannel;\n" ++
              "\n" ++
              "void TMChannel__get_tm_sent_packets(void * const __this,\n" ++
              "                                    uint32_t * const packets);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void TMChannel__get_tm_sent_packets(void * const __this,\n" ++
              "                                    uint32_t * const packets) {\n" ++
              "    \n" ++
              "    TMChannel * self = (TMChannel *)__this;\n" ++
              "\n" ++
              "    __termina_resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    *packets = self->tm_sent_packets;\n" ++
              "\n" ++
              "    __termina_resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of class UARTDriver" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* get_status)(void * const, uint32_t * const);\n" ++
              "} UARTDriverInterface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "    volatile uint32_t * status;\n" ++
              "} UARTDriver;\n" ++
              "\n" ++
              "void UARTDriver__get_status(void * const __this, uint32_t * const ret);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class UARTDriver" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void UARTDriver__get_status(void * const __this, uint32_t * const ret) {\n" ++
              "    \n" ++
              "    UARTDriver * self = (UARTDriver *)__this;\n" ++
              "\n" ++
              "    __termina_resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    *ret = *self->status;\n" ++
              "\n" ++
              "    __termina_resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")