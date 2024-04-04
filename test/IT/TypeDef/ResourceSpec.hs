module IT.TypeDef.ResourceSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import qualified Data.Map as M
import Control.Monad.Reader
import Generator.Module
import PPrinter
import Generator.LanguageC.Printer
import System.Path
import Modules.Modules

test0 :: String
test0 = "resource class TMChannel {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&priv self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test1 :: String
test1 = "resource class UARTDriver {\n" ++
        "  status : loc u32;\n" ++
        "\n" ++
        "  procedure get_status(&priv self, ret : &mut u32) {\n" ++
        "    *ret = self->status;\n" ++
        "    return;\n" ++
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
  describe "Pretty printing class methods" $ do
    it "Prints declaration of class TMChannel without no_handler" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "    uint32_t tm_sent_packets;\n" ++
              "} TMChannel;\n" ++
              "\n" ++
              "void TMChannel__get_tm_sent_packets(void * const __this, uint32_t * packets);\n" ++
              "\n" ++
              "#endif")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void TMChannel__get_tm_sent_packets(void * const __this, uint32_t * packets) {\n" ++
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
              "}")
    it "Prints declaration of class UARTDriver" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_t __resource;\n" ++
              "    volatile uint32_t * status;\n" ++
              "} UARTDriver;\n" ++
              "\n" ++
              "void UARTDriver__get_status(void * const __this, uint32_t * ret);\n" ++
              "\n" ++
              "#endif")
    it "Prints definition of class UARTDriver" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void UARTDriver__get_status(void * const __this, uint32_t * ret) {\n" ++
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
              "}")