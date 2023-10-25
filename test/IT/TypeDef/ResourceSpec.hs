module IT.TypeDef.ResourceSpec (spec) where

import Test.Hspec
import Parser.Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

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
              "typedef struct {\n" ++
              "    uint32_t tm_sent_packets;\n" ++
              "    __termina_resource_t __resource_id;\n" ++
              "} TMChannel;\n" ++
              "\n" ++
              "void __TMChannel_get_tm_sent_packets(TMChannel * self, uint32_t * packets);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void __TMChannel_get_tm_sent_packets(TMChannel * self, uint32_t * packets) {\n" ++
              "\n" ++
              "    __termina_resource_lock(self->__resource_id);\n" ++
              "\n" ++
              "    *packets = self->tm_sent_packets;\n" ++
              "\n" ++
              "    __termina_resource_unlock(self->__resource_id);\n" ++
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
              "    volatile uint32_t * status;\n" ++
              "    __termina_resource_t __resource_id;\n" ++
              "} UARTDriver;\n" ++
              "\n" ++
              "void __UARTDriver_get_status(UARTDriver * self, uint32_t * ret);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of class UARTDriver" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void __UARTDriver_get_status(UARTDriver * self, uint32_t * ret) {\n" ++
              "\n" ++
              "    __termina_resource_lock(self->__resource_id);\n" ++
              "\n" ++
              "    *ret = *self->status;\n" ++
              "\n" ++
              "    __termina_resource_unlock(self->__resource_id);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")