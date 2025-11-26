module IT.TypeDef.ResourceSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

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
              "    __termina_resource_lock_type_t __lock_type;\n" ++
              "    uint32_t tm_sent_packets;\n" ++
              "} TMChannel;\n" ++
              "\n" ++
              "void TMChannel__get_tm_sent_packets(const __termina_event_t * const __ev,\n" ++
              "                                    void * const __this,\n" ++
              "                                    uint32_t * const packets);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class TMChannel without no_handler" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void TMChannel__get_tm_sent_packets(const __termina_event_t * const __ev,\n" ++
              "                                    void * const __this,\n" ++
              "                                    uint32_t * const packets) {\n" ++
              "    \n" ++
              "    TMChannel * self = (TMChannel *)__this;\n" ++
              "\n" ++
              "    __termina_lock_t __lock = __termina_resource__lock(&__ev->owner,\n" ++
              "                                                       &self->__lock_type);\n" ++
              "\n" ++
              "    *packets = self->tm_sent_packets;\n" ++
              "\n" ++
              "    __termina_resource__unlock(&__ev->owner, &self->__lock_type, __lock);\n" ++
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
              "    __termina_resource_lock_type_t __lock_type;\n" ++
              "    volatile uint32_t * status;\n" ++
              "} UARTDriver;\n" ++
              "\n" ++
              "void UARTDriver__get_status(const __termina_event_t * const __ev,\n" ++
              "                            void * const __this, uint32_t * const ret);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class UARTDriver" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void UARTDriver__get_status(const __termina_event_t * const __ev,\n" ++
              "                            void * const __this, uint32_t * const ret) {\n" ++
              "    \n" ++
              "    UARTDriver * self = (UARTDriver *)__this;\n" ++
              "\n" ++
              "    __termina_lock_t __lock = __termina_resource__lock(&__ev->owner,\n" ++
              "                                                       &self->__lock_type);\n" ++
              "\n" ++
              "    *ret = *self->status;\n" ++
              "\n" ++
              "    __termina_resource__unlock(&__ev->owner, &self->__lock_type, __lock);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n");