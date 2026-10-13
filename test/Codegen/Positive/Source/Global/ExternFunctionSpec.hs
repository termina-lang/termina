module Codegen.Positive.Source.Global.ExternFunctionSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "extern function crc16(data : &[u8; 16], len : usize) -> u16;\n" ++
        "\n" ++
        "function checksum(data : &[u8; 16]) -> u16 {\n" ++
        "    let crc : u16 = crc16(data, 16);\n" ++
        "    return crc;\n" ++
        "}"

test1 :: String
test1 = "extern noreturn function jump_to_image(entry : usize);"

test2 :: String
test2 = "extern noreturn function jump_to_image(entry : usize);\n" ++
        "\n" ++
        "handler class Loader {\n" ++
        "    snk : sink usize triggers load;\n" ++
        "    action load(&priv self, entry : usize) -> Status<i32> {\n" ++
        "        jump_to_image(entry);\n" ++
        "    }\n" ++
        "};"

spec :: Spec
spec = do
  describe "Extern functions" $ do
    it "Declares a noreturn extern function with _Noreturn" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "_Noreturn void jump_to_image(const size_t entry);\n" ++
              "\n" ++
              "#endif\n")
    it "Stops the system before a call to a noreturn function" $ do
      renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Status__i32 Loader__load(const termina__event_t * const termina__ev,\n" ++
              "                         void * const termina__this, const size_t entry) {\n" ++
              "    \n" ++
              "    (void)termina__ev;\n" ++
              "\n" ++
              "    Loader * self = (Loader *)termina__this;\n" ++
              "\n" ++
              "    termina__exec__handover();\n" ++
              "    jump_to_image(entry);\n" ++
              "\n" ++
              "}\n")
    it "Declares the extern function in the header like a function" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "uint16_t crc16(const uint8_t data[16U], const size_t len);\n" ++
              "\n" ++
              "uint16_t checksum(const uint8_t data[16U]);\n" ++
              "\n" ++
              "#endif\n")
    it "Defines nothing for the extern function in the source" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "uint16_t checksum(const uint8_t data[16U]) {\n" ++
              "    \n" ++
              "    const uint16_t crc = crc16(data, 16U);\n" ++
              "\n" ++
              "    return crc;\n" ++
              "\n" ++
              "}\n")
