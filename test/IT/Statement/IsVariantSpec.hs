module IT.Statement.IsVariantSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "interface test_iface {\n" ++
        "    procedure match_test0(&mut self, option0 : Option<box u32>);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure match_test0(&mut self, option0 : Option<box u32>) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        if option0 is None {\n" ++
        "            foo = 1 : u32;\n" ++
        "        }\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test1 :: String
test1 = "enum Message {\n" ++
        "    In (u32, u32),\n" ++
        "    Out (u32),\n" ++
        "    Stop,\n" ++
        "    Reset\n" ++
        "};\n" ++
        "\n" ++
        "function match_test1() -> u32 {\n" ++
        "    var ret : u32 = 0 : u32;\n" ++
        "    var msg : Message = Message::In(10 : u32, 10 : u32);\n" ++
        "    if msg is Message::In {\n" ++
        "        ret = 1 : u32;\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

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
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* match_test0)(void * const, __option_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_lock_type_t __lock_type;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test0(const __termina_event_t * const __ev, void * const __this,\n" ++
              "                      __option_box_t option0);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of procedure match_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__match_test0(const __termina_event_t * const __ev, void * const __this,\n" ++
              "                      __option_box_t option0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __termina_lock_t __lock = __termina_resource__lock(&__ev->owner,\n" ++
              "                                                       &self->__lock_type);\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "        \n" ++
              "        foo = 1U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina_resource__unlock(&__ev->owner, &self->__lock_type, __lock);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of procedure match_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef enum {\n" ++
              "    Message__In,\n" ++
              "    Message__Out,\n" ++
              "    Message__Stop,\n" ++
              "    Message__Reset\n" ++
              "} __enum_Message_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "    uint32_t __1;\n" ++
              "} __enum_Message__In_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "} __enum_Message__Out_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __enum_Message_t __variant;\n" ++
              "    union {\n" ++
              "        __enum_Message__In_params_t In;\n" ++
              "        __enum_Message__Out_params_t Out;\n" ++
              "    };\n" ++
              "} Message;\n" ++
              "\n" ++
              "uint32_t match_test1();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of procedure match_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test1() {\n" ++
              "    \n" ++
              "    uint32_t ret = 0U;\n" ++
              "\n" ++
              "    Message msg;\n" ++
              "    msg.__variant = Message__In;\n" ++
              "    msg.In.__0 = 10U;\n" ++
              "    msg.In.__1 = 10U;\n" ++
              "\n" ++
              "    if (msg.__variant == Message__In) {\n" ++
              "        \n" ++
              "        ret = 1U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")