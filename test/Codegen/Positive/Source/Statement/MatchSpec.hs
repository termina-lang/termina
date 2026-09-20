module Codegen.Positive.Source.Statement.MatchSpec (spec) where

import Codegen.Positive.Source.Common

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
        "        match option0 {\n" ++
        "            case Some(value) => {\n" ++
        "                foo = value;\n" ++
        "            }\n" ++
        "            case None => {\n" ++
        "                foo = 0 : u32;\n" ++
        "            }\n" ++
        "        }\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test1 :: String
test1 = "interface test_iface {\n" ++
        "    procedure match_test1(&mut self, option0 : Option<box u32>);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure match_test1(&mut self, option0 : Option<box u32>) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        match option0 {\n" ++
        "            case None => {\n" ++
        "            }\n" ++
        "            case Some(value) => {\n" ++
        "                foo = value;\n" ++
        "            }\n" ++
        "        }\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test2 :: String
test2 = "enum Message {\n" ++
        "    In (u32, u32),\n" ++
        "    Out (u32),\n" ++
        "    Stop,\n" ++
        "    Reset\n" ++
        "};\n" ++
        "\n" ++
        "function match_test1() -> u32 {\n" ++
        "    var ret : u32 = 0 : u32;\n" ++
        "    var msg : Message = Message::In(10 : u32, 10 : u32);\n" ++
        "    match msg {\n" ++
        "        case In(param0, param1) => {\n" ++
        "            ret = param0 + param1;\n" ++
        "        }\n" ++
        "        case Out(result) => {\n" ++
        "            ret = result;\n" ++
        "        }\n" ++
        "        case Stop => {\n" ++
        "            ret = 0 : u32;\n" ++
        "        }\n" ++
        "        case Reset => {\n" ++
        "            ret = 1 : u32;\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

test3 :: String
test3 = "enum Sample {\n" ++
        "    Pair (u8, u32),\n" ++
        "    Empty\n" ++
        "};\n" ++
        "\n" ++
        "function match_test3(s : Sample) -> u32 {\n" ++
        "    var ret : u32;\n" ++
        "    match s {\n" ++
        "        case Pair(_a, b) => {\n" ++
        "            ret = b;\n" ++
        "        }\n" ++
        "        case Empty => {\n" ++
        "            ret = 0 : u32;\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Code generation for match statements" $ do
    it "Declares a procedure matching an option-box" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_lock_type_t _lock_type;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test0(const __termina_event_t * const termina__ev,\n" ++
              "                      void * const termina__this, __option_box_t option0);\n" ++
              "\n" ++
              "#endif\n")
    it "Generates an option-box match (Some before None)" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__match_test0(const __termina_event_t * const termina__ev,\n" ++
              "                      void * const termina__this, __option_box_t option0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    __termina_lock_t termina__lock = __termina_resource__lock(&termina__ev->owner,\n" ++
              "                                                              &self->_lock_type);\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    if (option0._variant == Some) {\n" ++
              "        \n" ++
              "        __termina_box_t value = option0.Some._0;\n" ++
              "\n" ++
              "        foo = *(uint32_t *)value.data;\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        foo = 0U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina_resource__unlock(&termina__ev->owner, &self->_lock_type,\n" ++
              "                               termina__lock);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Declares a procedure matching an option-box with reversed cases" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_resource_lock_type_t _lock_type;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test1(const __termina_event_t * const termina__ev,\n" ++
              "                      void * const termina__this, __option_box_t option0);\n" ++
              "\n" ++
              "#endif\n")
    it "Generates an option-box match (None before Some)" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__match_test1(const __termina_event_t * const termina__ev,\n" ++
              "                      void * const termina__this, __option_box_t option0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    __termina_lock_t termina__lock = __termina_resource__lock(&termina__ev->owner,\n" ++
              "                                                              &self->_lock_type);\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    if (option0._variant == None) {\n" ++
              "        \n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        __termina_box_t value = option0.Some._0;\n" ++
              "\n" ++
              "        foo = *(uint32_t *)value.data;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina_resource__unlock(&termina__ev->owner, &self->_lock_type,\n" ++
              "                               termina__lock);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Declares a function matching an enum" $ do
      renderHeader test2 `shouldBe`
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
              "    uint32_t _0;\n" ++
              "    uint32_t _1;\n" ++
              "} __enum_Message__In_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t _0;\n" ++
              "} __enum_Message__Out_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __enum_Message_t _variant;\n" ++
              "    union {\n" ++
              "        __enum_Message__In_params_t In;\n" ++
              "        __enum_Message__Out_params_t Out;\n" ++
              "    };\n" ++
              "} Message;\n" ++
              "\n" ++
              "uint32_t match_test1(void);\n" ++
              "\n" ++
              "#endif\n")
    it "Generates an enum match with four variants" $ do
      renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test1(void) {\n" ++
              "    \n" ++
              "    uint32_t ret = 0U;\n" ++
              "\n" ++
              "    Message msg = { ._variant = Message__In, .In = { ._0 = 10U, ._1 = 10U } };\n" ++
              "\n" ++
              "    if (msg._variant == Message__In) {\n" ++
              "        \n" ++
              "        uint32_t param0 = msg.In._0;\n" ++
              "        uint32_t param1 = msg.In._1;\n" ++
              "\n" ++
              "        ret = param0 + param1;\n" ++
              "\n" ++
              "    } else if (msg._variant == Message__Out) {\n" ++
              "        \n" ++
              "        uint32_t result = msg.Out._0;\n" ++
              "\n" ++
              "        ret = result;\n" ++
              "\n" ++
              "    } else if (msg._variant == Message__Stop) {\n" ++
              "        \n" ++
              "        ret = 0U;\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        ret = 1U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")
    it "Reads each case parameter from its own position when an earlier one is ignored" $ do
      renderSource test3 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "uint32_t match_test3(Sample s) {\n" ++
              "    \n" ++
              "    uint32_t ret;\n" ++
              "\n" ++
              "    if (s._variant == Sample__Pair) {\n" ++
              "        \n" ++
              "        uint32_t b = s.Pair._1;\n" ++
              "\n" ++
              "        ret = b;\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "        ret = 0U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")