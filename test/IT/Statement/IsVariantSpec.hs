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
              "    __termina_id_t __mutex_id;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test0(void * const __this, __option_box_t option0);\n" ++
              "void id0__match_test0__mutex_lock(void * const __this, __option_box_t option0);\n" ++
              "void id0__match_test0__task_lock(void * const __this, __option_box_t option0);\n" ++
              "void id0__match_test0__event_lock(void * const __this, __option_box_t option0);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of procedure match_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__match_test0(void * const __this, __option_box_t option0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "        \n" ++
              "        foo = 1U;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++  
              "void id0__match_test0__mutex_lock(void * const __this, __option_box_t option0) {\n" ++
              "    \n" ++ 
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    int32_t __status = 0L;\n" ++
              "\n" ++
              "    __termina_mutex__lock(self->__mutex_id, &__status);\n" ++
              "    id0__match_test0(self, option0);\n" ++
              "    __termina_mutex__unlock(self->__mutex_id, &__status);\n" ++
              "\n" ++  
              "}\n" ++
              "\n" ++  
              "void id0__match_test0__task_lock(void * const __this, __option_box_t option0) {\n" ++
              "    \n" ++      
              "    __termina_task_lock_t lock;\n" ++
              "\n" ++   
              "    lock = __termina_task__lock();\n" ++
              "    id0__match_test0(__this, option0);\n" ++
              "    __termina_task__unlock(lock);\n" ++
              "\n" ++  
              "}\n" ++
              "\n" ++  
              "void id0__match_test0__event_lock(void * const __this, __option_box_t option0) {\n" ++
              "    \n" ++      
              "    __termina_event_lock_t lock;\n" ++
              "\n" ++   
              "    lock = __termina_event__lock();\n" ++
              "    id0__match_test0(__this, option0);\n" ++
              "    __termina_event__unlock(lock);\n" ++
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