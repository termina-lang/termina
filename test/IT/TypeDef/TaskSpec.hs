module IT.TypeDef.TaskSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text hiding (empty)

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
        "  action timeout(&priv self, current : TimeVal) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
        "\n" ++
        "    self->interval = self->interval + 1;\n" ++
        "\n" ++
        "    var alloc_msg : Option<box Message> = None;\n" ++
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

spec :: Spec
spec = do
  describe "Pretty printing class methods" $ do
    it "Prints declaration of task class CHousekeeping" $ do
      renderHeader True test0 `shouldBe`
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
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t timer;\n" ++
              "    __termina_allocator_t message_pool;\n" ++
              "    uint32_t interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "_Bool CHousekeeping__check_interval(const CHousekeeping * const self,\n" ++
              "                                    uint32_t limit);\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(void * const __this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of task class CHousekeeping" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
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
              "}\n" ++ 
              "\n" ++
              "Result CHousekeeping__timeout(void * const __this, TimeVal current) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)__this;\n" ++
              "\n" ++
              "    Result ret;\n" ++
              "    ret.__variant = Result__Ok;\n" ++
              "\n" ++
              "    self->interval = self->interval + 1U;\n" ++
              "\n" ++
              "    __option_box_t alloc_msg;\n" ++
              "    alloc_msg.__variant = None;\n" ++
              "\n" ++
              "    (self->message_pool.alloc)(self->message_pool.__that, &alloc_msg);\n"  ++
              "\n"  ++
              "    if (alloc_msg.__variant == Some) {\n"  ++
              "        \n" ++
              "        __termina_box_t msg = alloc_msg.Some.__0;\n" ++
              "\n" ++
              "        (self->message_pool.free)(self->message_pool.__that, msg);\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n"  ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    _Bool check = CHousekeeping__check_interval(self, 10U);\n" ++
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
              "void __CHousekeeping__termina_task(void * arg) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)arg;\n" ++
              "\n" ++  
              "    Status status;\n" ++
              "    status.__variant = Status__Success;\n" ++
              "\n" ++  
              "    uint32_t next_msg = 0U;\n" ++
              "\n" ++   
              "    Result result;\n" ++
              "    result.__variant = Result__Ok;\n" ++
              "\n" ++ 
              "    TimeVal timeout__msg_data;\n" ++
              "\n" ++ 
              "    for (;;) {\n" ++
              "        \n" ++  
              "        __termina_msg_queue__recv(self->__task_msg_queue_id, &next_msg,\n" ++
              "                                  &status);\n" ++
              "\n" ++  
              "        if (status.__variant != Status__Success) {\n" ++
              "            break;\n" ++
              "        }\n" ++
              "\n" ++   
              "        switch (next_msg) {\n" ++
              "            \n" ++               
              "            case __CHousekeeping__timer:\n" ++
              "\n" ++   
              "                __termina_msg_queue__recv(self->timer,\n" ++
              "                                          (void *)&timeout__msg_data, &status);\n" ++
              "\n" ++   
              "                if (status.__variant != Status__Success) {\n" ++
              "                    __termina_exec__shutdown();\n" ++
              "                }\n" ++
              "\n" ++   
              "                result = CHousekeeping__timeout(self, timeout__msg_data);\n" ++
              "\n" ++   
              "                if (result.__variant != Result__Ok) {\n" ++
              "                    __termina_exec__shutdown();\n" ++
              "                }\n" ++
              "\n" ++   
              "                break;\n" ++
              "\n" ++   
              "            default:\n" ++
              "\n" ++   
              "                __termina_exec__shutdown();\n" ++
              "\n" ++   
              "                break;\n" ++
              "\n" ++   
              "        }\n" ++
              "\n" ++   
              "    }\n" ++
              "\n" ++   
              "    __termina_exec__shutdown();\n" ++
              "\n" ++   
              "    return;\n" ++
              "\n" ++
              "}\n")
