module Codegen.Positive.Source.TypeDef.TaskSpec (spec) where

import Codegen.Positive.Source.Common

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
        "  action timeout(&priv self, current : TimeVal) -> Status<i32> {\n" ++
        "\n" ++
        "    var ret : Status<i32> = Success;\n" ++
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
        "      ret = Failure(-1);\n" ++
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
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t sender_id;\n" ++
              "    Option__u32 destination_id;\n" ++
              "    _Bool urgent;\n" ++
              "} Message;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t _task_id;\n" ++
              "    __termina_id_t _task_msg_queue_id;\n" ++
              "    __termina_id_t timer;\n" ++
              "    __termina_allocator_t message_pool;\n" ++
              "    uint32_t interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "Status__i32 CHousekeeping__timeout(const __termina_event_t * const termina__ev,\n" ++
              "                                   void * const termina__this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of task class CHousekeeping" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "static _Bool CHousekeeping__check_interval(const __termina_event_t * const termina__ev,\n" ++
              "                                           const CHousekeeping * const self,\n" ++
              "                                           uint32_t limit);\n" ++
              "\n" ++
              "static _Bool CHousekeeping__check_interval(const __termina_event_t * const termina__ev,\n" ++
              "                                           const CHousekeeping * const self,\n" ++
              "                                           uint32_t limit) {\n" ++
              "    \n" ++
              "    (void)termina__ev;\n" ++
              "\n" ++
              "    _Bool ret = true;\n" ++
              "\n" ++
              "    if (self->interval > limit) {\n" ++
              "        \n" ++
              "        ret = false;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "Status__i32 CHousekeeping__timeout(const __termina_event_t * const termina__ev,\n" ++
              "                                   void * const termina__this,\n" ++
              "                                   TimeVal current) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)termina__this;\n" ++
              "\n" ++
              "    Status__i32 ret = { ._variant = Status__Success };\n" ++
              "\n" ++
              "    self->interval = self->interval + 1U;\n" ++
              "\n" ++
              "    Option__box alloc_msg = { ._variant = Option__None };\n" ++
              "\n" ++
              "    self->message_pool.alloc(termina__ev, self->message_pool._that, &alloc_msg);\n" ++
              "\n" ++
              "    if (alloc_msg._variant == Option__Some) {\n" ++
              "        \n" ++
              "        __termina_box_t msg = alloc_msg.Some._0;\n" ++
              "\n" ++
              "        self->message_pool.free(termina__ev, self->message_pool._that, msg);\n" ++
              "\n" ++
              "    } else {\n" ++
              "        \n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    _Bool check = CHousekeeping__check_interval(termina__ev, self, 10U);\n" ++
              "\n" ++
              "    if (check == false) {\n" ++
              "        \n" ++
              "        ret._variant = Status__Failure;\n" ++
              "        ret.Failure._0 = -(1L);\n" ++
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
              "    int32_t status = 0L;\n" ++
              "\n" ++
              "    __termina_event_t event;\n" ++
              "\n" ++
              "    Status__i32 result;\n" ++
              "\n" ++
              "    TimeVal timeout__msg_data;\n" ++
              "\n" ++
              "    for (;;) {\n" ++
              "        \n" ++
              "        __termina_msg_queue__recv(self->_task_msg_queue_id, &event, &status);\n" ++
              "\n" ++
              "        if (status != 0L) {\n" ++
              "            break;\n" ++
              "        }\n" ++
              "\n" ++
              "        switch (event.port_id) {\n" ++
              "            \n" ++
              "            case __CHousekeeping__timer:\n" ++
              "\n" ++
              "                __termina_msg_queue__recv(self->timer,\n" ++
              "                                          (void *)&timeout__msg_data, &status);\n" ++
              "\n" ++
              "                if (status != 0L) {\n" ++
              "                    __termina_except__msg_queue_recv_error(self->timer, status);\n" ++
              "                }\n" ++
              "\n" ++
              "                result = CHousekeeping__timeout(&event, self,\n" ++
              "                                                timeout__msg_data);\n" ++
              "\n" ++
              "                if (result._variant != Status__Success) {\n" ++
              "                    \n" ++
              "                    ExceptSource source;\n" ++
              "                    source._variant = ExceptSource__Task;\n" ++
              "                    source.Task._0 = self->_task_id;\n" ++
              "\n" ++
              "                    __termina_except__action_failure(source,\n" ++
              "                                                     __CHousekeeping__timer,\n" ++
              "                                                     result.Failure._0);\n" ++
              "\n" ++
              "                }\n" ++
              "\n" ++
              "                break;\n" ++
              "\n" ++
              "            default:\n" ++
              "\n" ++
              "                __termina_exec__reboot();\n" ++
              "\n" ++
              "                break;\n" ++
              "\n" ++
              "        }\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
