module Codegen.Positive.Source.TypeDef.AtomicSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "  interval : access AtomicAccess<u32>;\n" ++
        "  timer : sink TimeVal triggers timeout;\n" ++
        "\n" ++
        "  action timeout(&priv self, current : TimeVal) -> Status<i32> {\n" ++
        "\n" ++
        "    var ret : Status<i32> = Success;\n" ++
        "    var local : u32 = 0;\n" ++
        "\n" ++
        "    self->interval.store(32);\n" ++
        "    self->interval.load(&mut local);\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test1 :: String
test1 = "task class CHousekeeping {\n" ++
        "  interval : access AtomicArrayAccess<u32; 10>;\n" ++
        "  timer : sink TimeVal triggers timeout;\n" ++
        "\n" ++
        "  action timeout(&priv self, current : TimeVal) -> Status<i32> {\n" ++
        "\n" ++
        "    var ret : Status<i32> = Success;\n" ++
        "    var local : u32 = 0;\n" ++
        "\n" ++
        "    self->interval.store_index(0, 32);\n" ++
        "    self->interval.load_index(1, &mut local);\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

spec :: Spec
spec = do
  describe "Classes with atomic access ports" $ do
    it "Prints declaration of class with atomic access port" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    termina__id_t _task_id;\n" ++
              "    termina__id_t _task_msg_queue_id;\n" ++
              "    termina__id_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void termina__task_entry__CHousekeeping(void * const arg);\n" ++
              "\n" ++
              "Status__i32 CHousekeeping__timeout(const termina__event_t * const termina__ev,\n" ++
              "                                   void * const termina__this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Status__i32 CHousekeeping__timeout(const termina__event_t * const termina__ev,\n" ++
              "                                   void * const termina__this,\n" ++
              "                                   TimeVal current) {\n" ++
              "    \n" ++
              "    (void)termina__ev;\n" ++
              "\n" ++
              "    CHousekeeping * self = (CHousekeeping *)termina__this;\n" ++
              "\n" ++
              "    Status__i32 ret = { ._variant = Status__Success };\n" ++
              "\n" ++
              "    uint32_t local = 0U;\n" ++
              "\n" ++
              "    atomic_store(self->interval, 32U);\n" ++
              "\n" ++
              "    local = atomic_load(self->interval);\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void termina__task_entry__CHousekeeping(void * arg) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)arg;\n" ++
              "\n" ++
              "    int32_t status = 0L;\n" ++
              "\n" ++
              "    termina__event_t event;\n" ++
              "\n" ++
              "    Status__i32 result;\n" ++
              "\n" ++
              "    TimeVal timeout__msg_data;\n" ++
              "\n" ++
              "    for (;;) {\n" ++
              "        \n" ++
              "        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);\n" ++
              "\n" ++
              "        if (status != 0L) {\n" ++
              "            break;\n" ++
              "        }\n" ++
              "\n" ++
              "        switch (event.port_id) {\n" ++
              "            \n" ++
              "            case CHousekeeping__timer:\n" ++
              "\n" ++
              "                termina__msg_queue__recv(self->timer,\n" ++
              "                                         (void *)&timeout__msg_data, &status);\n" ++
              "\n" ++
              "                if (status != 0L) {\n" ++
              "                    termina__except__msg_queue_recv_error(self->timer, status);\n" ++
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
              "                    termina__except__action_failure(source,\n" ++
              "                                                    CHousekeeping__timer,\n" ++
              "                                                    result.Failure._0);\n" ++
              "\n" ++
              "                }\n" ++
              "\n" ++
              "                break;\n" ++
              "\n" ++
              "            default:\n" ++
              "\n" ++
              "                termina__exec__reboot();\n" ++
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
    it "Prints declaration of class with atomic access port" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    termina__id_t _task_id;\n" ++
              "    termina__id_t _task_msg_queue_id;\n" ++
              "    termina__id_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void termina__task_entry__CHousekeeping(void * const arg);\n" ++
              "\n" ++
              "Status__i32 CHousekeeping__timeout(const termina__event_t * const termina__ev,\n" ++
              "                                   void * const termina__this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Status__i32 CHousekeeping__timeout(const termina__event_t * const termina__ev,\n" ++
              "                                   void * const termina__this,\n" ++
              "                                   TimeVal current) {\n" ++
              "    \n" ++
              "    (void)termina__ev;\n" ++
              "\n" ++
              "    CHousekeeping * self = (CHousekeeping *)termina__this;\n" ++
              "\n" ++
              "    Status__i32 ret = { ._variant = Status__Success };\n" ++
              "\n" ++
              "    uint32_t local = 0U;\n" ++
              "\n" ++
              "    atomic_store(&self->interval[0U], 32U);\n" ++
              "\n" ++
              "    local = atomic_load(&self->interval[1U]);\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void termina__task_entry__CHousekeeping(void * arg) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)arg;\n" ++
              "\n" ++
              "    int32_t status = 0L;\n" ++
              "\n" ++
              "    termina__event_t event;\n" ++
              "\n" ++
              "    Status__i32 result;\n" ++
              "\n" ++
              "    TimeVal timeout__msg_data;\n" ++
              "\n" ++
              "    for (;;) {\n" ++
              "        \n" ++
              "        termina__msg_queue__recv(self->_task_msg_queue_id, &event, &status);\n" ++
              "\n" ++
              "        if (status != 0L) {\n" ++
              "            break;\n" ++
              "        }\n" ++
              "\n" ++
              "        switch (event.port_id) {\n" ++
              "            \n" ++
              "            case CHousekeeping__timer:\n" ++
              "\n" ++
              "                termina__msg_queue__recv(self->timer,\n" ++
              "                                         (void *)&timeout__msg_data, &status);\n" ++
              "\n" ++
              "                if (status != 0L) {\n" ++
              "                    termina__except__msg_queue_recv_error(self->timer, status);\n" ++
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
              "                    termina__except__action_failure(source,\n" ++
              "                                                    CHousekeeping__timer,\n" ++
              "                                                    result.Failure._0);\n" ++
              "\n" ++
              "                }\n" ++
              "\n" ++
              "                break;\n" ++
              "\n" ++
              "            default:\n" ++
              "\n" ++
              "                termina__exec__reboot();\n" ++
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
