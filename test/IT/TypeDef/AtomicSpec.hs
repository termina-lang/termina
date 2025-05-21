module IT.TypeDef.AtomicSpec (spec) where

import IT.Common

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
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __task_id;\n" ++
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "__status_int32_t CHousekeeping__timeout(void * const __this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "__status_int32_t CHousekeeping__timeout(void * const __this, TimeVal current) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)__this;\n" ++
              "\n" ++
              "    __status_int32_t ret;\n" ++
              "    ret.__variant = Success;\n" ++
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
              "void __CHousekeeping__termina_task(void * arg) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)arg;\n" ++
              "\n" ++  
              "    int32_t status = 0L;\n" ++
              "\n" ++  
              "    __termina_id_t next_msg = 0U;\n" ++
              "\n" ++   
              "    __status_int32_t result;\n" ++
              "    result.__variant = Success;\n" ++
              "\n" ++ 
              "    TimeVal timeout__msg_data;\n" ++
              "\n" ++ 
              "    for (;;) {\n" ++
              "        \n" ++  
              "        __termina_msg_queue__recv(self->__task_msg_queue_id, &next_msg,\n" ++
              "                                  &status);\n" ++
              "\n" ++  
              "        if (status != 0L) {\n" ++
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
              "                if (status != 0L) {\n" ++
              "                    __termina_except__msg_queue_recv_error(self->timer, status);\n" ++
              "                }\n" ++
              "\n" ++   
              "                result = CHousekeeping__timeout(self, timeout__msg_data);\n" ++
              "\n" ++   
              "                if (result.__variant != Success) {\n" ++
              "                    \n" ++ 
              "                    ExceptSource source;\n" ++
              "                    source.__variant = ExceptSource__Handler;\n" ++
              "                    source.Task.__0 = self->__task_id;\n" ++
              "\n" ++
              "                    __termina_except__action_failure(source,\n" ++
              "                                                     __CHousekeeping__timer,\n" ++
              "                                                     result.Failure.__0);\n" ++
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
    it "Prints declaration of class with atomic access port" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __task_id;\n" ++
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "__status_int32_t CHousekeeping__timeout(void * const __this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "__status_int32_t CHousekeeping__timeout(void * const __this, TimeVal current) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)__this;\n" ++
              "\n" ++
              "    __status_int32_t ret;\n" ++
              "    ret.__variant = Success;\n" ++
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
              "void __CHousekeeping__termina_task(void * arg) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)arg;\n" ++
              "\n" ++  
              "    int32_t status = 0L;\n" ++
              "\n" ++  
              "    __termina_id_t next_msg = 0U;\n" ++
              "\n" ++   
              "    __status_int32_t result;\n" ++
              "    result.__variant = Success;\n" ++
              "\n" ++ 
              "    TimeVal timeout__msg_data;\n" ++
              "\n" ++ 
              "    for (;;) {\n" ++
              "        \n" ++  
              "        __termina_msg_queue__recv(self->__task_msg_queue_id, &next_msg,\n" ++
              "                                  &status);\n" ++
              "\n" ++  
              "        if (status != 0L) {\n" ++
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
              "                if (status != 0L) {\n" ++
              "                    __termina_except__msg_queue_recv_error(self->timer, status);\n" ++
              "                }\n" ++
              "\n" ++   
              "                result = CHousekeeping__timeout(self, timeout__msg_data);\n" ++
              "\n" ++   
              "                if (result.__variant != Success) {\n" ++
              "                    \n" ++ 
              "                    ExceptSource source;\n" ++
              "                    source.__variant = ExceptSource__Handler;\n" ++
              "                    source.Task.__0 = self->__task_id;\n" ++
              "\n" ++
              "                    __termina_except__action_failure(source,\n" ++
              "                                                     __CHousekeeping__timer,\n" ++
              "                                                     result.Failure.__0);\n" ++
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
