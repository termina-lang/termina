module IT.TypeDef.AtomicSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "  interval : access AtomicAccess<u32>;\n" ++
        "  timer : sink TimeVal triggers timeout;\n" ++
        "\n" ++
        "  action timeout(&priv self, current : TimeVal) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
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
        "  action timeout(&priv self, current : TimeVal) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
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
      renderHeader True test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(void * const __this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(void * const __this, TimeVal current) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)__this;\n" ++
              "\n" ++
              "    Result ret;\n" ++
              "    ret.__variant = Result__Ok;\n" ++
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
              "    Status status;\n" ++
              "    status.__variant = Status__Success;\n" ++
              "\n" ++  
              "    uint32_t next_msg = 0U;\n" ++
              "\n" ++   
              "    size_t size = 0U;\n" ++
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
    it "Prints declaration of class with atomic access port" $ do
      renderHeader True test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(void * const __this, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(void * const __this, TimeVal current) {\n" ++
              "    \n" ++
              "    CHousekeeping * self = (CHousekeeping *)__this;\n" ++
              "\n" ++
              "    Result ret;\n" ++
              "    ret.__variant = Result__Ok;\n" ++
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
              "    Status status;\n" ++
              "    status.__variant = Status__Success;\n" ++
              "\n" ++  
              "    uint32_t next_msg = 0U;\n" ++
              "\n" ++   
              "    size_t size = 0U;\n" ++
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
