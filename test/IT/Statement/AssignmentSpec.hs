module IT.Statement.AssignmentSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "function assignment_test0() {\n" ++
        "    var foo0 : u32 = 0:u32;\n" ++
        "    var foo1 : u32 = 0:u32;\n" ++
        "    foo1 = foo0;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "interface test_iface {\n" ++
        "    procedure assignment_test1(&mut self, box_var0 : box u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure assignment_test1(&mut self, box_var0 : box u32) {\n" ++
        "        var opt : Option<box u32> = None;\n" ++
        "        opt = Some(box_var0);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test2 :: String
test2 = "interface test_iface {\n" ++
        "    procedure assignment_test2(&mut self, box_var0 : box u32, box_var1 : box u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure assignment_test2(&mut self, box_var0 : box u32, box_var1 : box u32) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        box_var0 = foo;\n" ++
        "        foo = box_var1;\n" ++
        "        box_var1 = box_var0;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

spec :: Spec
spec = do
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints declaration of function assignment_test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void assignment_test0();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function assignment_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void assignment_test0() {\n" ++
              "    \n" ++
              "    uint32_t foo0 = 0U;\n" ++
              "\n" ++
              "    uint32_t foo1 = 0U;\n" ++
              "\n" ++
              "    foo1 = foo0;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++ 
              "}\n")    
    it "Prints declaration of function assignment_test1" $ do
     renderHeader False test1 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* assignment_test1)(void * const, __termina_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __mutex_id;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__assignment_test1(void * const __this, __termina_box_t box_var0);\n" ++
              "void id0__assignment_test1__mutex_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0);\n" ++
              "void id0__assignment_test1__task_lock(void * const __this,\n" ++
              "                                      __termina_box_t box_var0);\n" ++
              "void id0__assignment_test1__event_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function assignment_test1" $ do
     renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__assignment_test1(void * const __this, __termina_box_t box_var0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __option_box_t opt;\n" ++
              "    opt.__variant = None;\n" ++
              "\n" ++
              "    opt.__variant = Some;\n" ++
              "    opt.Some.__0 = box_var0;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++  
              "void id0__assignment_test1__mutex_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0) {\n" ++
              "    \n" ++ 
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __status_int32_t status;\n" ++
              "    status.__variant = Success;\n" ++
              "\n" ++
              "    __termina_mutex__lock(self->__mutex_id, &status);\n" ++
              "    id0__assignment_test1(self, box_var0);\n" ++
              "    __termina_mutex__unlock(self->__mutex_id, &status);\n" ++
              "\n" ++  
              "}\n" ++
              "\n" ++  
              "void id0__assignment_test1__task_lock(void * const __this,\n" ++
              "                                      __termina_box_t box_var0) {\n" ++
              "    \n" ++      
              "    __termina_task_lock_t lock;\n" ++
              "\n" ++   
              "    lock = __termina_task__lock();\n" ++
              "    id0__assignment_test1(__this, box_var0);\n" ++
              "    __termina_task__unlock(lock);\n" ++
              "\n" ++  
              "}\n" ++
              "\n" ++  
              "void id0__assignment_test1__event_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0) {\n" ++
              "    \n" ++      
              "    __termina_event_lock_t lock;\n" ++
              "\n" ++   
              "    lock = __termina_event__lock();\n" ++
              "    id0__assignment_test1(__this, box_var0);\n" ++
              "    __termina_event__unlock(lock);\n" ++
              "\n" ++  
              "}\n")
    it "Prints declaration of function assignment_test2" $ do
     renderHeader False test2 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* assignment_test2)(void * const, __termina_box_t, __termina_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __mutex_id;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__assignment_test2(void * const __this, __termina_box_t box_var0,\n" ++
              "                           __termina_box_t box_var1);\n" ++
              "void id0__assignment_test2__mutex_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0,\n" ++
              "                                       __termina_box_t box_var1);\n" ++
              "void id0__assignment_test2__task_lock(void * const __this,\n" ++
              "                                      __termina_box_t box_var0,\n" ++
              "                                      __termina_box_t box_var1);\n" ++
              "void id0__assignment_test2__event_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0,\n" ++
              "                                       __termina_box_t box_var1);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function assignment_test2" $ do
     renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__assignment_test2(void * const __this, __termina_box_t box_var0,\n" ++
              "                           __termina_box_t box_var1) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    *(uint32_t *)box_var0.data = foo;\n" ++
              "\n" ++
              "    foo = *(uint32_t *)box_var1.data;\n" ++
              "\n" ++
              "    *(uint32_t *)box_var1.data = *(uint32_t *)box_var0.data;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++ 
              "\n" ++  
              "void id0__assignment_test2__mutex_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0,\n" ++
              "                                       __termina_box_t box_var1) {\n" ++
              "    \n" ++ 
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __status_int32_t status;\n" ++
              "    status.__variant = Success;\n" ++
              "\n" ++
              "    __termina_mutex__lock(self->__mutex_id, &status);\n" ++
              "    id0__assignment_test2(self, box_var0, box_var1);\n" ++
              "    __termina_mutex__unlock(self->__mutex_id, &status);\n" ++
              "\n" ++  
              "}\n" ++
              "\n" ++  
              "void id0__assignment_test2__task_lock(void * const __this,\n" ++
              "                                      __termina_box_t box_var0,\n" ++
              "                                      __termina_box_t box_var1) {\n" ++
              "    \n" ++      
              "    __termina_task_lock_t lock;\n" ++
              "\n" ++   
              "    lock = __termina_task__lock();\n" ++
              "    id0__assignment_test2(__this, box_var0, box_var1);\n" ++
              "    __termina_task__unlock(lock);\n" ++
              "\n" ++  
              "}\n" ++
              "\n" ++  
              "void id0__assignment_test2__event_lock(void * const __this,\n" ++
              "                                       __termina_box_t box_var0,\n" ++
              "                                       __termina_box_t box_var1) {\n" ++
              "    \n" ++      
              "    __termina_event_lock_t lock;\n" ++
              "\n" ++   
              "    lock = __termina_event__lock();\n" ++
              "    id0__assignment_test2(__this, box_var0, box_var1);\n" ++
              "    __termina_event__unlock(lock);\n" ++
              "\n" ++  
              "}\n")