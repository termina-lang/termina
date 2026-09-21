module Codegen.Positive.Source.Statement.AssignmentSpec (spec) where

import Codegen.Positive.Source.Common

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
  describe "Code generation for assignment statements" $ do
    it "Declares a function with a scalar assignment" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void assignment_test0(void);\n" ++
              "\n" ++
              "#endif\n")
    it "Generates a scalar assignment" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void assignment_test0(void) {\n" ++
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
    it "Declares a procedure assigning Some to an option-box" $ do
     renderHeader test1 `shouldBe`
       pack ("#ifndef TEST_H__\n" ++
             "#define TEST_H__\n" ++
             "\n" ++
             "#include <termina.h>\n" ++
             "\n" ++
             "typedef struct {\n" ++
             "    termina__resource_lock_type_t _lock_type;\n" ++
             "} id0;\n" ++
             "\n" ++
             "void id0__assignment_test1(const termina__event_t * const termina__ev,\n" ++
             "                           void * const termina__this,\n" ++
             "                           const termina__box_t box_var0);\n" ++
             "\n" ++
             "#endif\n")
    it "Generates an option-box assignment inside a locked resource" $ do
     renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__assignment_test1(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this,\n" ++
              "                           const termina__box_t box_var0) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    termina__lock_t termina__lock = termina__resource__lock(&termina__ev->owner,\n" ++
              "                                                            &self->_lock_type);\n" ++
              "\n" ++
              "    Option__box opt = { ._variant = Option__None };\n" ++
              "\n" ++
              "    opt._variant = Option__Some;\n" ++
              "    opt.Some._0 = box_var0;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type,\n" ++
              "                              termina__lock);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Declares a procedure with two box parameters" $ do
     renderHeader test2 `shouldBe`
       pack ("#ifndef TEST_H__\n" ++
             "#define TEST_H__\n" ++
             "\n" ++
             "#include <termina.h>\n" ++
             "\n" ++
             "typedef struct {\n" ++
             "    termina__resource_lock_type_t _lock_type;\n" ++
             "} id0;\n" ++
             "\n" ++
             "void id0__assignment_test2(const termina__event_t * const termina__ev,\n" ++
             "                           void * const termina__this,\n" ++
             "                           const termina__box_t box_var0,\n" ++
             "                           const termina__box_t box_var1);\n" ++
             "\n" ++
             "#endif\n")
    it "Generates assignments through unboxed box parameters" $ do
     renderSource test2 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__assignment_test2(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this,\n" ++
              "                           const termina__box_t box_var0,\n" ++
              "                           const termina__box_t box_var1) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    termina__lock_t termina__lock = termina__resource__lock(&termina__ev->owner,\n" ++
              "                                                            &self->_lock_type);\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    *(uint32_t *)box_var0.data = foo;\n" ++
              "\n" ++
              "    foo = *(uint32_t *)box_var1.data;\n" ++
              "\n" ++
              "    *(uint32_t *)box_var1.data = *(uint32_t *)box_var0.data;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type,\n" ++
              "                              termina__lock);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")