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

test3 :: String
test3 = "interface test_iface {\n" ++
        "    procedure assignment_test3(&mut self, mode : u8);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    status : loc u32;\n" ++
        "    procedure assignment_test3(&mut self, mode : u8) {\n" ++
        "        self->status = 0xDF0D : u32;\n" ++
        "        self->status = ((mode as u32) << 24 : u32) | (self->status & 0xFF : u32);\n" ++
        "        let value : u32 = 5 : u32;\n" ++
        "        self->status = value;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test4 :: String
test4 = "struct Regs {\n" ++
        "    data : u32;\n" ++
        "    tri : u32;\n" ++
        "};\n" ++
        "\n" ++
        "interface test_iface {\n" ++
        "    procedure assignment_test4(&mut self);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    regs : loc Regs;\n" ++
        "    procedure assignment_test4(&mut self) {\n" ++
        "        self->regs = {data = 1 : u32, tri = 2 : u32} : Regs;\n" ++
        "        self->regs.data = 3 : u32;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test5 :: String
test5 = "interface test_iface {\n" ++
        "    procedure assignment_test5(&mut self);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    buf : loc [u8; 4];\n" ++
        "    procedure assignment_test5(&mut self) {\n" ++
        "        self->buf = [0 : u8; 4];\n" ++
        "        self->buf[1] = 2 : u8;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test6 :: String
test6 = "struct Regs {\n" ++
        "    data : u32;\n" ++
        "    tri : u32;\n" ++
        "};\n" ++
        "\n" ++
        "enum Mode {\n" ++
        "    Off,\n" ++
        "    On(u8)\n" ++
        "};\n" ++
        "\n" ++
        "interface test_iface {\n" ++
        "    procedure assignment_test6(&mut self, regs : Regs);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    flag : loc bool;\n" ++
        "    gain : loc f32;\n" ++
        "    mode : loc Mode;\n" ++
        "    opt : loc Option<u16>;\n" ++
        "    regs : loc Regs;\n" ++
        "    matrix : loc [[u8; 4]; 2];\n" ++
        "    banks : loc [Regs; 2];\n" ++
        "    procedure assignment_test6(&mut self, regs : Regs) {\n" ++
        "        self->flag = true;\n" ++
        "        self->gain = 1.5 : f32;\n" ++
        "        self->mode = Mode::On(3 : u8);\n" ++
        "        self->opt = Some(7 : u16);\n" ++
        "        self->opt = None;\n" ++
        "        self->regs = regs;\n" ++
        "        self->matrix = [[1 : u8; 4]; 2];\n" ++
        "        self->matrix[1][2] = 5 : u8;\n" ++
        "        self->banks = [{data = 0 : u32, tri = 0xFFFFFFFF : u32} : Regs; 2];\n" ++
        "        self->banks[1].data = 9 : u32;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

spec :: Spec
spec = do
  describe "Code generation for assignments to fixed locations" $ do
    it "Declares fixed locations of every kind of field type" $ do
      renderHeader test6 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t data;\n" ++
              "    uint32_t tri;\n" ++
              "} Regs;\n" ++
              "\n" ++
              "typedef enum {\n" ++
              "    Mode__Off,\n" ++
              "    Mode__On\n" ++
              "} termina__enum__Mode_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint8_t _0;\n" ++
              "} termina__enum__Mode__On_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    termina__enum__Mode_t _variant;\n" ++
              "    termina__enum__Mode__On_params_t On;\n" ++
              "} Mode;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    termina__resource_lock_type_t _lock_type;\n" ++
              "    volatile Regs (* banks);\n" ++
              "    volatile uint8_t (* matrix)[4U];\n" ++
              "    volatile Regs * regs;\n" ++
              "    volatile Option__u16 * opt;\n" ++
              "    volatile Mode * mode;\n" ++
              "    volatile float32_t * gain;\n" ++
              "    volatile _Bool * flag;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__assignment_test6(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this, const Regs regs);\n" ++
              "\n" ++
              "#endif\n")
    it "Writes fixed locations of every kind of field type" $ do
      renderSource test6 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__assignment_test6(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this, const Regs regs) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    termina__resource__lock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    *self->flag = true;\n" ++
              "\n" ++
              "    *self->gain = 1.5f;\n" ++
              "\n" ++
              "    (*self->mode)._variant = Mode__On;\n" ++
              "    (*self->mode).On._0 = 3U;\n" ++
              "\n" ++
              "    (*self->opt)._variant = Option__Some;\n" ++
              "    (*self->opt).Some._0 = 7U;\n" ++
              "\n" ++
              "    (*self->opt)._variant = Option__None;\n" ++
              "\n" ++
              "    *self->regs = regs;\n" ++
              "\n" ++
              "    for (size_t termina__i0 = 0U;\n" ++
              "         termina__i0 < 2U;\n" ++
              "         termina__i0 = termina__i0 + 1U) {\n" ++
              "        for (size_t termina__i1 = 0U;\n" ++
              "             termina__i1 < 4U;\n" ++
              "             termina__i1 = termina__i1 + 1U) {\n" ++
              "            self->matrix[termina__i0][termina__i1] = 1U;\n" ++
              "        }\n" ++
              "    }\n" ++
              "\n" ++
              "    self->matrix[1U][2U] = 5U;\n" ++
              "\n" ++
              "    for (size_t termina__i0 = 0U;\n" ++
              "         termina__i0 < 2U;\n" ++
              "         termina__i0 = termina__i0 + 1U) {\n" ++
              "        self->banks[termina__i0].data = 0U;\n" ++
              "        self->banks[termina__i0].tri = 0xFFFFFFFFU;\n" ++
              "    }\n" ++
              "\n" ++
              "    self->banks[1U].data = 9U;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Writes a scalar fixed location" $ do
      renderSource test3 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__assignment_test3(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this, const uint8_t mode) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    termina__resource__lock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    *self->status = 0xDF0DU;\n" ++
              "\n" ++
              "    *self->status = (uint32_t)((uint32_t)mode << 24U) | (uint32_t)(*self->status & 0xFFU);\n" ++
              "\n" ++
              "    const uint32_t value = 5U;\n" ++
              "\n" ++
              "    *self->status = value;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Writes a struct fixed location, whole and by field" $ do
      renderSource test4 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__assignment_test4(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    termina__resource__lock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    (*self->regs).data = 1U;\n" ++
              "    (*self->regs).tri = 2U;\n" ++
              "\n" ++
              "    self->regs->data = 3U;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Writes an array fixed location, whole and by element" $ do
      renderSource test5 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "void id0__assignment_test5(const termina__event_t * const termina__ev,\n" ++
              "                           void * const termina__this) {\n" ++
              "    \n" ++
              "    id0 * self = (id0 *)termina__this;\n" ++
              "\n" ++
              "    termina__resource__lock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    for (size_t termina__i0 = 0U;\n" ++
              "         termina__i0 < 4U;\n" ++
              "         termina__i0 = termina__i0 + 1U) {\n" ++
              "        self->buf[termina__i0] = 0U;\n" ++
              "    }\n" ++
              "\n" ++
              "    self->buf[1U] = 2U;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
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
              "    termina__resource__lock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    Option__box opt = { ._variant = Option__None };\n" ++
              "\n" ++
              "    opt._variant = Option__Some;\n" ++
              "    opt.Some._0 = box_var0;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);\n" ++
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
              "    termina__resource__lock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    uint32_t foo = 0U;\n" ++
              "\n" ++
              "    *(uint32_t *)box_var0.data = foo;\n" ++
              "\n" ++
              "    foo = *(uint32_t *)box_var1.data;\n" ++
              "\n" ++
              "    *(uint32_t *)box_var1.data = *(uint32_t *)box_var0.data;\n" ++
              "\n" ++
              "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")