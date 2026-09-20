module Codegen.Positive.Source.Expression.ArithmeticSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "function test0() {\n" ++
        "    var foo : u16 = 0 : u16;\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    foo = 1024 : u16 + foo;\n" ++
        "    foo = foo - 1024 : u16;\n" ++
        "    foo = 1024 : u16 - foo;\n" ++
        "    foo = foo * 1024 : u16;\n" ++
        "    foo = 1024 : u16 * foo;\n" ++
        "    foo = foo / 1024 : u16;\n" ++
        "    foo = 1024 : u16 / foo;\n" ++
        "    foo = foo % 1024 : u16;\n" ++
        "    foo = 1024 : u16 % foo;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "interface test_iface {\n" ++
        "    procedure test1(&mut self, foo : box u16);\n" ++
        "};\n" ++
        "\n"++
        "resource class id0 provides test_iface {\n" ++
        "    procedure test1(&mut self, foo : box u16) {\n" ++
        "        foo = foo + 1024 : u16;\n" ++
        "        foo = 1024 : u16 + foo;\n" ++
        "        foo = foo - 1024 : u16;\n" ++
        "        foo = 1024 : u16 - foo;\n" ++
        "        foo = foo * 1024 : u16;\n" ++
        "        foo = 1024 : u16 * foo;\n" ++
        "        foo = foo / 1024 : u16;\n" ++
        "        foo = 1024 : u16 / foo;\n" ++
        "        foo = foo % 1024 : u16;\n" ++
        "        foo = 1024 : u16 % foo;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

spec :: Spec
spec = do
  describe "Pretty printing arithmetic expressions" $ do
    it "Declares a function with scalar arithmetic" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void test0(void);\n" ++
              "\n" ++
              "#endif\n")
    it "Generates scalar arithmetic operations" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void test0(void) {\n" ++
              "    \n" ++
              "    uint16_t foo = 0U;\n" ++ 
              "\n" ++
              "    foo = foo + 1024U;\n" ++
              "\n" ++
              "    foo = 1024U + foo;\n" ++ 
              "\n" ++
              "    foo = foo - 1024U;\n" ++ 
              "\n" ++
              "    foo = 1024U - foo;\n" ++
              "\n" ++
              "    foo = foo * 1024U;\n" ++
              "\n" ++
              "    foo = 1024U * foo;\n" ++
              "\n" ++
              "    foo = foo / 1024U;\n" ++
              "\n" ++
              "    foo = 1024U / foo;\n" ++
              "\n" ++
              "    foo = foo % 1024U;\n" ++
              "\n" ++
              "    foo = 1024U % foo;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    
    it "Declares a procedure with box arithmetic" $ do
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
             "void id0__test1(const termina__event_t * const termina__ev,\n" ++
             "                void * const termina__this, termina__box_t foo);\n" ++
             "\n" ++
             "#endif\n")
    it "Generates arithmetic on an unboxed parameter" $ do
     renderSource test1 `shouldBe`
       pack ("\n" ++
             "#include \"test.h\"\n" ++
             "\n" ++
             "void id0__test1(const termina__event_t * const termina__ev,\n" ++
             "                void * const termina__this, termina__box_t foo) {\n" ++
             "    \n" ++
             "    id0 * self = (id0 *)termina__this;\n" ++
             "\n" ++
             "    termina__lock_t termina__lock = termina__resource__lock(&termina__ev->owner,\n" ++
             "                                                            &self->_lock_type);\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data + 1024U;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024U + *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data - 1024U;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024U - *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data * 1024U;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024U * *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data / 1024U;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024U / *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = *(uint16_t *)foo.data % 1024U;\n" ++
             "\n" ++
             "    *(uint16_t *)foo.data = 1024U % *(uint16_t *)foo.data;\n" ++
             "\n" ++
             "    termina__resource__unlock(&termina__ev->owner, &self->_lock_type,\n" ++
             "                              termina__lock);\n" ++
             "\n" ++
             "    return;\n" ++
             "\n" ++
             "}\n")
