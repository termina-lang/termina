module IT.Expression.ArithmeticSpec (spec) where

import IT.Common

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
    it "Prints declaration of function test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void test0();\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void test0() {\n" ++
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
    it "Prints declaration of function test1" $ do
     renderHeader False test1 `shouldBe`
       pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    void * __that;\n" ++
              "    void (* test1)(void * const, __termina_box_t);\n" ++
              "} test_iface;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __mutex_id;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__test1(void * const __this, __termina_box_t foo);\n" ++
              "void id0__test1__mutex_lock(void * const __this, __termina_box_t foo);\n" ++
              "void id0__test1__task_lock(void * const __this, __termina_box_t foo);\n" ++
              "void id0__test1__event_lock(void * const __this, __termina_box_t foo);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function test1" $ do
     renderSource test1 `shouldBe`
       pack ("\n" ++
             "#include \"test.h\"\n" ++
             "\n" ++ 
             "void id0__test1(void * const __this, __termina_box_t foo) {\n" ++
             "    \n" ++
             "    id0 * self = (id0 *)__this;\n" ++
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
             "    return;\n" ++
             "\n" ++
             "}\n")
