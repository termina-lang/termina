module IT.Expression.BitwiseSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text

test0 :: String
test0 = "function bitwise_test0(foo : u16) {\n" ++
        "    var bar8 : u8 = 0;\n" ++
        "    var bar16 : u16 = 0;\n" ++
        "    bar16 = foo << 8 : u8;\n" ++
        "    bar8 = 8 : u8 << foo;\n" ++
        "    bar16 = foo >> 8 : u8;\n" ++
        "    bar8 = 8 : u8 << foo;\n" ++
        "    bar16 = foo & 1024: u16;\n" ++
        "    bar16 = 1024 : u16 & foo;\n" ++
        "    bar16 = foo | 1024;\n" ++
        "    bar16 = 1024 : u16 | foo;\n" ++
        "    bar16 = foo ^ 1024: u16;\n" ++
        "    bar16 = 1024 : u16 ^ foo;\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Pretty printing bitwise shifting expressions" $ do
    it "Prints declaration of function bitwise_test0" $ do
      renderHeader False test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "void bitwise_test0(uint16_t foo);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of function bitwise_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void bitwise_test0(uint16_t foo) {\n" ++
              "    \n" ++
              "    uint8_t bar8 = 0;\n" ++ 
              "\n" ++
              "    uint16_t bar16 = 0;\n" ++ 
              "\n" ++
              "    bar16 = foo << 8;\n" ++
              "\n" ++
              "    bar8 = 8 << foo;\n" ++ 
              "\n" ++
              "    bar16 = foo >> 8;\n" ++ 
              "\n" ++
              "    bar8 = 8 << foo;\n" ++
              "\n" ++
              "    bar16 = foo & 1024;\n" ++
              "\n" ++
              "    bar16 = 1024 & foo;\n" ++
              "\n" ++
              "    bar16 = foo | 1024;\n" ++
              "\n" ++
              "    bar16 = 1024 | foo;\n" ++
              "\n" ++
              "    bar16 = foo ^ 1024;\n" ++
              "\n" ++
              "    bar16 = 1024 ^ foo;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    