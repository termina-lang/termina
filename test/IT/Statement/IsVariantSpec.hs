module IT.Statement.IsVariantSpec (spec) where

import Test.Hspec
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Text.Parsec
import Prettyprinter
import Modules.Printing
import qualified Data.Map as M

test0 :: String
test0 = "resource class id0 {\n" ++
        "    procedure match_test0(&priv self, option0 : Option<dyn u32>) {\n" ++
        "        var foo : u32 = 0 : u32;\n" ++
        "        if option0 is None {\n" ++
        "            foo = 1 : u32;\n" ++
        "        }\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test1 :: String
test1 = "enum Message {\n" ++
        "    In (u32, u32),\n" ++
        "    Out (u32),\n" ++
        "    Stop,\n" ++
        "    Reset\n" ++
        "};\n" ++
        "\n" ++
        "function match_test1() -> u32 {\n" ++
        "    var ret : u32 = 0 : u32;\n" ++
        "    var msg : Message = Message::In(10 : u32, 10 : u32);\n" ++
        "    if msg is Message::In {\n" ++
        "        ret = 1 : u32;\n" ++
        "    }\n" ++
        "    return ret;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile False M.empty (pretty "__TEST_H__") emptyDoc tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile (pretty "test") tast

spec :: Spec
spec = do
  describe "Pretty printing match statements" $ do
    it "Prints declaration of function match_test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina__resource_t __resource;\n" ++
              "} id0;\n" ++
              "\n" ++
              "void id0__match_test0(void * const __this, __option__dyn_t option0);\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of procedure match_test0" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "void id0__match_test0(void * const __this, __option__dyn_t option0) {\n" ++
              "\n" ++
              "    id0 * self = (id0 *)__this;\n" ++
              "\n" ++
              "    __termina__resource__lock(&self->__resource);\n" ++
              "\n" ++
              "    uint32_t foo = 0;\n" ++
              "\n" ++
              "    if (option0.__variant == None) {\n" ++
              "\n" ++
              "        foo = 1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    __termina__resource__unlock(&self->__resource);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of procedure match_test1" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef enum {\n" ++
              "    Message__In,\n" ++
              "    Message__Out,\n" ++
              "    Message__Stop,\n" ++
              "    Message__Reset\n" ++
              "} __enum_Message_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "    uint32_t __1;\n" ++
              "} __enum_Message__In_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "} __enum_Message__Out_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "\n" ++
              "    __enum_Message_t __variant;\n" ++
              "\n" ++
              "    union {\n" ++
              "        __enum_Message__In_params_t In;\n" ++
              "        __enum_Message__Out_params_t Out;\n" ++
              "    };\n" ++
              "\n" ++
              "} Message;\n" ++
              "\n" ++
              "uint32_t match_test1();\n" ++
              "\n" ++
              "#endif // __TEST_H__\n")
    it "Prints definition of procedure match_test1" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++ 
              "uint32_t match_test1() {\n" ++
              "\n" ++
              "    uint32_t ret = 0;\n" ++
              "\n" ++
              "    Message msg;\n" ++
              "\n" ++
              "    msg.__variant = Message__In;\n" ++
              "    msg.In.__0 = 10;\n" ++
              "    msg.In.__1 = 10;\n" ++
              "\n" ++
              "    if (msg.__variant == Message__In) {\n" ++
              "\n" ++
              "        ret = 1;\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")