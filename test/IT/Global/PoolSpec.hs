module IT.Global.PoolSpec (spec) where

import Test.Hspec
import Parsing
import PPrinter
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking

test0 :: String
test0 = "enum Message {\n" ++
        "    In (u32, u32),\n" ++
        "    Out (u32),\n" ++
        "    Stop,\n" ++
        "    Reset\n" ++
        "};\n" ++
        "\n" ++
        "protected message_pool : Pool<Message; 10>;\n" ++
        "\n" ++
        "fn test0(in0 : u32, in1 : u32) {\n" ++
        "    var alloc_msg : Option<'dyn Message> = None;\n" ++
        "    message_pool.alloc(&alloc_msg);\n" ++
        "    match alloc_msg {\n" ++
        "        case Some(msg) => {\n" ++
        "            msg = Message::In(in0, in1);\n" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return;\n" ++
        "}"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile tast

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppSourceFile tast

spec :: Spec
spec = do
  describe "Pretty printing pool methods" $ do
    it "Prints declaration of function test0" $ do
      renderHeader test0 `shouldBe`
        pack ("typedef enum {\n" ++
              "    In,\n" ++
              "    Out,\n" ++
              "    Stop,\n" ++
              "    Reset\n" ++
              "} __enum_Message;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "\n" ++
              "    __enum_Message __variant;\n" ++
              "    \n" ++
              "    union {\n" ++
              "        struct {\n" ++
              "            uint32_t __0;\n" ++
              "            uint32_t __1;\n" ++
              "        } __In;\n" ++
              "        struct {\n" ++
              "            uint32_t __0;\n" ++
              "        } __Out;\n" ++
              "    };\n" ++
              "\n" ++
              "} Message;\n" ++
              "\n" ++
              "extern __termina_pool_t message_pool;\n" ++
              "\n" ++
              "void test0(uint32_t in0, uint32_t in1);\n")
    it "Prints definition of functions test0" $ do
      renderSource test0 `shouldBe`
        pack ("void test0(uint32_t in0, uint32_t in1) {\n" ++
              "    \n" ++
              "    __Option_dyn_t alloc_msg;\n" ++
              "\n" ++
              "    {\n" ++
              "        alloc_msg.__variant = None;\n" ++
              "    }\n" ++
              "\n" ++
              "    __pool_alloc(&message_pool, &alloc_msg);\n" ++
              "\n" ++
              "    if (alloc_msg.__variant == None) {\n" ++
              "\n" ++
              "        \n" ++
              "    } else {\n" ++
              "\n" ++
              "        {\n" ++
              "            *((Message *)alloc_msg.__Some.__0.datum).__variant = In;\n" ++
              "            *((Message *)alloc_msg.__Some.__0.datum).__In.__0 = in0;\n" ++
              "            *((Message *)alloc_msg.__Some.__0.datum).__In.__1 = in1;\n" ++
              "        }\n" ++
              "\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")    