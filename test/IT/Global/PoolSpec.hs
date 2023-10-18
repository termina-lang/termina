module IT.Global.PoolSpec (spec) where

import Test.Hspec
import Parser.Parsing
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
        "resource message_pool : Pool<Message; 10>;\n"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile tast

spec :: Spec
spec = do
  describe "Pretty printing pool methods" $ do
    it "Prints declaration of Message type and external pool" $ do
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
              "extern __termina_pool_t message_pool;\n")