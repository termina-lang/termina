module IT.Option.OptionSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text
import qualified Data.Set as S
import qualified Data.Map as M
import Core.AST
import Generator.Monadic

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "\n" ++
        "  data_in : sink u32 triggers action0;\n" ++
        "\n" ++
        "  action action0(&priv self, _data : u32) -> Status<i32> {\n" ++
        "\n" ++
        "    var ret : Status<i32> = Success;\n" ++
        "    var opt : Option<u32> = None;\n" ++
        "\n" ++
        "    opt = Some(0 : u32);\n" ++
        "\n" ++
        "    match opt {\n" ++
        "        case Some(integer) => {\n" ++
        "            let foo : u32 = integer + 1 : u32\n;" ++
        "        }\n" ++
        "        case None => {\n" ++
        "        }\n" ++
        "    }\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test0OptionMap :: MonadicTypes
test0OptionMap = MonadicTypes {
  optionTypes = S.fromList [TUInt32],
  statusTypes = S.empty,
  resultTypes = M.empty,
  generatedTypes = M.empty
}

spec :: Spec
spec = do
  describe "Pretty printing pool methods" $ do
    it "Prints header file of test0" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __task_id;\n" ++
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t data_in;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++
              "__status_int32_t CHousekeeping__action0(const __termina_event_t * const __ev,\n" ++
              "                                        void * const __this, uint32_t _data);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints option header file of test0" $ do
      renderOption test0OptionMap `shouldBe`
        pack ("#ifndef __OPTION_H__\n" ++
              "#define __OPTION_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    uint32_t __0;\n" ++
              "} __option_uint32__Some_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __option_uint32__Some_params_t Some;\n" ++
              "    __enum_option_t __variant;\n" ++
              "} __option_uint32_t;\n" ++
              "\n" ++
              "#endif\n")