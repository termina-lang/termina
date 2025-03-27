module IT.Option.OptionSpec (spec) where

import IT.Common

import Test.Hspec
import Data.Text
import qualified Data.Map as M
import qualified Data.Set as S
import Core.AST
import Generator.Option

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "\n" ++
        "  data_in : sink u32 triggers action0;\n" ++
        "\n" ++
        "  action action0(&priv self, _data : u32) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
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

test0OptionMap :: OptionMap
test0OptionMap = M.fromList [(TUInt32, S.fromList [TOption TUInt32])]

spec :: Spec
spec = do
  describe "Pretty printing pool methods" $ do
    it "Prints header file of test0" $ do
      renderHeader True test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_id_t __task_msg_queue_id;\n" ++
              "    __termina_id_t data_in;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++   
              "void __CHousekeeping__termina_task(void * const arg);\n" ++
              "\n" ++   
              "Result CHousekeeping__action0(CHousekeeping * const self, uint32_t _data);\n" ++
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
              "} __option_uint32_params_t;\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __option_uint32_params_t Some;\n" ++
              "    __enum_option_t __variant;\n" ++
              "} __option_uint32_t;\n" ++
              "\n" ++
              "#endif\n")