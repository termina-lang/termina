module IT.TypeDef.AtomicSpec (spec) where

import Test.Hspec
import Parser.Parsing
import Data.Text hiding (empty)
import Text.Parsec
import Semantic.TypeChecking
import Semantic.Monad
import qualified Data.Map as M
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import ControlFlow.BasicBlocks

test0 :: String
test0 = "task class CHousekeeping {\n" ++
        "  interval : access AtomicAccess<u32>;\n" ++
        "  timer : sink TimeVal triggers timeout;\n" ++
        "\n" ++
        "  action timeout(&mut self, current : TimeVal) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
        "    var local : u32 = 0;\n" ++
        "\n" ++
        "    self->interval.store(32);\n" ++
        "    self->interval.load(&mut local);\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test1 :: String
test1 = "task class CHousekeeping {\n" ++
        "  interval : access AtomicArrayAccess<u32; 10>;\n" ++
        "  timer : sink TimeVal triggers timeout;\n" ++
        "\n" ++
        "  action timeout(&mut self, current : TimeVal) -> Result {\n" ++
        "\n" ++
        "    var ret : Result = Result::Ok;\n" ++
        "    var local : u32 = 0;\n" ++
        "\n" ++
        "    self->interval.store_index(0, 32);\n" ++
        "    self->interval.load_index(1, &mut local);\n" ++
        "\n" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

renderHeader :: String -> Text
renderHeader input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenHeaderFile True "test" [] bbAST M.empty of
              Left err -> pack $ show err
              Right cHeaderFile -> runCPrinter cHeaderFile

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile "test" bbAST of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter cSourceFile


spec :: Spec
spec = do
  describe "Classes with atomic access ports" $ do
    it "Prints declaration of class with atomic access port" $ do
      renderHeader test0 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_task_t __task;\n" ++
              "    __termina_sink_port_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(CHousekeeping * const self, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(CHousekeeping * const self, TimeVal current) {\n" ++
              "    \n" ++
              "    Result ret;\n" ++
              "    ret.__variant = Result__Ok;\n" ++
              "\n" ++
              "    uint32_t local = 0;\n" ++
              "\n" ++
              "    atomic_store(self->interval, 32);\n" ++
              "\n" ++
              "    local = atomic_load(self->interval);\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")
    it "Prints declaration of class with atomic access port" $ do
      renderHeader test1 `shouldBe`
        pack ("#ifndef __TEST_H__\n" ++
              "#define __TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "#include \"option.h\"\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    __termina_task_t __task;\n" ++
              "    __termina_sink_port_t timer;\n" ++
              "    _Atomic uint32_t * interval;\n" ++
              "} CHousekeeping;\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(CHousekeeping * const self, TimeVal current);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of class with atomic access port" $ do
      renderSource test1 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "Result CHousekeeping__timeout(CHousekeeping * const self, TimeVal current) {\n" ++
              "    \n" ++
              "    Result ret;\n" ++
              "    ret.__variant = Result__Ok;\n" ++
              "\n" ++
              "    uint32_t local = 0;\n" ++
              "\n" ++
              "    atomic_store(&self->interval[0], 32);\n" ++
              "\n" ++
              "    local = atomic_load(&self->interval[1]);\n" ++
              "\n" ++
              "    return ret;\n" ++
              "\n" ++
              "}\n")
