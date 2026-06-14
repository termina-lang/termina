module ConstFolding.AtomicSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

-- A task that drives a periodic timer and reaches into an atomic array through
-- an access port. Reused, with the wiring/index tweaked, to trigger each atomic
-- constant-folding error.
atomicProgram :: String -> String -> String -> String
atomicProgram extraConsts portSize body =
    extraConsts ++
    "resource arr : AtomicArray<u32; 4> = { values = [0 : u32; 4] };\n" ++
    "task class T {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    arr_port : access AtomicArrayAccess<u32; " ++ portSize ++ ">;\n" ++
    "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
    body ++
    "        let ret : Status<i32> = Success;\n" ++
    "        return ret;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : T = { timer_port <- timer, arr_port <-> arr };\n"

spec :: Spec
spec = do
  describe "ConstFolding: atomic array errors" $ do

    it "CPE-001: atomic array connection size mismatch" $ do
      -- The access port is sized [u32; 5] but the connected array is [u32; 4].
      let src = atomicProgram "" "5"
                  "        self->arr_port.store_index(0 : usize, 5 : u32);\n"
      compileErrorCode src `shouldBe` Just (pack "CPE-001")

    it "CPE-015: atomic array index out of bounds with constant index" $ do
      -- store_index at a constant index 10, out of the size-4 array.
      let src = atomicProgram "const bad_idx : usize = 10;\n" "4"
                  "        self->arr_port.store_index(bad_idx, 5 : u32);\n"
      compileErrorCode src `shouldBe` Just (pack "CPE-015")
