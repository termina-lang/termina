-- | Full-pipeline tests of the instances of a test class: the instance is a
-- global of the C of the test.fin, like an instance of a task, and the init
-- file wires its ports.
module Pipeline.Positive.TestInstanceSpec (spec) where

import Pipeline.Common

import qualified Data.Map.Strict as M
import Data.Text (unpack)
import Test.Hspec

-- | A task of the unit that reads a channel and writes a resource, and a test
-- class that sends to the channel and reaches the resource.
unitTest :: String
unitTest =
    "interface IValue {\n" ++
    "    procedure set(&mut self, v : u32);\n" ++
    "};\n" ++
    "resource class CValue provides IValue {\n" ++
    "    value : u32;\n" ++
    "    procedure set(&mut self, v : u32) {\n" ++
    "        self->value = self->value + v;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "task class CWriter {\n" ++
    "    tc_in : in u32 triggers on_tc;\n" ++
    "    value : access IValue;\n" ++
    "    action on_tc(&priv self, v : u32) {\n" ++
    "        self->value.set(v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CWriterTests {\n" ++
    "    tc_out : out u32;\n" ++
    "    value : access IValue;\n" ++
    "    case writes(&mut self) {\n" ++
    "        self->tc_out.send(7 : u32);\n" ++
    "        self->value.set(0 : u32);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "resource value : CValue = { value = 0 };\n" ++
    "channel tcs : MsgQueue<u32; 1>;\n" ++
    "#[priority(5)]\n" ++
    "task writer : CWriter = { tc_in <- tcs, value <-> value };\n" ++
    "test writer_tests : CWriterTests = { tc_out -> tcs, value <-> value };\n"

spec :: Spec
spec = describe "Full pipeline: test instances" $ do
  it "defines the test instance as a global of the module" $
    case runTestBuild unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right rendered ->
        maybe (expectationFailure "no module test")
          (\c -> unpack c `shouldContain` "CWriterTests writer_tests;")
          (M.lookup "test" rendered)
  it "wires the ports of the test instance in the init file" $
    case runTestApp unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (_, prjprogs) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "writer_tests.tc_out = &tcs;"
            unpack c `shouldContain` "writer_tests.value._that = &value;"
            unpack c `shouldContain` "writer_tests.value.set = &CValue__set;")
          (renderInitFile prjprogs)
