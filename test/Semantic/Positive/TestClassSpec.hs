module Semantic.Positive.TestClassSpec (spec) where

import Semantic.Common (typeCheckErrorOn)
import Configuration.Platform (Platform(TestPlatform))

import Data.Text (pack)
import Test.Hspec

-- A test class has access and out ports, private methods and cases, and a
-- test instance wires its ports like any other instance. A case takes any
-- self reference, and one that takes "&self" can only call what an immutable
-- self reference allows.
-- The cases call the methods of the class and the procedures and the send of
-- its ports.

unit :: String
unit =
  "interface ISensor {\n" ++
  "    procedure set(&mut self, v : u32);\n" ++
  "    procedure get(&self, v : &mut u32);\n" ++
  "};\n" ++
  "resource class CSensor provides ISensor {\n" ++
  "    value : u32;\n" ++
  "    procedure set(&mut self, v : u32) {\n" ++
  "        self->value = v;\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    procedure get(&self, v : &mut u32) {\n" ++
  "        *v = self->value;\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "task class CMonitor {\n" ++
  "    tc_in : in u32 triggers on_tc;\n" ++
  "    sensor : access ISensor;\n" ++
  "    action on_tc(&priv self, v : u32) {\n" ++
  "        self->sensor.set(v);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "resource sensor : CSensor = { value = 0 : u32 };\n" ++
  "channel tcs : MsgQueue<u32; 1>;\n" ++
  "#[priority(5)]\n" ++
  "task uut : CMonitor = {\n" ++
  "    tc_in <- tcs,\n" ++
  "    sensor <-> sensor\n" ++
  "};\n"

tests :: String
tests =
  "test class CMonitorTests {\n" ++
  "    sensor : access ISensor;\n" ++
  "    tc_out : out u32;\n" ++
  "    method prepare(&priv self) {\n" ++
  "        self->sensor.set(70 : u32);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    case sends_a_value(&mut self) {\n" ++
  "        self->prepare();\n" ++
  "        self->tc_out.send(95 : u32);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    case sets_the_sensor(&priv self) {\n" ++
  "        self->sensor.set(80 : u32);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    case reads_the_sensor(&self) {\n" ++
  "        var v : u32 = 0;\n" ++
  "        self->sensor.get(&mut v);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "test monitor_tests : CMonitorTests = {\n" ++
  "    sensor <-> sensor,\n" ++
  "    tc_out -> tcs\n" ++
  "};\n"

spec :: Spec
spec = describe "Semantic: test classes and test instances" $ do

  it "type-checks a test class with ports, a method and cases, and its instance" $
    typeCheckErrorOn TestPlatform (unit ++ tests) `shouldBe` Nothing

  it "rejects a case with an immutable self that calls a procedure with a mutable self" $
    typeCheckErrorOn TestPlatform (unit ++
      "test class CReadTests {\n" ++
      "    sensor : access ISensor;\n" ++
      "    case sets_the_sensor(&self) {\n" ++
      "        self->sensor.set(1 : u32);\n" ++
      "        return;\n" ++
      "    }\n" ++
      "};\n") `shouldBe` Just (pack "SE-199")

  it "rejects a call to a case, which only the harness runs" $
    typeCheckErrorOn TestPlatform (unit ++
      "test class CCallTests {\n" ++
      "    sensor : access ISensor;\n" ++
      "    case first(&mut self) {\n" ++
      "        self->sensor.set(1 : u32);\n" ++
      "        return;\n" ++
      "    }\n" ++
      "    case second(&mut self) {\n" ++
      "        self->first();\n" ++
      "        return;\n" ++
      "    }\n" ++
      "};\n") `shouldBe` Just (pack "SE-038")

  it "type-checks several test instances of the same class" $
    typeCheckErrorOn TestPlatform (unit ++ tests ++
      "test more_tests : CMonitorTests = {\n" ++
      "    sensor <-> sensor,\n" ++
      "    tc_out -> tcs\n" ++
      "};\n") `shouldBe` Nothing
