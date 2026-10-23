module Semantic.Positive.TestComponentsSpec (spec) where

import Semantic.Common (typeCheckErrorOn, typeCheckTestErrorOn)
import Configuration.Platform (Platform(TestPlatform))

import Data.Text (pack)
import Test.Hspec

-- The components of a unit test take the place of the emitters, the channels
-- and the pools of production at the other end of the ports of the unit, and
-- a case reaches them, and test_entry, through access ports. These are the
-- examples of sections 4.10 and 4.11 of the design of the tests.

-- | A task that samples a sensor on each period and sends an alarm above a
-- limit, tested through a TestEmitter and a TestChannel (section 4.10).
tempMonitor :: String
tempMonitor =
  "interface ITempSensor {\n" ++
  "    procedure read(&self, temp : &mut u32);\n" ++
  "};\n" ++
  "interface IFakeSensor {\n" ++
  "    procedure set(&mut self, v : u32);\n" ++
  "};\n" ++
  "task class CTempMonitor {\n" ++
  "    tick_ev : sink TimeVal triggers sample;\n" ++
  "    sensor : access ITempSensor;\n" ++
  "    alarm_out : out u32;\n" ++
  "    limit : u32;\n" ++
  "    action sample(&priv self, _now : TimeVal) {\n" ++
  "        var temp : u32 = 0;\n" ++
  "        self->sensor.read(&mut temp);\n" ++
  "        if temp > self->limit {\n" ++
  "            self->alarm_out.send(temp);\n" ++
  "        }\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "resource class CFakeSensor provides ITempSensor, IFakeSensor {\n" ++
  "    value : u32;\n" ++
  "    procedure read(&self, temp : &mut u32) { *temp = self->value; return; }\n" ++
  "    procedure set(&mut self, v : u32) { self->value = v; return; }\n" ++
  "};\n" ++
  "test class CTempMonitorTests {\n" ++
  "    sensor : access IFakeSensor;\n" ++
  "    tick : access TestEmitterAccess<TimeVal>;\n" ++
  "    alarms : access TestChannelAccess<u32>;\n" ++
  "    t : access TestAPI;\n" ++
  "    case no_alarm_below_limit(&mut self) {\n" ++
  "        var n : usize = 0;\n" ++
  "        self->sensor.set(70);\n" ++
  "        self->tick.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
  "        self->alarms.pending(&mut n);\n" ++
  "        self->t.check_eq_usize(n, 0 : usize);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    case alarm_above_limit(&mut self) {\n" ++
  "        var a : Option<u32> = None;\n" ++
  "        self->sensor.set(95);\n" ++
  "        self->t.advance({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
  "        self->tick.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
  "        self->alarms.take(&mut a);\n" ++
  "        match a {\n" ++
  "            case Some(v) => { self->t.check_eq_u32(v, 95 : u32); }\n" ++
  "            case None => { self->t.fail(); }\n" ++
  "        }\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

tempMonitorWiring :: String
tempMonitorWiring =
  "resource sensor : CFakeSensor = { value = 0 };\n" ++
  "emitter tick : TestEmitter<TimeVal>;\n" ++
  "channel alarms : TestChannel<u32; 4>;\n" ++
  "#[priority(5)]\n" ++
  "task uut : CTempMonitor = {\n" ++
  "    limit = 80,\n" ++
  "    tick_ev <- tick,\n" ++
  "    sensor <-> sensor,\n" ++
  "    alarm_out -> alarms\n" ++
  "};\n" ++
  "test monitor_tests : CTempMonitorTests = {\n" ++
  "    sensor <-> sensor,\n" ++
  "    tick <-> tick,\n" ++
  "    alarms <-> alarms,\n" ++
  "    t <-> test_entry\n" ++
  "};\n"

-- | A task that answers a telecommand with a telemetry packet in a box, with
-- the message to the unit sent through an ordinary channel and the answer
-- read from a TestChannel of boxes (section 4.11), and the pool of the
-- packets a TestPool that a case observes and makes fail.
tcExecutor :: String
tcExecutor =
  "struct TCCommand {\n" ++
  "    service : u8;\n" ++
  "    subtype : u8;\n" ++
  "};\n" ++
  "struct TMPacket {\n" ++
  "    service : u8;\n" ++
  "    subtype : u8;\n" ++
  "};\n" ++
  "task class CTCExecutor {\n" ++
  "    tc_in : in TCCommand triggers execute;\n" ++
  "    tm_out : out box TMPacket;\n" ++
  "    tm_pool : access Allocator<TMPacket>;\n" ++
  "    action execute(&priv self, tc : TCCommand) {\n" ++
  "        var tm_opt : Option<box TMPacket> = None;\n" ++
  "        self->tm_pool.alloc(&mut tm_opt);\n" ++
  "        match tm_opt {\n" ++
  "            case Some(b) => {\n" ++
  "                b.service = tc.service;\n" ++
  "                b.subtype = tc.subtype + 1 : u8;\n" ++
  "                self->tm_out.send(b);\n" ++
  "            }\n" ++
  "            case None => {}\n" ++
  "        }\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "test class CTCExecutorTests {\n" ++
  "    tc_out : out TCCommand;\n" ++
  "    tm_rx : access TestChannelAccess<box TMPacket>;\n" ++
  "    tm_pool : access Allocator<TMPacket>;\n" ++
  "    tm_pool_state : access TestPoolAccess;\n" ++
  "    t : access TestAPI;\n" ++
  "    case ping_answers_with_one_tm(&mut self) {\n" ++
  "        var n : usize = 0;\n" ++
  "        var tm_opt : Option<box TMPacket> = None;\n" ++
  "        let ping : TCCommand = { service = 17, subtype = 1 } : TCCommand;\n" ++
  "        self->tc_out.send(ping);\n" ++
  "        self->tm_rx.pending(&mut n);\n" ++
  "        self->t.check_eq_usize(n, 1 : usize);\n" ++
  "        self->tm_rx.take(&mut tm_opt);\n" ++
  "        match tm_opt {\n" ++
  "            case Some(b) => {\n" ++
  "                self->t.check_eq_u8(b.service, 17 : u8);\n" ++
  "                self->t.check_eq_u8(b.subtype, 2 : u8);\n" ++
  "                self->tm_pool.free(b);\n" ++
  "            }\n" ++
  "            case None => {\n" ++
  "                self->t.fail();\n" ++
  "            }\n" ++
  "        }\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    case no_answer_without_blocks(&mut self) {\n" ++
  "        var n : usize = 0;\n" ++
  "        let ping : TCCommand = { service = 17, subtype = 1 } : TCCommand;\n" ++
  "        self->tm_pool_state.fail_allocs(1 : usize);\n" ++
  "        self->tc_out.send(ping);\n" ++
  "        self->tm_rx.pending(&mut n);\n" ++
  "        self->t.check_eq_usize(n, 0 : usize);\n" ++
  "        self->tm_pool_state.in_use(&mut n);\n" ++
  "        self->t.check_eq_usize(n, 0 : usize);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

tcExecutorWiring :: String
tcExecutorWiring =
  "resource tm_pool : TestPool<TMPacket; 4>;\n" ++
  "channel tcs : MsgQueue<TCCommand; 1>;\n" ++
  "channel tms : TestChannel<box TMPacket; 4>;\n" ++
  "#[priority(5)]\n" ++
  "task uut : CTCExecutor = {\n" ++
  "    tc_in <- tcs,\n" ++
  "    tm_out -> tms,\n" ++
  "    tm_pool <-> tm_pool\n" ++
  "};\n" ++
  "test exec_tests : CTCExecutorTests = {\n" ++
  "    tc_out -> tcs,\n" ++
  "    tm_rx <-> tms,\n" ++
  "    tm_pool <-> tm_pool,\n" ++
  "    tm_pool_state <-> tm_pool,\n" ++
  "    t <-> test_entry\n" ++
  "};\n"

spec :: Spec
spec = describe "Semantic: components of a unit test and test_entry" $ do

  it "type-checks the example of a task with a sink and an output" $
    typeCheckTestErrorOn TestPlatform (tempMonitor ++ tempMonitorWiring) `shouldBe` Nothing

  it "type-checks the example of messages in both directions, with a TestPool" $
    typeCheckTestErrorOn TestPlatform (tcExecutor ++ tcExecutorWiring) `shouldBe` Nothing

  it "rejects the test class outside a test" $
    typeCheckErrorOn TestPlatform (tempMonitor ++ tempMonitorWiring) `shouldBe` Just (pack "SE-236")

  it "rejects the components of a unit test outside a test" $
    typeCheckErrorOn TestPlatform "channel alarms : TestChannel<u32; 4>;\n" `shouldBe` Just (pack "SE-236")

  it "has no TestAPI outside a test" $
    typeCheckErrorOn TestPlatform
      ("task class CUser {\n" ++
       "    tick_ev : sink TimeVal triggers run;\n" ++
       "    t : access TestAPI;\n" ++
       "    action run(&priv self, _now : TimeVal) {\n" ++
       "        self->t.fail();\n" ++
       "        return;\n" ++
       "    }\n" ++
       "};\n") `shouldBe` Just (pack "SE-077")

  it "rejects firing an emitter through an immutable self reference" $
    typeCheckTestErrorOn TestPlatform (tempMonitor ++
      "test class CReadTests {\n" ++
      "    tick : access TestEmitterAccess<TimeVal>;\n" ++
      "    case fires(&self) {\n" ++
      "        self->tick.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
      "        return;\n" ++
      "    }\n" ++
      "};\n") `shouldBe` Just (pack "SE-199")

  it "rejects an unknown procedure of a TestChannelAccess" $
    typeCheckTestErrorOn TestPlatform (tempMonitor ++
      "test class CReadTests {\n" ++
      "    alarms : access TestChannelAccess<u32>;\n" ++
      "    case receives(&mut self) {\n" ++
      "        var v : Option<u32> = None;\n" ++
      "        self->alarms.receive(&mut v);\n" ++
      "        return;\n" ++
      "    }\n" ++
      "};\n") `shouldBe` Just (pack "SE-012")
