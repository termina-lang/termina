module Architecture.AppWiringSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

-- A monadic variant ('Success') cannot be returned directly; it must be bound
-- first (otherwise SE-069 fires before we reach the architecture stage). This
-- is the smallest well-formed action body.
returnSuccess :: String
returnSuccess =
    "        let ret : Status<i32> = Success;\n" ++
    "        return ret;\n"

-- A minimal valid task class triggered by a periodic timer. Reused, with
-- different wiring, to violate one architecture rule at a time.
timerTaskClass :: String
timerTaskClass =
    "task class TimerTask {\n" ++
    "    timer_port : sink TimeVal triggers timeout;\n" ++
    "    action timeout(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
    returnSuccess ++
    "    }\n" ++
    "};\n"

periodicEmitter :: String -> Integer -> String
periodicEmitter name secs =
    "emitter " ++ name ++ " : PeriodicTimer = { period = {tv_sec = " ++
    show secs ++ ", tv_usec = 0} };\n"

spec :: Spec
spec = do
  describe "Architecture: application wiring errors" $ do

    it "AE-004: emitter not connected to any sink" $ do
      -- Two emitters declared, only one wired; the other is disconnected.
      let src = timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ periodicEmitter "spare" 2
             ++ "#[priority(10)]\n"
             ++ "task t : TimerTask = { timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-004")

    it "AE-007: resource instance used by nobody" $ do
      let src =
            "interface IFoo {\n" ++
            "    procedure get(&mut self, result : &mut u32);\n" ++
            "};\n" ++
            "resource class FooRes provides IFoo {\n" ++
            "    value : u32;\n" ++
            "    procedure get(&mut self, result : &mut u32) {\n" ++
            "        *result = self->value;\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            timerTaskClass
            ++ periodicEmitter "timer" 1
            ++ "#[priority(10)]\n"
            ++ "task t : TimerTask = { timer_port <- timer };\n"
            ++ "resource foo : FooRes = { value = 0 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-007")

    it "AE-008: pool used by nobody" $ do
      let src = timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ "#[priority(10)]\n"
             ++ "task t : TimerTask = { timer_port <- timer };\n"
             ++ "resource mypool : Pool<u32; 4>;\n"
      compileErrorCode src `shouldBe` Just (pack "AE-008")

    it "AE-006: channel with a source but no target" $ do
      -- The task feeds a channel through an out port, but nothing consumes it.
      let src =
            "task class ProducerTask {\n" ++
            "    timer_port : sink TimeVal triggers timeout;\n" ++
            "    out_port : out u32;\n" ++
            "    action timeout(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
            "        let ret : Status<i32> = Success;\n" ++
            "        self->out_port.send(0 : u32);\n" ++
            "        return ret;\n" ++
            "    }\n" ++
            "};\n" ++
            periodicEmitter "timer" 1
            ++ "channel chan : MsgQueue<u32; 10>;\n"
            ++ "#[priority(10)]\n"
            ++ "task t : ProducerTask = { timer_port <- timer, out_port -> chan };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-006")

    it "AE-005: channel with a target but no source" $ do
      -- The task consumes a channel through an in port, but nothing feeds it.
      let src =
            "task class ConsumerTask {\n" ++
            "    in_port : in u32 triggers handle;\n" ++
            "    action handle(&priv self, _msg : u32) -> Status<i32> {\n" ++
            returnSuccess ++
            "    }\n" ++
            "};\n" ++
            "channel chan : MsgQueue<u32; 10>;\n"
            ++ "#[priority(10)]\n"
            ++ "task t : ConsumerTask = { in_port <- chan };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-005")

    it "AE-001: same emitter connected to two sinks" $ do
      let src = timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ "#[priority(10)]\n"
             ++ "task t1 : TimerTask = { timer_port <- timer };\n"
             ++ "#[priority(11)]\n"
             ++ "task t2 : TimerTask = { timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-001")

    it "AE-002: same channel connected to two in ports" $ do
      let src =
            "task class ConsumerTask {\n" ++
            "    in_port : in u32 triggers handle;\n" ++
            "    action handle(&priv self, _msg : u32) -> Status<i32> {\n" ++
            returnSuccess ++
            "    }\n" ++
            "};\n" ++
            "channel chan : MsgQueue<u32; 10>;\n"
            ++ "#[priority(10)]\n"
            ++ "task t1 : ConsumerTask = { in_port <- chan };\n"
            ++ "#[priority(11)]\n"
            ++ "task t2 : ConsumerTask = { in_port <- chan };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-002")

    it "AE-003: mismatched box source" $ do
      -- A box is allocated from pool0 but freed through pool1, so the source the
      -- box is returned to does not match the one it came from.
      let src =
            "task class BoxTask {\n" ++
            "    data_pool0 : access Allocator<u32>;\n" ++
            "    data_pool1 : access Allocator<u32>;\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
            "        var ret : Status<i32> = Success;\n" ++
            "        var alloc_data : Option<box u32> = None;\n" ++
            "        self->data_pool0.alloc(&mut alloc_data);\n" ++
            "        match alloc_data {\n" ++
            "            case Some(data) => {\n" ++
            "                self->data_pool1.free(data);\n" ++
            "            }\n" ++
            "            case None => {\n" ++
            "                ret = Failure(0 : i32);\n" ++
            "            }\n" ++
            "        }\n" ++
            "        return ret;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 10>;\n" ++
            "resource pool1 : Pool<u32; 10>;\n" ++
            periodicEmitter "timer" 10
            ++ "#[priority(10)]\n"
            ++ "task t : BoxTask = { timer_port <- timer, data_pool0 <-> pool0, data_pool1 <-> pool1 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-003")
