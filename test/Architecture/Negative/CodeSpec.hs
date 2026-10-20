-- | Architecture (application wiring) negative tests, *code* flavour. The
-- reusable source fragments are exported for 'Architecture.Negative.DetailSpec'
-- and 'Architecture.Positive.WiringSpec'.
module Architecture.Negative.CodeSpec
  ( spec
  , timerTaskClass, periodicEmitter
  ) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)

-- A minimal valid task class triggered by a periodic timer. Reused, with
-- different wiring, to violate one architecture rule at a time.
timerTaskClass :: String
timerTaskClass =
    "task class TimerTask {\n" ++
    "    ticks : u32;\n" ++
    "    timer_port : sink TimeVal triggers timeout;\n" ++
    "    action timeout(&priv self, _t : TimeVal) {\n" ++
    "        self->ticks = self->ticks + 1 : u32;\n" ++
    "        return;\n" ++
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
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
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
            ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
            ++ "resource foo : FooRes = { value = 0 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-007")

    it "AE-008: pool used by nobody" $ do
      let src = timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ "#[priority(10)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
             ++ "resource mypool : Pool<u32; 4>;\n"
      compileErrorCode src `shouldBe` Just (pack "AE-008")

    it "AE-006: channel with a source but no target" $ do
      -- The task feeds a channel through an out port, but nothing consumes it.
      let src =
            "task class ProducerTask {\n" ++
            "    timer_port : sink TimeVal triggers timeout;\n" ++
            "    out_port : out u32;\n" ++
            "    action timeout(&priv self, _t : TimeVal) {\n" ++
            "        self->out_port.send(0 : u32);\n" ++
            "        return;\n" ++
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
            "    last : u32;\n" ++
            "    in_port : in u32 triggers handle;\n" ++
            "    action handle(&priv self, msg : u32) {\n" ++
            "        self->last = self->last + msg;\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "channel chan : MsgQueue<u32; 10>;\n"
            ++ "#[priority(10)]\n"
            ++ "task t : ConsumerTask = { last = 0, in_port <- chan };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-005")

    it "AE-001: same emitter connected to two sinks" $ do
      let src = timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ "#[priority(10)]\n"
             ++ "task t1 : TimerTask = { ticks = 0, timer_port <- timer };\n"
             ++ "#[priority(11)]\n"
             ++ "task t2 : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-001")

    it "AE-010: reboot in the action that attends system_except" $ do
      let src =
            "handler class FaultHandler {\n" ++
            "    faults : u32;\n" ++
            "    except_port : sink Exception triggers on_fault;\n" ++
            "    action on_fault(&priv self, _ex : Exception) {\n" ++
            "        self->faults = self->faults + 1 : u32;\n" ++
            "        reboot;\n" ++
            "    }\n" ++
            "};\n" ++
            "handler fault_handler : FaultHandler = { faults = 0, except_port <- system_except };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-010")

    it "AE-010: noreturn call in the action that attends system_except" $ do
      let src =
            "extern noreturn function jump_to_safe_image();\n" ++
            "handler class FaultHandler {\n" ++
            "    faults : u32;\n" ++
            "    except_port : sink Exception triggers on_fault;\n" ++
            "    action on_fault(&priv self, _ex : Exception) {\n" ++
            "        self->faults = self->faults + 1 : u32;\n" ++
            "        jump_to_safe_image();\n" ++
            "    }\n" ++
            "};\n" ++
            "handler fault_handler : FaultHandler = { faults = 0, except_port <- system_except };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-010")

    it "AE-011: send in the action that attends system_except" $ do
      let src =
            "handler class FaultHandler {\n" ++
            "    faults : u32;\n" ++
            "    except_port : sink Exception triggers on_fault;\n" ++
            "    report_port : out u32;\n" ++
            "    action on_fault(&priv self, _ex : Exception) {\n" ++
            "        self->faults = self->faults + 1 : u32;\n" ++
            "        self->report_port.send(self->faults);\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "task class FdirTask {\n" ++
            "    last : u32;\n" ++
            "    reports : in u32 triggers on_report;\n" ++
            "    action on_report(&priv self, code : u32) {\n" ++
            "        self->last = self->last + code;\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "channel report_chan : MsgQueue<u32; 4>;\n" ++
            "handler fault_handler : FaultHandler = { faults = 0, except_port <- system_except, report_port -> report_chan };\n" ++
            "#[priority(10)]\n" ++
            "task fdir : FdirTask = { last = 0, reports <- report_chan };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-011")

    it "accepts a system_except action that neither reboots nor hands over" $ do
      let src =
            "handler class FaultHandler {\n" ++
            "    faults : u32;\n" ++
            "    except_port : sink Exception triggers on_fault;\n" ++
            "    action on_fault(&priv self, _ex : Exception) {\n" ++
            "        self->faults = self->faults + 1 : u32;\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "handler fault_handler : FaultHandler = { faults = 0, except_port <- system_except };\n"
      compileErrorCode src `shouldBe` Nothing

    it "AE-009: two tasks with the same priority" $ do
      let src = timerTaskClass
             ++ periodicEmitter "timer1" 1
             ++ periodicEmitter "timer2" 2
             ++ "#[priority(10)]\n"
             ++ "task t1 : TimerTask = { ticks = 0, timer_port <- timer1 };\n"
             ++ "#[priority(10)]\n"
             ++ "task t2 : TimerTask = { ticks = 0, timer_port <- timer2 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-009")

    it "AE-002: same channel connected to two in ports" $ do
      let src =
            "task class ConsumerTask {\n" ++
            "    last : u32;\n" ++
            "    in_port : in u32 triggers handle;\n" ++
            "    action handle(&priv self, msg : u32) {\n" ++
            "        self->last = self->last + msg;\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "channel chan : MsgQueue<u32; 10>;\n"
            ++ "#[priority(10)]\n"
            ++ "task t1 : ConsumerTask = { last = 0, in_port <- chan };\n"
            ++ "#[priority(11)]\n"
            ++ "task t2 : ConsumerTask = { last = 0, in_port <- chan };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-002")

    it "AE-003: mismatched box source" $ do
      -- A box is allocated from pool0 but freed through pool1, so the source the
      -- box is returned to does not match the one it came from.
      let src =
            "task class BoxTask {\n" ++
            "    data_pool0 : access Allocator<u32>;\n" ++
            "    data_pool1 : access Allocator<u32>;\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    action tick(&priv self, _t : TimeVal) {\n" ++
            "        var alloc_data : Option<box u32> = None;\n" ++
            "        self->data_pool0.alloc(&mut alloc_data);\n" ++
            "        match alloc_data {\n" ++
            "            case Some(data) => {\n" ++
            "                self->data_pool1.free(data);\n" ++
            "            }\n" ++
            "            case None => {\n" ++
            "            }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 10>;\n" ++
            "resource pool1 : Pool<u32; 10>;\n" ++
            periodicEmitter "timer" 10
            ++ "#[priority(10)]\n"
            ++ "task t : BoxTask = { timer_port <- timer, data_pool0 <-> pool0, data_pool1 <-> pool1 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-003")

    it "AE-003: mismatched box source through a deque" $ do
      -- A box allocated from pool0 is parked in a deque by one action and
      -- freed through pool1 by the other one.
      let src =
            "task class BoxTask {\n" ++
            "    data_pool0 : access Allocator<u32>;\n" ++
            "    data_pool1 : access Allocator<u32>;\n" ++
            "    pending : access DequeAccess<u32>;\n" ++
            "    timer_port : sink TimeVal triggers park;\n" ++
            "    drain_port : sink TimeVal triggers drain;\n" ++
            "    action park(&priv self, _t : TimeVal) {\n" ++
            "        var old : Option<box u32> = None;\n" ++
            "        self->pending.take(&mut old);\n" ++
            "        match old {\n" ++
            "            case Some(o) => { self->data_pool0.free(o); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        var alloc_data : Option<box u32> = None;\n" ++
            "        self->data_pool0.alloc(&mut alloc_data);\n" ++
            "        match alloc_data {\n" ++
            "            case Some(data) => { self->pending.put(data); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        var other_data : Option<box u32> = None;\n" ++
            "        self->data_pool1.alloc(&mut other_data);\n" ++
            "        match other_data {\n" ++
            "            case Some(d) => { self->data_pool1.free(d); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "    action drain(&priv self, _t : TimeVal) {\n" ++
            "        var old : Option<box u32> = None;\n" ++
            "        self->pending.take(&mut old);\n" ++
            "        match old {\n" ++
            "            case Some(o) => { self->data_pool1.free(o); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 10>;\n" ++
            "resource pool1 : Pool<u32; 10>;\n" ++
            "resource pending_deque : Deque<u32; 1>;\n" ++
            periodicEmitter "timer" 10 ++
            periodicEmitter "timer2" 20
            ++ "#[priority(10)]\n"
            ++ "task t : BoxTask = { timer_port <- timer, drain_port <- timer2, data_pool0 <-> pool0, data_pool1 <-> pool1, pending <-> pending_deque };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-003")

    it "AE-012: deque not connected to any access port" $ do
      let src = timerTaskClass ++ periodicEmitter "timer" 1
            ++ "resource orphan : Deque<u32; 1>;\n"
            ++ "#[priority(10)]\n"
            ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-012")

    it "AE-014: deque that only gets back the boxes taken from it" $ do
      let src =
            "task class DequeTask {\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    pending : access DequeAccess<u32>;\n" ++
            "    action tick(&priv self, _t : TimeVal) {\n" ++
            "        var old : Option<box u32> = None;\n" ++
            "        self->pending.take(&mut old);\n" ++
            "        match old {\n" ++
            "            case Some(o) => { self->pending.put(o); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pending_deque : Deque<u32; 1>;\n" ++
            periodicEmitter "timer" 10
            ++ "#[priority(10)]\n"
            ++ "task t : DequeTask = { timer_port <- timer, pending <-> pending_deque };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-014")

    it "AE-013: deque connected to the access ports of two tasks" $ do
      let src =
            "task class DequeTask {\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    pool : access Allocator<u32>;\n" ++
            "    pending : access DequeAccess<u32>;\n" ++
            "    action tick(&priv self, _t : TimeVal) {\n" ++
            "        var old : Option<box u32> = None;\n" ++
            "        self->pending.take(&mut old);\n" ++
            "        match old {\n" ++
            "            case Some(o) => { self->pool.free(o); }\n" ++
            "            case None => {\n" ++
            "                var fresh : Option<box u32> = None;\n" ++
            "                self->pool.alloc(&mut fresh);\n" ++
            "                match fresh {\n" ++
            "                    case Some(f) => { self->pending.put(f); }\n" ++
            "                    case None => { }\n" ++
            "                }\n" ++
            "            }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 4>;\n" ++
            "resource shared_deque : Deque<u32; 1>;\n" ++
            periodicEmitter "timer" 10 ++
            periodicEmitter "timer2" 20
            ++ "#[priority(10)]\n"
            ++ "task t1 : DequeTask = { timer_port <- timer, pool <-> pool0, pending <-> shared_deque };\n"
            ++ "#[priority(11)]\n"
            ++ "task t2 : DequeTask = { timer_port <- timer2, pool <-> pool0, pending <-> shared_deque };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-013")

    it "AE-003: mismatched box source inside a for loop" $ do
      -- The same mismatch, with the box allocated and freed in the body of a loop.
      let src =
            "task class BoxTask {\n" ++
            "    data_pool0 : access Allocator<u32>;\n" ++
            "    data_pool1 : access Allocator<u32>;\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    action tick(&priv self, _t : TimeVal) {\n" ++
            "        for i : usize in 0 : usize .. 3 : usize {\n" ++
            "            var alloc_data : Option<box u32> = None;\n" ++
            "            self->data_pool0.alloc(&mut alloc_data);\n" ++
            "            match alloc_data {\n" ++
            "                case Some(data) => {\n" ++
            "                    self->data_pool1.free(data);\n" ++
            "                }\n" ++
            "                case None => {\n" ++
            "                }\n" ++
            "            }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 10>;\n" ++
            "resource pool1 : Pool<u32; 10>;\n" ++
            periodicEmitter "timer" 10
            ++ "#[priority(10)]\n"
            ++ "task t : BoxTask = { timer_port <- timer, data_pool0 <-> pool0, data_pool1 <-> pool1 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-003")

    it "AE-003: mismatched box source through an option-box parameter" $ do
      -- The procedure of Source fills the option-box with a box from pool0,
      -- through Inner, on one branch, and with a box from pool1 on the other.
      -- The task frees every box it gets back to pool0.
      let src =
            "interface IGet {\n" ++
            "    procedure get(&mut self, result : &mut Option<box u32>);\n" ++
            "};\n" ++
            "resource class Inner provides IGet {\n" ++
            "    pool : access Allocator<u32>;\n" ++
            "    procedure get(&mut self, result : &mut Option<box u32>) {\n" ++
            "        self->pool.alloc(result);\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource class Source provides IGet {\n" ++
            "    inner : access IGet;\n" ++
            "    pool : access Allocator<u32>;\n" ++
            "    procedure get(&mut self, result : &mut Option<box u32>) {\n" ++
            "        var tmp : Option<box u32> = None;\n" ++
            "        self->inner.get(&mut tmp);\n" ++
            "        match tmp {\n" ++
            "            case Some(b) => { *result = Some(b); }\n" ++
            "            case None => { self->pool.alloc(result); }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "task class BoxTask {\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    src : access IGet;\n" ++
            "    data_pool : access Allocator<u32>;\n" ++
            "    action tick(&priv self, _t : TimeVal) {\n" ++
            "        var opt : Option<box u32> = None;\n" ++
            "        self->src.get(&mut opt);\n" ++
            "        match opt {\n" ++
            "            case Some(b) => { self->data_pool.free(b); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 10>;\n" ++
            "resource pool1 : Pool<u32; 10>;\n" ++
            "resource inner_res : Inner = { pool <-> pool0 };\n" ++
            "resource src_res : Source = { inner <-> inner_res, pool <-> pool1 };\n" ++
            periodicEmitter "timer" 10
            ++ "#[priority(10)]\n"
            ++ "task t : BoxTask = { timer_port <- timer, src <-> src_res, data_pool <-> pool0 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-003")

    it "AE-003: mismatched box source through an option-box argument" $ do
      -- The task moves an option-box allocated from pool0 into the procedure,
      -- which frees its box to pool1.
      let src =
            "interface ISink {\n" ++
            "    procedure consume(&mut self, data : Option<box u32>);\n" ++
            "};\n" ++
            "resource class Sink provides ISink {\n" ++
            "    pool : access Allocator<u32>;\n" ++
            "    procedure consume(&mut self, data : Option<box u32>) {\n" ++
            "        match data {\n" ++
            "            case Some(v) => { self->pool.free(v); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "task class BoxTask {\n" ++
            "    timer_port : sink TimeVal triggers tick;\n" ++
            "    snk : access ISink;\n" ++
            "    data_pool0 : access Allocator<u32>;\n" ++
            "    data_pool1 : access Allocator<u32>;\n" ++
            "    action tick(&priv self, _t : TimeVal) {\n" ++
            "        var a : Option<box u32> = None;\n" ++
            "        self->data_pool0.alloc(&mut a);\n" ++
            "        self->snk.consume(a);\n" ++
            "        var b : Option<box u32> = None;\n" ++
            "        self->data_pool1.alloc(&mut b);\n" ++
            "        match b {\n" ++
            "            case Some(x) => { self->data_pool1.free(x); }\n" ++
            "            case None => { }\n" ++
            "        }\n" ++
            "        return;\n" ++
            "    }\n" ++
            "};\n" ++
            "resource pool0 : Pool<u32; 10>;\n" ++
            "resource pool1 : Pool<u32; 10>;\n" ++
            "resource sink_res : Sink = { pool <-> pool1 };\n" ++
            periodicEmitter "timer" 10
            ++ "#[priority(10)]\n"
            ++ "task t : BoxTask = { timer_port <- timer, snk <-> sink_res, data_pool0 <-> pool0, data_pool1 <-> pool1 };\n"
      compileErrorCode src `shouldBe` Just (pack "AE-003")
