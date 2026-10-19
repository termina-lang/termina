-- | Architecture positive tests: sound application wiring raises no error. Each
-- case is the well-wired dual of an AE error (emitter connected, channel with
-- both ends, resource and pool actually used).
module Architecture.Positive.WiringSpec (spec) where

import Architecture.Common (architectureError)
import Architecture.Negative.CodeSpec (timerTaskClass, periodicEmitter)

import Data.Maybe (isNothing)
import Test.Hspec

-- A timer emitter wired to the sink of a single task.
timerWired :: String
timerWired =
  timerTaskClass ++ periodicEmitter "timer" 1
  ++ "#[priority(10)]\n"
  ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"

-- A channel with both a source (producer out port) and a target (consumer in
-- port): the dual of AE-005/AE-006.
channelWired :: String
channelWired =
  "task class ProducerTask {\n" ++
  "    timer_port : sink TimeVal triggers tick;\n" ++
  "    out_port : out u32;\n" ++
  "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
  "        let ret : Status<i32> = Success;\n" ++
  "        self->out_port.send(0 : u32);\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "};\n" ++
  "task class ConsumerTask {\n" ++
  "    in_port : in u32 triggers handle;\n" ++
  "    action handle(&priv self, _m : u32) -> Status<i32> {\n" ++
  "        let ret : Status<i32> = Success;\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "};\n" ++
  periodicEmitter "timer" 1 ++
  "channel chan : MsgQueue<u32; 10>;\n" ++
  "#[priority(10)]\n" ++
  "task p : ProducerTask = { timer_port <- timer, out_port -> chan };\n" ++
  "#[priority(11)]\n" ++
  "task c : ConsumerTask = { in_port <- chan };\n"

-- A resource instance accessed by a task through an access port: dual of AE-007.
resourceUsed :: String
resourceUsed =
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
  "task class UserTask {\n" ++
  "    timer_port : sink TimeVal triggers tick;\n" ++
  "    foo : access IFoo;\n" ++
  "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
  "        var ret : Status<i32> = Success;\n" ++
  "        var v : u32 = 0 : u32;\n" ++
  "        self->foo.get(&mut v);\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "};\n" ++
  periodicEmitter "timer" 1 ++
  "resource foo_res : FooRes = { value = 0 };\n" ++
  "#[priority(10)]\n" ++
  "task t : UserTask = { timer_port <- timer, foo <-> foo_res };\n"

-- A pool accessed by a task through an allocator port: dual of AE-008.
poolUsed :: String
poolUsed =
  "task class PoolTask {\n" ++
  "    timer_port : sink TimeVal triggers tick;\n" ++
  "    mypool : access Allocator<u32>;\n" ++
  "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
  "        var ret : Status<i32> = Success;\n" ++
  "        var b : Option<box u32> = None;\n" ++
  "        self->mypool.alloc(&mut b);\n" ++
  "        match b {\n" ++
  "            case Some(x) => { self->mypool.free(x); }\n" ++
  "            case None => { }\n" ++
  "        }\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "};\n" ++
  periodicEmitter "timer" 1 ++
  "resource mypool_res : Pool<u32; 4>;\n" ++
  "#[priority(10)]\n" ++
  "task t : PoolTask = { timer_port <- timer, mypool <-> mypool_res };\n"

-- A box allocated and freed to the same pool in the body of a loop: dual of
-- AE-003 inside a for loop.
poolUsedInLoop :: String
poolUsedInLoop =
  "task class PoolTask {\n" ++
  "    timer_port : sink TimeVal triggers tick;\n" ++
  "    mypool : access Allocator<u32>;\n" ++
  "    action tick(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
  "        var ret : Status<i32> = Success;\n" ++
  "        for i : usize in 0 : usize .. 3 : usize {\n" ++
  "            var b : Option<box u32> = None;\n" ++
  "            self->mypool.alloc(&mut b);\n" ++
  "            match b {\n" ++
  "                case Some(x) => { self->mypool.free(x); }\n" ++
  "                case None => { }\n" ++
  "            }\n" ++
  "        }\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "};\n" ++
  periodicEmitter "timer" 1 ++
  "resource mypool_res : Pool<u32; 4>;\n" ++
  "#[priority(10)]\n" ++
  "task t : PoolTask = { timer_port <- timer, mypool <-> mypool_res };\n"

-- | A box parked in a deque by one action and freed to the pool it came from by
-- another one, also taken and put back into the deque: dual of AE-003 through a
-- deque, and the walk back through a deque that feeds itself has to end.
poolThroughDeque :: String
poolThroughDeque =
  "task class DequeTask {\n" ++
  "    timer_port : sink TimeVal triggers park;\n" ++
  "    drain_port : sink TimeVal triggers drain;\n" ++
  "    mypool : access Allocator<u32>;\n" ++
  "    pending : access DequeAccess<u32>;\n" ++
  "    action park(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
  "        var ret : Status<i32> = Success;\n" ++
  "        var old : Option<box u32> = None;\n" ++
  "        self->pending.take(&mut old);\n" ++
  "        match old {\n" ++
  "            case Some(o) => { self->pending.put(o); }\n" ++
  "            case None => {\n" ++
  "                var b : Option<box u32> = None;\n" ++
  "                self->mypool.alloc(&mut b);\n" ++
  "                match b {\n" ++
  "                    case Some(x) => { self->pending.put(x); }\n" ++
  "                    case None => { }\n" ++
  "                }\n" ++
  "            }\n" ++
  "        }\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "    action drain(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
  "        var ret : Status<i32> = Success;\n" ++
  "        var old : Option<box u32> = None;\n" ++
  "        self->pending.take(&mut old);\n" ++
  "        match old {\n" ++
  "            case Some(o) => { self->mypool.free(o); }\n" ++
  "            case None => { }\n" ++
  "        }\n" ++
  "        return ret;\n" ++
  "    }\n" ++
  "};\n" ++
  periodicEmitter "timer" 1 ++
  periodicEmitter "timer2" 2 ++
  "resource mypool_res : Pool<u32; 4>;\n" ++
  "resource pending_deque : Deque<u32; 1>;\n" ++
  "#[priority(10)]\n" ++
  "task t : DequeTask = { timer_port <- timer, drain_port <- timer2, mypool <-> mypool_res, pending <-> pending_deque };\n"

spec :: Spec
spec = describe "Architecture: sound wiring raises no error" $
  mapM_ (\(name, src) -> it name $ architectureError src `shouldSatisfy` isNothing)
    [ ("accepts a timer emitter wired to a task", timerWired)
    , ("accepts a channel wired source-to-target", channelWired)
    , ("accepts a resource used through an access port", resourceUsed)
    , ("accepts a pool used through an allocator port", poolUsed)
    , ("accepts a box allocated and freed to the same pool inside a loop", poolUsedInLoop)
    , ("accepts a box parked in a deque and freed to the pool it came from", poolThroughDeque)
    ]
