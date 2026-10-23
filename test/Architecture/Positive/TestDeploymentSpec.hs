-- | Architecture positive tests of the deployment of a test: the unit wired to
-- the components of a unit test and to the test instances, and the scenario
-- of a concurrent test, pass the checks of the architecture.
module Architecture.Positive.TestDeploymentSpec (spec) where

import Architecture.Negative.CodeSpec
  (monitorClass, monitorTestClass, monitorTest, executorClass, executorTestClass,
   executorWiring, scenarioClass, periodicEmitter)
import ControlFlow.Architecture.Types (ResourceLock(..))
import ControlFlow.Architecture.Utils (genResourceLockings)
import Pipeline.Common (runTestArchitecture, Failure(..))

import qualified Data.Map.Strict as M
import Test.Hspec

-- | The lock of each resource of the deployment, or the message of the first
-- error
lockings :: String -> Either String (M.Map String ResourceLock)
lockings src = case runTestArchitecture src of
  Left err -> Left (show (failCode err))
  Right arch -> Right (genResourceLockings arch)

-- | A double that the case programs and two tasks of the unit read
sharedDouble :: String
sharedDouble =
  "interface IValue {\n" ++
  "    procedure get(&self, v : &mut u32);\n" ++
  "};\n" ++
  "interface IFakeValue {\n" ++
  "    procedure set(&mut self, v : u32);\n" ++
  "};\n" ++
  "resource class CFakeValue provides IValue, IFakeValue {\n" ++
  "    value : u32;\n" ++
  "    procedure get(&self, v : &mut u32) { *v = self->value; return; }\n" ++
  "    procedure set(&mut self, v : u32) { self->value = v; return; }\n" ++
  "};\n" ++
  "task class CReader {\n" ++
  "    tick_ev : sink TimeVal triggers sample;\n" ++
  "    value : access IValue;\n" ++
  "    out_value : out u32;\n" ++
  "    action sample(&priv self, _now : TimeVal) {\n" ++
  "        var v : u32 = 0;\n" ++
  "        self->value.get(&mut v);\n" ++
  "        self->out_value.send(v);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "test class CReaderTests {\n" ++
  "    value : access IFakeValue;\n" ++
  "    tick_a : access TestEmitterAccess<TimeVal>;\n" ++
  "    out_a : access TestChannelAccess<u32>;\n" ++
  "    tick_b : access TestEmitterAccess<TimeVal>;\n" ++
  "    out_b : access TestChannelAccess<u32>;\n" ++
  "    t : access TestAPI;\n" ++
  "    case reads(&mut self) {\n" ++
  "        var n : usize = 0;\n" ++
  "        self->value.set(3);\n" ++
  "        self->tick_a.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
  "        self->out_a.pending(&mut n);\n" ++
  "        self->t.check_eq_usize(n, 1 : usize);\n" ++
  "        self->tick_b.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
  "        self->out_b.pending(&mut n);\n" ++
  "        self->t.check_eq_usize(n, 1 : usize);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "resource value : CFakeValue = { value = 0 };\n" ++
  "emitter tick_a : TestEmitter<TimeVal>;\n" ++
  "emitter tick_b : TestEmitter<TimeVal>;\n" ++
  "channel out_a : TestChannel<u32; 4>;\n" ++
  "channel out_b : TestChannel<u32; 4>;\n"

spec :: Spec
spec = describe "Architecture: deployment of a test" $ do

  it "accepts a task with a test emitter and a test channel" $
    lockings (monitorClass "CMonitor" ++ monitorTestClass
              ++ "emitter tick : TestEmitter<TimeVal>;\n"
              ++ "channel alarms : TestChannel<u32; 4>;\n"
              ++ "#[priority(5)]\n"
              ++ "task uut : CMonitor = { tick_ev <- tick, alarm_out -> alarms };\n"
              ++ monitorTest)
      `shouldBe` Right M.empty

  it "accepts a box sent to a test channel and freed by the case to its pool" $
    M.lookup "tm_pool" <$> lockings (executorClass ++ executorTestClass True
              ++ "resource tm_pool : TestPool<TMPacket; 4>;\n"
              ++ executorWiring True)
      `shouldBe` Right (Just ResourceLockNone)

  it "does not count the test instances as users of a resource" $ do
    -- One task and the case: the resource keeps the protection of one task
    let oneTask = sharedDouble
          ++ "#[priority(5)]\n"
          ++ "task reader_a : CReader = { tick_ev <- tick_a, value <-> value, out_value -> out_a };\n"
          ++ "#[priority(6)]\n"
          ++ "task reader_b : CMonitor = { tick_ev <- tick_b, alarm_out -> out_b };\n"
          ++ "test reader_tests : CReaderTests = { value <-> value, tick_a <-> tick_a, out_a <-> out_a,\n"
          ++ "    tick_b <-> tick_b, out_b <-> out_b, t <-> test_entry };\n"
    M.lookup "value" <$> lockings (monitorClass "CMonitor" ++ oneTask) `shouldBe` Right (Just ResourceLockNone)
    -- Two tasks and the case: the ceiling of the two tasks
    let twoTasks = sharedDouble
          ++ "#[priority(5)]\n"
          ++ "task reader_a : CReader = { tick_ev <- tick_a, value <-> value, out_value -> out_a };\n"
          ++ "#[priority(6)]\n"
          ++ "task reader_b : CReader = { tick_ev <- tick_b, value <-> value, out_value -> out_b };\n"
          ++ "test reader_tests : CReaderTests = { value <-> value, tick_a <-> tick_a, out_a <-> out_a,\n"
          ++ "    tick_b <-> tick_b, out_b <-> out_b, t <-> test_entry };\n"
    M.lookup "value" <$> lockings twoTasks `shouldBe` Right (Just (ResourceLockMutex 5))

  it "accepts the scenario of a concurrent test that calls finish" $
    lockings (scenarioClass "finish()"
              ++ periodicEmitter "timer" 1
              ++ "#[priority(5)]\n"
              ++ "task scenario : CScenario = { tick_ev <- timer, t <-> test_entry };\n")
      `shouldBe` Right M.empty
