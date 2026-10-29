-- | Full-pipeline tests of test_checks, test_clock and test_scenario, which are
-- generated as system_entry is: the init file points the access ports to the
-- functions of the runtime named after the class, and the main file writes the
-- protection of the resource when it needs one.
module Pipeline.Positive.TestResourceSpec (spec) where

import Pipeline.Common

import Data.Text (unpack)
import Test.Hspec

-- | Two tasks of a concurrent test that record checks, so test_checks takes
-- the ceiling of both, and one of them ends the scenario.
concurrentTest :: String
concurrentTest =
    "task class CChecker {\n" ++
    "    tick_ev : sink TimeVal triggers run;\n" ++
    "    t : access TestChecks;\n" ++
    "    action run(&priv self, _now : TimeVal) {\n" ++
    "        self->t.check(true);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "task class CEnder {\n" ++
    "    tick_ev : sink TimeVal triggers run;\n" ++
    "    t : access TestChecks;\n" ++
    "    s : access TestScenario;\n" ++
    "    action run(&priv self, _now : TimeVal) {\n" ++
    "        self->t.step(1 : usize);\n" ++
    "        self->s.finish();\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer_a : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "emitter timer_b : PeriodicTimer = { period = {tv_sec = 2, tv_usec = 0} };\n" ++
    "#[priority(5)]\n" ++
    "task checker : CChecker = { tick_ev <- timer_a, t <-> test_checks };\n" ++
    "#[priority(6)]\n" ++
    "task ender : CEnder = { tick_ev <- timer_b, t <-> test_checks, s <-> test_scenario };\n"

-- | Two tasks of a unit test that record checks, so test_checks takes the ceiling of both
unitTest :: String
unitTest =
    "task class CChecker {\n" ++
    "    tick_ev : sink TimeVal triggers run;\n" ++
    "    t : access TestChecks;\n" ++
    "    action run(&priv self, _now : TimeVal) {\n" ++
    "        self->t.check(true);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CCheckerTests {\n" ++
    "    tick_a : access TestEmitterAccess<TimeVal>;\n" ++
    "    tick_b : access TestEmitterAccess<TimeVal>;\n" ++
    "    case fires(&mut self) {\n" ++
    "        self->tick_a.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
    "        self->tick_b.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter tick_a : TestEmitter<TimeVal>;\n" ++
    "emitter tick_b : TestEmitter<TimeVal>;\n" ++
    "#[priority(5)]\n" ++
    "task checker_a : CChecker = { tick_ev <- tick_a, t <-> test_checks };\n" ++
    "#[priority(6)]\n" ++
    "task checker_b : CChecker = { tick_ev <- tick_b, t <-> test_checks };\n" ++
    "test checker_tests : CCheckerTests = { tick_a <-> tick_a, tick_b <-> tick_b };\n"

spec :: Spec
spec = describe "Full pipeline: the test resources" $ do
  case runTestApp concurrentTest of
    Left err ->
      it "builds a concurrent test" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, prjprogs) -> do
      it "points the access ports to the functions of the runtime" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "checker.t._that = &test_checks;"
            unpack c `shouldContain` "checker.t.check = &TestChecksEntry__check;"
            unpack c `shouldContain` "ender.t.step = &TestChecksEntry__step;"
            unpack c `shouldContain` "ender.s._that = &test_scenario;"
            unpack c `shouldContain` "ender.s.finish = &TestScenarioEntry__finish;")
          (renderInitFile prjprogs)
      it "creates the queue of the harness, the harness and the hook of a concurrent test" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "termina__test__scenario__init(termina__test__harness__msg_queue_id,\n                                      status);"
            unpack c `shouldContain` "termina__task__init(termina__test__harness__task_id, 254, 4096U,\n                            &termina__app__test_harness, NULL, status);"
            unpack c `shouldContain` "termina__test__scenario__harness();"
            unpack c `shouldContain` "except_connection.handler_action = &termina__test__except_hook;"
            unpack c `shouldContain` "termina__test__header_t termina__test__header;")
          (renderTestMainFile progArch)
      it "gives the harness and its queue their identifiers in a concurrent test" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "#define checker__task_id 0\n#define ender__task_id 1\n#define termina__test__harness__task_id 2\n"
            unpack c `shouldContain` "#define termina__test__harness__msg_queue_id 4\n"
            unpack c `shouldNotContain` "termina__test__harness__emitter_id")
          (renderTestConfigFile progArch)
  case runTestApp unitTest of
    Left err ->
      it "builds a unit test" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) ->
      it "protects test_checks with a mutex at the ceiling of its two tasks" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "termina__mutex__init(test_checks__mutex_id,"
            unpack c `shouldContain` "test_checks._lock_type.type = termina__resource_lock_type__mutex;"
            unpack c `shouldContain` "test_checks._lock_type.mutex.mutex_id = test_checks__mutex_id;")
          (renderTestMainFile progArch)
