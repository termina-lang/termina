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

spec :: Spec
spec = describe "Full pipeline: the test resources" $
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
      it "protects test_checks with a mutex at the ceiling of its two tasks" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "termina__mutex__init(test_checks__mutex_id,"
            unpack c `shouldContain` "test_checks._lock_type.type = termina__resource_lock_type__mutex;"
            unpack c `shouldContain` "test_checks._lock_type.mutex.mutex_id = test_checks__mutex_id;")
          (renderTestMainFile progArch)
