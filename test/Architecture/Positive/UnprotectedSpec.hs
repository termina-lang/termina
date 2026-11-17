-- | Architecture positive tests of unprotected resources: a resource reached
-- through one or more unprotected resources gets the protection of the
-- entities that reach it, and none when only a test instance reaches it.
module Architecture.Positive.UnprotectedSpec (spec) where

import Architecture.Negative.CodeSpec (periodicEmitter)
import ControlFlow.Architecture.Types (ResourceLock(..))
import ControlFlow.Architecture.Utils (genResourceLockings)
import Pipeline.Common (runFullProjectApp, runTestArchitecture, Failure(..))

import qualified Data.Map.Strict as M
import Test.Hspec

-- | The lock of each resource of an application, or the code of the first error
appLockings :: String -> Either String (M.Map String ResourceLock)
appLockings src = case runFullProjectApp [("test", src)] of
  Left err -> Left (show (failCode err))
  Right (arch, _) -> Right (genResourceLockings arch)

-- | The lock of each resource of the deployment of a test, or the code of the
-- first error
testLockings :: String -> Either String (M.Map String ResourceLock)
testLockings src = case runTestArchitecture src of
  Left err -> Left (show (failCode err))
  Right arch -> Right (genResourceLockings arch)

-- | A resource with state, an unprotected resource that forwards to it, a
-- second unprotected resource that forwards to the first one, and a task
-- class that uses a forwarder
classes :: String
classes =
  "interface ICount {\n" ++
  "    procedure add(&mut self, v : u32);\n" ++
  "};\n" ++
  "resource class CInner provides ICount {\n" ++
  "    count : u32;\n" ++
  "    procedure add(&mut self, v : u32) { self->count = self->count + v; return; }\n" ++
  "};\n" ++
  "resource class CForward provides ICount {\n" ++
  "    next : access ICount;\n" ++
  "    procedure add(&mut self, v : u32) { self->next.add(v); return; }\n" ++
  "};\n" ++
  "task class CBeat {\n" ++
  "    tick_ev : sink TimeVal triggers beat;\n" ++
  "    counter : access ICount;\n" ++
  "    action beat(&priv self, _now : TimeVal) {\n" ++
  "        self->counter.add(1);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "resource inner : CInner = { count = 0 };\n"

-- | Two tasks of different priorities that use the resource named
twoTasks :: String -> String
twoTasks target =
  periodicEmitter "timer_a" 1 ++
  periodicEmitter "timer_b" 2 ++
  "#[priority(5)]\n" ++
  "task beat_a : CBeat = { tick_ev <- timer_a, counter <-> " ++ target ++ " };\n" ++
  "#[priority(6)]\n" ++
  "task beat_b : CBeat = { tick_ev <- timer_b, counter <-> " ++ target ++ " };\n"

spec :: Spec
spec = describe "Architecture: unprotected resources" $ do

  it "leaves unprotected a resource that one task reaches through an unprotected resource" $
    M.lookup "inner" <$> appLockings (classes
      ++ "#[unprotected]\nresource mid : CForward = { next <-> inner };\n"
      ++ periodicEmitter "timer_a" 1
      ++ "#[priority(5)]\n"
      ++ "task beat_a : CBeat = { tick_ev <- timer_a, counter <-> mid };\n")
      `shouldBe` Right (Just ResourceLockNone)

  it "gives the ceiling of two tasks to a resource reached through an unprotected resource" $
    M.lookup "inner" <$> appLockings (classes
      ++ "#[unprotected]\nresource mid : CForward = { next <-> inner };\n"
      ++ twoTasks "mid")
      `shouldBe` Right (Just (ResourceLockMutex 5))

  it "gives the ceiling of two tasks to a resource reached through two unprotected resources" $
    M.lookup "inner" <$> appLockings (classes
      ++ "#[unprotected]\nresource mid : CForward = { next <-> inner };\n"
      ++ "#[unprotected]\nresource outer : CForward = { next <-> mid };\n"
      ++ twoTasks "outer")
      `shouldBe` Right (Just (ResourceLockMutex 5))

  it "leaves unprotected a resource that only a test instance reaches through an unprotected resource" $
    M.lookup "inner" <$> testLockings (classes
      ++ "#[unprotected]\nresource mid : CForward = { next <-> inner };\n"
      ++ "test class CForwardTests {\n"
      ++ "    counter : access ICount;\n"
      ++ "    t : access TestChecks;\n"
      ++ "    case adds(&mut self) {\n"
      ++ "        self->counter.add(1);\n"
      ++ "        self->t.check(true);\n"
      ++ "        return;\n"
      ++ "    }\n"
      ++ "};\n"
      ++ "test forward_tests : CForwardTests = { counter <-> mid, t <-> test_checks };\n")
      `shouldBe` Right (Just ResourceLockNone)
