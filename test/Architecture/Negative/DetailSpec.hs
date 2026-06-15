{-# LANGUAGE LambdaCase #-}
-- | Architecture negative tests, *detail* flavour: assert the exact identifier
-- the wiring error blames, not just the AE-NNN code.
module Architecture.Negative.DetailSpec (spec) where

import Architecture.Common (architectureError)
import Architecture.Negative.CodeSpec (timerTaskClass, periodicEmitter)
import ControlFlow.Architecture.Errors (Error(..))
import Utils.Annotations (AnnotatedError(..))

import Test.Hspec

-- AE-004: two emitters, only @timer@ is wired, so @spare@ is left disconnected.
disconnectedSpareEmitter :: String
disconnectedSpareEmitter =
  timerTaskClass ++ periodicEmitter "timer" 1 ++ periodicEmitter "spare" 2
  ++ "#[priority(10)]\n"
  ++ "task t : TimerTask = { timer_port <- timer };\n"

-- AE-008: a pool nobody accesses.
unusedPool :: String
unusedPool =
  timerTaskClass ++ periodicEmitter "timer" 1
  ++ "#[priority(10)]\n"
  ++ "task t : TimerTask = { timer_port <- timer };\n"
  ++ "resource mypool : Pool<u32; 4>;\n"

spec :: Spec
spec = describe "Architecture: error detail (blamed identifier)" $ do
  it "AE-004 names the disconnected emitter" $
    architectureError disconnectedSpareEmitter `shouldSatisfy` \case
      Just (AnnotatedError (EDisconnectedEmitter "spare") _) -> True
      _ -> False
  it "AE-008 names the unused pool" $
    architectureError unusedPool `shouldSatisfy` \case
      Just (AnnotatedError (EUnusedPool "mypool") _) -> True
      _ -> False
