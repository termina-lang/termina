-- | Full-pipeline golden for the calls to system_entry. Its access port carries
-- _that like any other, init.c points it to system_entry, and a call passes it
-- after the event, both for a procedure with &mut self (print) and for one with
-- &self (clock_get_uptime).
module Pipeline.Positive.SystemEntrySpec (spec) where

import Pipeline.Common
import Golden

import qualified Data.Map.Strict as M
import Data.Text (unpack)
import Test.Hspec

-- | A periodic task that reads the time and prints it through system_entry
systemApp :: String
systemApp =
    "task class UserTask {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    sys : access SystemAPI;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var now : TimeVal = { tv_sec = 0, tv_usec = 0 };\n" ++
    "        self->sys.clock_get_uptime(&mut now);\n" ++
    "        self->sys.println_u32(now.tv_sec, SysPrintBase::Decimal);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : UserTask = { timer_port <- timer, sys <-> system_entry };\n"

spec :: Spec
spec = describe "Full pipeline: system_entry" $ do
  case runFullProjectApp [("test", systemApp)] of
    Left err ->
      it "builds an application that reaches system_entry" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (_, prjprogs) ->
      it "points the access port to system_entry (init file)" $
        either (expectationFailure . unpack) (goldenC "system_entry_init")
          (renderInitFile prjprogs)
  case runFullProjectBuild [("test", systemApp)] of
    Left err ->
      it "generates the module that reaches system_entry" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right modules ->
      it "passes _that after the event in each call (source file)" $
        maybe (expectationFailure "no module test") (goldenC "system_entry_source")
          (M.lookup "test" modules)
