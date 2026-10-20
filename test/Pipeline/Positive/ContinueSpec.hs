-- | Full-pipeline golden for a continue: the continued action runs in place of
-- the rest of the current one, which then returns.
module Pipeline.Positive.ContinueSpec (spec) where

import Pipeline.Common
import Golden

import Test.Hspec

-- | A task whose first action continues to the second one on one branch and
-- returns on the other.
continueApp :: String
continueApp =
    "task class T {\n" ++
    "    timer_port : sink TimeVal triggers first_step;\n" ++
    "    count : u32;\n" ++
    "    action first_step(&priv self, _t : TimeVal) {\n" ++
    "        self->count = self->count + 1 : u32;\n" ++
    "        if (self->count > 3 : u32) {\n" ++
    "            continue self->second_step();\n" ++
    "        } else {\n" ++
    "            return;\n" ++
    "        }\n" ++
    "    }\n" ++
    "    action second_step(&priv self) {\n" ++
    "        self->count = 0 : u32;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : T = { timer_port <- timer, count = 0 };\n"

spec :: Spec
spec = describe "Full pipeline: continue" $
  it "calls the continued action and returns" $
    goldenC "continue_action" (buildAndRenderModule "test" [("test", continueApp)])
