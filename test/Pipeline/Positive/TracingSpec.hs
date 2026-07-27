-- | Tracing profile: the labels the generator emits at the entry and the exits
-- of every function it produces. The fixture covers the four kinds of member
-- function plus a free function, and an action with two exits, one of them a
-- @continue@.
module Pipeline.Positive.TracingSpec (spec) where

import Data.Text (isInfixOf, pack)

import Configuration.Configuration (ProjectProfile(..))
import Pipeline.Common
import Pipeline.Golden

import Test.Hspec

-- | A resource with a procedure (which takes the lock) and a method, a task
-- with one single-exit action and one two-exit action that chains to it, and a
-- free function.
testTracing :: String
testTracing =
    "interface ICounter {\n" ++
    "    procedure increment(&mut self, status : &mut Status<i32>);\n" ++
    "};\n" ++
    "\n" ++
    "resource class CCounter provides ICounter {\n" ++
    "    count : u32;\n" ++
    "\n" ++
    "    procedure increment(&mut self, status : &mut Status<i32>) {\n" ++
    "        self->count = self->count + 1 : u32;\n" ++
    "        *status = Success;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "\n" ++
    "    method double(&self) -> u32 {\n" ++
    "        return self->count * 2 : u32;\n" ++
    "    }\n" ++
    "};\n" ++
    "\n" ++
    "task class CWorker {\n" ++
    "\n" ++
    "    tick : sink TimeVal triggers on_tick;\n" ++
    "\n" ++
    "    threshold : u32;\n" ++
    "\n" ++
    "    action on_tick(&priv self, _current : TimeVal) -> Status<i32> {\n" ++
    "        if (self->threshold == 0 : u32) {\n" ++
    "            var failed : Status<i32> = Failure(1 : i32);\n" ++
    "            return failed;\n" ++
    "        } else {\n" ++
    "            continue self->on_retry();\n" ++
    "        }\n" ++
    "    }\n" ++
    "\n" ++
    "    action on_retry(&priv self) -> Status<i32> {\n" ++
    "        var result : Status<i32> = Success;\n" ++
    "        return result;\n" ++
    "    }\n" ++
    "\n" ++
    "};\n" ++
    "\n" ++
    "function scale(value : u32) -> u32 {\n" ++
    "    return value * 2 : u32;\n" ++
    "}"

spec :: Spec
spec = do
  describe "Full pipeline under the tracing profile" $ do
    it "Labels the entry and the exits of every generated function" $
      goldenC "tracing_labels" (runFullBuildWithProfile Tracing testTracing)
    it "Numbers the exits of an action that has more than one" $ do
      let generated = runFullBuildWithProfile Tracing testTracing
      generated `shouldSatisfy`
        isInfixOf (pack "termina__CWorker__on_tick__exit__0")
      generated `shouldSatisfy`
        isInfixOf (pack "termina__CWorker__on_tick__exit__1")
    it "Emits no label under the release profile" $
      runFullBuildWithProfile Release testTracing `shouldSatisfy`
        (not . isInfixOf (pack "__asm__"))
    it "Emits no label under the debug profile" $
      runFullBuildWithProfile Debug testTracing `shouldSatisfy`
        (not . isInfixOf (pack "__asm__"))
