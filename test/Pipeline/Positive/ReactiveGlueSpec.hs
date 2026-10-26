-- | Full-pipeline golden for the application glue that installs function
-- pointers. A reactive application (a periodic task reaching a resource through
-- an access port) drives the two glue sites that store a function's address:
-- the task entry passed to @termina__task__init@ (rendered into the main file)
-- and the resource procedure stored into the task's access-port field (rendered
-- into the init file). Both must be emitted as @&f@, the form MISRA-C:2023 Rule
-- 17.12 requires for taking the address of a function. No other golden in the
-- suite exercises the app-level glue, so without this a change there is
-- unobserved.
module Pipeline.Positive.ReactiveGlueSpec (spec) where

import Pipeline.Common
import Golden
import Configuration.Platform (Platform(..))

import Data.Text (unpack)
import Test.Hspec

-- | A minimal but complete reactive application: one resource, one periodic
-- task that reaches the resource through an access port, wired together.
reactiveApp :: String
reactiveApp =
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
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->foo.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "resource foo_res : FooRes = { value = 0 };\n" ++
    "#[priority(10)]\n" ++
    "task t : UserTask = { timer_port <- timer, foo <-> foo_res };\n"

spec :: Spec
spec = describe "Full pipeline: reactive application glue" $
  case runFullProjectApp [("test", reactiveApp)] of
    Left err ->
      it "builds the reactive application" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, prjprogs) -> do
      it "installs the task entry as an explicit function pointer (main file)" $
        either (expectationFailure . unpack) (goldenC "reactive_glue_main")
          (renderMainFile progArch)
      it "installs the resource procedure as an explicit function pointer (init file)" $
        either (expectationFailure . unpack) (goldenC "reactive_glue_init")
          (renderInitFile prjprogs)
      it "gives a task without stack_size the default stack size of the platform" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain` "termina__task__init(t__task_id, 10, 2048U,")
          (renderMainFileOn FreeRTOS10STM32L432NUCLEOL432KC progArch)
      it "reserves emitter identifier 0 for system_except, connected or not" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "#define system_except__emitter_id 0\n"
            unpack c `shouldContain` "#define timer__emitter_id 1\n")
          (renderConfigFile progArch)
      it "reserves handler identifier 0 for the handler of system_except of the runtime" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain` "#define system_except__handler_id 0\n")
          (renderConfigFile progArch)
