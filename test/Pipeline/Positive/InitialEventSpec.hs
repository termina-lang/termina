-- | Full-pipeline goldens for @termina__app__initial_event@, the function the
-- generator emits when an application connects @system_init@. It has two
-- shapes, one per kind of entity the emitter can reach, and they are written by
-- two separate branches of the generator: a handler, whose action takes the
-- event, and a task, whose action takes it too but through a different call
-- site. Nothing else in the suite renders either of them, and the task branch
-- spent a while emitting a call with two arguments and a dereferenced TimeVal,
-- which does not compile, without a single test turning red.
module Pipeline.Positive.InitialEventSpec (spec) where

import Pipeline.Common
import Golden

import Data.Text (unpack)
import Test.Hspec

-- | The action both entities run on the initial event. Written once, since the
-- two applications differ only in the class that carries it.
bootAction :: String
bootAction =
    "    boot_ev : sink TimeVal triggers boot;\n" ++
    "    action boot(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
    "        var ret : Status<i32> = Success;\n" ++
    "        self->count = self->count + 1;\n" ++
    "        return ret;\n" ++
    "    }\n"

-- | An application whose @system_init@ reaches a handler.
handlerApp :: String
handlerApp =
    "handler class BootHandler {\n" ++
    "    count : u32;\n" ++
    bootAction ++
    "};\n" ++
    "handler boot_hdlr : BootHandler = { count = 0, boot_ev <- system_init };\n"

-- | The same application with a task in place of the handler.
taskApp :: String
taskApp =
    "task class BootTask {\n" ++
    "    count : u32;\n" ++
    bootAction ++
    "};\n" ++
    "#[priority(10)]\n" ++
    "task boot_task : BootTask = { count = 0, boot_ev <- system_init };\n"

-- | Render the main file of an application under a configuration that has the
-- system-init emitter switched on, which is what makes @system_init@ nameable.
mainFileOf :: String -> String -> Spec
mainFileOf name source =
  case runFullProjectAppWith systemInitConfig [("test", source)] of
    Left err ->
      it ("builds " ++ name) $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) ->
      it ("emits the initial event of " ++ name) $
        either (expectationFailure . unpack) (goldenC name)
          (renderMainFileWith systemInitConfig progArch)

spec :: Spec
spec = describe "Full pipeline: the initial event" $ do
  mainFileOf "initial_event_handler" handlerApp
  mainFileOf "initial_event_task" taskApp
