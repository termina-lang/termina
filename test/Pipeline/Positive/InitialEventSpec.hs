-- | Full-pipeline coverage of @termina__app__initial_event@, the function the
-- generator emits when an application connects @system_init@. It builds the
-- connection and hands it to the runtime, which dispatches the event; nothing
-- else in the suite renders it, and the function spent a while emitting a call
-- with two arguments and a dereferenced TimeVal, which does not compile,
-- without a single test turning red.
--
-- The second case is the other half of the contract: only a handler may attend
-- this emitter, which the type checker rejects with SE-220. Before that rule
-- existed the generator carried a second branch for a task target, unreachable
-- in practice and broken for as long as it existed.
module Pipeline.Positive.InitialEventSpec (spec) where

import Pipeline.Common
import Golden

import Data.Text (pack, unpack)
import Test.Hspec

-- | The action the entity runs on the initial event.
bootAction :: String
bootAction =
    "    boot_ev : sink TimeVal triggers boot;\n" ++
    "    action boot(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
    "        var ret : Status<i32> = Success;\n" ++
    "        self->count = self->count + 1;\n" ++
    "        return ret;\n" ++
    "    }\n"

-- | An application whose @system_init@ reaches a handler, which is the only
-- shape the language admits.
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

spec :: Spec
spec = describe "Full pipeline: the initial event" $ do

  case runFullProjectAppWith systemInitConfig [("test", handlerApp)] of
    Left err ->
      it "builds an application whose system_init reaches a handler" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) ->
      it "emits the initial event as a connection handed to the runtime" $
        either (expectationFailure . unpack) (goldenC "initial_event_handler")
          (renderMainFileWith systemInitConfig progArch)

  it "SE-220: rejects an initial event attended by a task" $
    compileProjectErrorCodeWith systemInitConfig [("test", taskApp)]
      `shouldBe` Just (pack "SE-220")
