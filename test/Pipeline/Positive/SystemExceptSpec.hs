-- | Full-pipeline coverage of the registration of @system_except@. The
-- generator builds the connection of the handler in
-- @termina__app__init_emitters@ and hands it to the runtime with
-- @termina__except__init_emitter@, which raises the exceptions through it.
-- Until this spec existed the emitter never entered the architecture, so no
-- registration was emitted and an exception rebooted without calling the
-- handler.
--
-- The second case is the other half of the contract: only a handler may attend
-- this emitter, which the type checker rejects with SE-214.
module Pipeline.Positive.SystemExceptSpec (spec) where

import Pipeline.Common
import Golden

import Data.Text (pack, unpack)
import Test.Hspec

-- | The action the entity runs on an exception. It returns nothing, because
-- the system reboots as soon as it finishes.
faultAction :: String
faultAction =
    "    fault_ev : sink Exception triggers on_fault;\n" ++
    "    action on_fault(&priv self, _e : Exception) {\n" ++
    "        self->count = self->count + 1;\n" ++
    "        return;\n" ++
    "    }\n"

-- | An application whose @system_except@ reaches a handler, which is the only
-- shape the language admits.
handlerApp :: String
handlerApp =
    "handler class FaultHandler {\n" ++
    "    count : u32;\n" ++
    faultAction ++
    "};\n" ++
    "handler fault_hdlr : FaultHandler = { count = 0, fault_ev <- system_except };\n"

-- | The same application with a task in place of the handler.
taskApp :: String
taskApp =
    "task class FaultTask {\n" ++
    "    count : u32;\n" ++
    faultAction ++
    "};\n" ++
    "#[priority(10)]\n" ++
    "task fault_task : FaultTask = { count = 0, fault_ev <- system_except };\n"

spec :: Spec
spec = describe "Full pipeline: the system exception" $ do

  case runFullProjectApp [("test", handlerApp)] of
    Left err ->
      it "builds an application whose system_except reaches a handler" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) -> do
      it "registers the handler as a connection handed to the runtime" $
        either (expectationFailure . unpack) (goldenC "system_except_handler")
          (renderMainFile progArch)
      it "gives the handler of the application identifier 1, after the 0 of the runtime" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "#define system_except__handler_id 0\n"
            unpack c `shouldContain` "#define fault_hdlr__handler_id 1\n")
          (renderConfigFile progArch)

  it "SE-214: rejects a system exception attended by a task" $
    compileProjectErrorCode [("test", taskApp)]
      `shouldBe` Just (pack "SE-214")
