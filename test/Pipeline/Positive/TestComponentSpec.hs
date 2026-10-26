-- | Full-pipeline tests of the components of a unit test: the calls of a case
-- to a TestEmitter and a TestChannel, and the wiring of the ports to the
-- functions of the runtime, a TestPool included.
module Pipeline.Positive.TestComponentSpec (spec) where

import Pipeline.Common

import qualified Data.Map.Strict as M
import Data.Text (unpack)
import Test.Hspec

-- | A task that answers each tick with a box to a channel, and a case that
-- fires the tick, reads the channel and frees the box to the pool.
unitTest :: String
unitTest =
    "struct TMPacket {\n" ++
    "    service : u8;\n" ++
    "};\n" ++
    "task class CExecutor {\n" ++
    "    tick : sink TimeVal triggers run;\n" ++
    "    tm_out : out box TMPacket;\n" ++
    "    tm_pool : access Allocator<TMPacket>;\n" ++
    "    action run(&priv self, _t : TimeVal) {\n" ++
    "        var opt : Option<box TMPacket> = None;\n" ++
    "        self->tm_pool.alloc(&mut opt);\n" ++
    "        match opt {\n" ++
    "            case Some(b) => {\n" ++
    "                b.service = 1;\n" ++
    "                self->tm_out.send(b);\n" ++
    "            }\n" ++
    "            case None => {}\n" ++
    "        }\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CExecutorTests {\n" ++
    "    tick : access TestEmitterAccess<TimeVal>;\n" ++
    "    tms : access TestChannelAccess<box TMPacket>;\n" ++
    "    tm_pool : access Allocator<TMPacket>;\n" ++
    "    pool_state : access TestPoolAccess;\n" ++
    "    t : access TestChecks;\n" ++
    "    case fires(&mut self) {\n" ++
    "        var n : usize = 0;\n" ++
    "        let now : TimeVal = {tv_sec = 1, tv_usec = 0} : TimeVal;\n" ++
    "        var tm_opt : Option<box TMPacket> = None;\n" ++
    "        self->pool_state.fail_allocs(1 : usize);\n" ++
    "        self->tick.fire(now);\n" ++
    "        self->tick.fire({tv_sec = 2, tv_usec = 0} : TimeVal);\n" ++
    "        self->tms.pending(&mut n);\n" ++
    "        self->t.check_eq_usize(n, 1 : usize);\n" ++
    "        self->tms.take(&mut tm_opt);\n" ++
    "        match tm_opt {\n" ++
    "            case Some(b) => {\n" ++
    "                self->tm_pool.free(b);\n" ++
    "            }\n" ++
    "            case None => {\n" ++
    "                self->t.fail();\n" ++
    "            }\n" ++
    "        }\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter tick : TestEmitter<TimeVal>;\n" ++
    "channel tms : TestChannel<box TMPacket; 4>;\n" ++
    "resource tm_pool : TestPool<TMPacket; 4>;\n" ++
    "#[priority(5)]\n" ++
    "task uut : CExecutor = { tick <- tick, tm_out -> tms, tm_pool <-> tm_pool };\n" ++
    "test exec_tests : CExecutorTests = { tick <-> tick, tms <-> tms, tm_pool <-> tm_pool,\n" ++
    "    pool_state <-> tm_pool, t <-> test_checks };\n"

spec :: Spec
spec = describe "Full pipeline: the components of a unit test" $ do
  it "calls fire, pending and take with the functions of the runtime" $
    case runTestBuild unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right rendered ->
        maybe (expectationFailure "no module test")
          (\c -> do
            -- A variable is fired by address, a value through a temporary
            unpack c `shouldContain` "self->tick.fire(termina__ev, self->tick._that, &now);"
            unpack c `shouldContain` "        const TimeVal value = { .tv_sec = 2U, .tv_usec = 0U };\n        self->tick.fire(termina__ev, self->tick._that, &value);"
            unpack c `shouldContain` "self->tms.pending(termina__ev, self->tms._that, &n);"
            unpack c `shouldContain` "self->tms.take(termina__ev, self->tms._that, &tm_opt._variant,\n                   &tm_opt.Some._0);"
            unpack c `shouldContain` "termina__test__emitter_t tick;"
            unpack c `shouldContain` "termina__msg_queue_t tms;"
            unpack c `shouldContain` "termina__test__pool_t tm_pool;")
          (M.lookup "test" rendered)
  it "gives the harness, the test emitters, the queues of the test channel and the test pool their identifiers" $
    case runTestApp unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "#define system_except__emitter_id 0\n#define termina__test__harness__emitter_id 1\n#define tick__emitter_id 2\n"
            unpack c `shouldContain` "#define uut__task_id 0\n#define termina__test__harness__task_id 1\n"
            unpack c `shouldContain` "#define tms__channel_msg_queue_id 2\n#define tms__event_msg_queue_id 3\n"
            unpack c `shouldContain` "#define tm_pool__pool_id 0\n"
            unpack c `shouldContain` "#define TERMINA__APP_CONFIG__POOLS 1\n"
            unpack c `shouldContain` "#define TERMINA__APP_CONFIG__TASKS 2\n"
            unpack c `shouldContain` "#define TERMINA__APP_CONFIG__MESSAGE_QUEUES 4\n")
          (renderConfigFile progArch)
  it "creates the two queues of the test channel" $
    case runTestApp unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "termina__msg_queue__init(tms__channel_msg_queue_id,\n                                 sizeof(termina__box_t), 4U, status);"
            unpack c `shouldContain` "termina__msg_queue__init(tms__event_msg_queue_id,\n                                 sizeof(termina__event_t), 4U, status);")
          (renderTestMainFile progArch)
  it "wires the ports to the functions of the runtime" $
    case runTestApp unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (_, prjprogs) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "tm_pool.pool._lock_type.type = termina__resource_lock_type__none;"
            unpack c `shouldContain` "uut.tm_out = &tms;"
            unpack c `shouldContain` "uut.tm_pool._that = &tm_pool;\n    uut.tm_pool.alloc = &termina__test__pool__alloc;\n    uut.tm_pool.free = &termina__test__pool__free;"
            -- Only the procedure of TestPoolAccess that the case uses
            unpack c `shouldContain` "exec_tests.pool_state._that = &tm_pool;\n    exec_tests.pool_state.fail_allocs = &termina__test__pool__fail_allocs;\n"
            unpack c `shouldNotContain` "in_use"
            unpack c `shouldContain` "exec_tests.tick._that = &tick;\n    exec_tests.tick.fire = &termina__test__emitter__fire;"
            unpack c `shouldContain` "exec_tests.tms._that = &tms;\n    exec_tests.tms.pending = &termina__test__channel__pending;\n    exec_tests.tms.take = &termina__test__channel__take;")
          (renderInitFile prjprogs)
