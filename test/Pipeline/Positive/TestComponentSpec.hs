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

-- | A handler that forwards the value of its interrupt to a channel, and a
-- case that fires the interrupt and reads the channel.
handlerTest :: String
handlerTest =
    "handler class CRx {\n" ++
    "    rx : sink u32 triggers on_rx;\n" ++
    "    rx_out : out u32;\n" ++
    "    action on_rx(&priv self, v : u32) {\n" ++
    "        self->rx_out.send(v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CRxTests {\n" ++
    "    irq : access TestEmitterAccess<u32>;\n" ++
    "    rxs : access TestChannelAccess<u32>;\n" ++
    "    t : access TestChecks;\n" ++
    "    case forwards(&mut self) {\n" ++
    "        var n : usize = 0;\n" ++
    "        self->irq.fire(7 : u32);\n" ++
    "        self->rxs.pending(&mut n);\n" ++
    "        self->t.check_eq_usize(n, 1 : usize);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter irq : TestEmitter<u32>;\n" ++
    "channel rxs : TestChannel<u32; 2>;\n" ++
    "handler rx : CRx = { rx <- irq, rx_out -> rxs };\n" ++
    "test rx_tests : CRxTests = { irq <-> irq, rxs <-> rxs, t <-> test_checks };\n"

-- | Two instances and two cases declared against the alphabetical order
orderTest :: String
orderTest =
    "task class CTicker {\n" ++
    "    tick : sink TimeVal triggers run;\n" ++
    "    count : u32;\n" ++
    "    action run(&priv self, _t : TimeVal) {\n" ++
    "        self->count = self->count + 1;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CTickerTests {\n" ++
    "    tick : access TestEmitterAccess<TimeVal>;\n" ++
    "    case zeta(&mut self) {\n" ++
    "        self->tick.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "    case alpha(&mut self) {\n" ++
    "        self->tick.fire({tv_sec = 2, tv_usec = 0} : TimeVal);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter tick : TestEmitter<TimeVal>;\n" ++
    "#[priority(5)]\n" ++
    "task ticker : CTicker = { count = 0, tick <- tick };\n" ++
    "test b_tests : CTickerTests = { tick <-> tick };\n" ++
    "test a_tests : CTickerTests = { tick <-> tick };\n"

-- | A test of a function, whose test.fin declares only a test class and its instance
functionTest :: String
functionTest =
    "function sum(a : u32, b : u32) -> u32 {\n" ++
    "    return a + b;\n" ++
    "}\n" ++
    "test class CSumTests {\n" ++
    "    t : access TestChecks;\n" ++
    "    case adds(&mut self) {\n" ++
    "        self->t.check_eq_u32(sum(2 : u32, 3 : u32), 5 : u32);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test sum_tests : CSumTests = { t <-> test_checks };\n"

spec :: Spec
spec = describe "Full pipeline: the components of a unit test" $ do
  it "includes the module of the test instances in the main file" $
    case runTestApp functionTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain` "#include \"test.h\"")
          (renderTestMainFile progArch)
  it "sets up the test pool, the test channel and the test emitter in the main file" $
    case runTestApp unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "static max_align_t termina__pool_memory__tm_pool["
            unpack c `shouldContain` "tm_pool.pool.pool_id = tm_pool__pool_id;"
            unpack c `shouldContain` "termina__test__pool__init(&tm_pool, (void *)termina__pool_memory__tm_pool,"
            unpack c `shouldContain` "tms.task_id = termina__test__harness__task_id;\n    tms.task_msg_queue_id = tms__event_msg_queue_id;\n    tms.channel_msg_queue_id = tms__channel_msg_queue_id;\n    tms.port_id = 0U;"
            unpack c `shouldContain` "tick.emitter_id = tick__emitter_id;\n    tick.type = termina__emitter_connection_type__task;\n    tick.task.task_id = uut__task_id;\n    tick.task.task_msg_queue_id = uut__task_msg_queue_id;\n    tick.task.sink_msgq_id = uut__tick__sink_msg_queue_id;\n    tick.task.sink_port_id = CExecutor__tick;"
            unpack c `shouldContain` "uut.tick = uut__tick__sink_msg_queue_id;")
          (renderTestMainFile progArch)
  it "runs the cases in the task of the harness, after the reset" $
    case runTestApp unitTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> do
            -- The record of the test, with stamp zero until the descriptor exists
            unpack c `shouldContain` "const uint32_t termina__test__stamp = 0U;"
            unpack c `shouldContain` "const uint32_t termina__test__capacity = 8192U;"
            unpack c `shouldContain` "termina__test__header_t termina__test__header;"
            unpack c `shouldContain` "uint8_t termina__test__area[8192U];"
            unpack c `shouldContain` "static void termina__app__test_harness(void * const arg);"
            unpack c `shouldContain` "termina__task__init(termina__test__harness__task_id, 254, 4096U,"
            unpack c `shouldContain` "&termina__app__test_harness, NULL, status);"
            -- The hook of the runtime attends system_except
            unpack c `shouldContain` "except_connection.handler_object = NULL;"
            unpack c `shouldContain` "except_connection.handler_id = system_except__handler_id;"
            unpack c `shouldContain` "except_connection.handler_action = &termina__test__except_hook;"
            unpack c `shouldContain` "termina__except__init_emitter(system_except__emitter_id,"
            -- The reset repeats the steps that only write memory
            unpack c `shouldContain` "static void termina__app__test_reset(void) {"
            unpack c `shouldContain` "termina__app__init_pools(&status);\n\n    termina__test__channel__flush(&tms);\n\n    termina__test__clock__reset();"
            unpack c `shouldContain` "termina__runtime_operation__test_reset,"
            -- The harness, with the event of the cases
            unpack c `shouldContain` ".emitter_id = termina__test__harness__emitter_id,"
            unpack c `shouldContain` ".owner.task.task_id = termina__test__harness__task_id,"
            unpack c `shouldContain` "if (termina__test__selected(1U)) {"
            unpack c `shouldContain` "termina__test__case_begin(1U);\n        termina__app__test_reset();\n        CExecutorTests__fires(&termina__ev, &exec_tests);\n        termina__test__case_end();"
            unpack c `shouldContain` "termina__test__end();")
          (renderTestMainFile progArch)
  it "numbers the cases in the order of declaration, as termina test --list" $
    case runTestApp orderTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "termina__test__case_begin(1U);\n        termina__app__test_reset();\n        CTickerTests__zeta(&termina__ev, &b_tests);"
            unpack c `shouldContain` "termina__test__case_begin(2U);\n        termina__app__test_reset();\n        CTickerTests__alpha(&termina__ev, &b_tests);"
            unpack c `shouldContain` "termina__test__case_begin(3U);\n        termina__app__test_reset();\n        CTickerTests__zeta(&termina__ev, &a_tests);"
            unpack c `shouldContain` "termina__test__case_begin(4U);\n        termina__app__test_reset();\n        CTickerTests__alpha(&termina__ev, &a_tests);")
          (renderTestMainFile progArch)
  it "reaches the action of a handler through a function of the main file" $
    case runTestApp handlerTest of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right (progArch, _) ->
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "static void termina__test_fire__irq(const termina__event_t * const termina__ev,\n                                    const void * const value) {\n    \n    CRx__on_rx(termina__ev, &rx, *(const uint32_t *)value);\n\n}"
            unpack c `shouldContain` "irq.emitter_id = irq__emitter_id;\n    irq.type = termina__emitter_connection_type__handler;\n    irq.handler.handler_id = rx__handler_id;\n    irq.handler.handler_action = &termina__test_fire__irq;")
          (renderTestMainFile progArch)
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
          (renderTestConfigFile progArch)
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
