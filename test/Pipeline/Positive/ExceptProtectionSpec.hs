-- | Full-pipeline checks for the protection of the resources that the handler
-- of @system_except@ reaches. The handler does not count as a user when the
-- lock of a resource is chosen, so a resource that only one task shares with
-- it takes no lock. The resources and pools it reaches that do take one,
-- nested ones included, are listed in @termina__app__disable_protection@,
-- which the runtime calls before the handler, and @config.h@ gives their
-- number in @TERMINA__APP_CONFIG__EXCEPT_PROTECTED@ in every application.
module Pipeline.Positive.ExceptProtectionSpec (spec) where

import Pipeline.Common

import Data.Text (unpack)
import Test.Hspec

resourceClasses :: String
resourceClasses =
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
    "resource class RecRes provides IFoo {\n" ++
    "    mem : access IFoo;\n" ++
    "    procedure get(&mut self, result : &mut u32) {\n" ++
    "        self->mem.get(result);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

-- | A task class that reaches up to three resources, and a periodic handler
-- that reaches one.
userClasses :: String
userClasses =
    "task class FirstTask {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    shared : access IFoo;\n" ++
    "    rec : access IFoo;\n" ++
    "    own : access IFoo;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->shared.get(&mut v);\n" ++
    "        self->rec.get(&mut v);\n" ++
    "        self->own.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "task class SecondTask {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    rec : access IFoo;\n" ++
    "    mem : access IFoo;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->rec.get(&mut v);\n" ++
    "        self->mem.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "handler class TimerHandler {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    shared : access IFoo;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->shared.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

-- | shared_res: a task, a handler and the handler of system_except, an
-- interrupt lock. rec_res: two tasks and the handler of system_except, a
-- mutex, and so is mem_res, which rec_res and the second task reach and the
-- handler of system_except only through rec_res. own_res: a task and the
-- handler of system_except, no lock.
exceptApp :: String
exceptApp =
    resourceClasses ++ userClasses ++
    "handler class FaultHandler {\n" ++
    "    fault_ev : sink Exception triggers on_fault;\n" ++
    "    shared : access IFoo;\n" ++
    "    rec : access IFoo;\n" ++
    "    own : access IFoo;\n" ++
    "    action on_fault(&priv self, _e : Exception) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->shared.get(&mut v);\n" ++
    "        self->rec.get(&mut v);\n" ++
    "        self->own.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter timer_a : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "emitter timer_b : PeriodicTimer = { period = {tv_sec = 2, tv_usec = 0} };\n" ++
    "emitter timer_c : PeriodicTimer = { period = {tv_sec = 3, tv_usec = 0} };\n" ++
    "resource shared_res : FooRes = { value = 0 };\n" ++
    "resource mem_res : FooRes = { value = 0 };\n" ++
    "resource rec_res : RecRes = { mem <-> mem_res };\n" ++
    "resource own_res : FooRes = { value = 0 };\n" ++
    "#[priority(10)]\n" ++
    "task t1 : FirstTask = { timer_port <- timer_a, shared <-> shared_res, rec <-> rec_res, own <-> own_res };\n" ++
    "#[priority(11)]\n" ++
    "task t2 : SecondTask = { timer_port <- timer_b, rec <-> rec_res, mem <-> mem_res };\n" ++
    "handler h : TimerHandler = { timer_port <- timer_c, shared <-> shared_res };\n" ++
    "handler fault : FaultHandler = { fault_ev <- system_except, shared <-> shared_res, rec <-> rec_res, own <-> own_res };\n"

-- | The same application without a handler of system_except.
noExceptApp :: String
noExceptApp =
    resourceClasses ++ userClasses ++
    "emitter timer_a : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "emitter timer_b : PeriodicTimer = { period = {tv_sec = 2, tv_usec = 0} };\n" ++
    "emitter timer_c : PeriodicTimer = { period = {tv_sec = 3, tv_usec = 0} };\n" ++
    "resource shared_res : FooRes = { value = 0 };\n" ++
    "resource mem_res : FooRes = { value = 0 };\n" ++
    "resource rec_res : RecRes = { mem <-> mem_res };\n" ++
    "resource own_res : FooRes = { value = 0 };\n" ++
    "#[priority(10)]\n" ++
    "task t1 : FirstTask = { timer_port <- timer_a, shared <-> shared_res, rec <-> rec_res, own <-> own_res };\n" ++
    "#[priority(11)]\n" ++
    "task t2 : SecondTask = { timer_port <- timer_b, rec <-> rec_res, mem <-> mem_res };\n" ++
    "handler h : TimerHandler = { timer_port <- timer_c, shared <-> shared_res };\n"

noLock :: String -> String
noLock r = r ++ "._lock_type.type = termina__resource_lock_type__none;\n"

spec :: Spec
spec = describe "Full pipeline: protection withdrawn for the handler of system_except" $ do

  case runFullProjectApp [("test", exceptApp)] of
    Left err ->
      it "builds an application with a handler of system_except" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) -> do
      it "chooses the locks without counting the handler of system_except" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "#define shared_res__irq_lock_id 0\n"
            unpack c `shouldContain` "mem_res__mutex_id"
            unpack c `shouldContain` "rec_res__mutex_id"
            unpack c `shouldNotContain` "own_res__")
          (renderConfigFile progArch)
      it "counts the protected resources that the handler reaches, nested ones included" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain` "#define TERMINA__APP_CONFIG__EXCEPT_PROTECTED 3\n")
          (renderConfigFile progArch)
      it "withdraws their protection in termina__app__disable_protection" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "void termina__app__disable_protection(void) {\n"
            unpack c `shouldContain` noLock "mem_res"
            unpack c `shouldContain` noLock "rec_res"
            unpack c `shouldContain` noLock "shared_res"
            unpack c `shouldNotContain` noLock "own_res")
          (renderMainFile progArch)

  case runFullProjectApp [("test", noExceptApp)] of
    Left err ->
      it "builds an application without a handler of system_except" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) -> do
      it "defines the count as 0" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain` "#define TERMINA__APP_CONFIG__EXCEPT_PROTECTED 0\n")
          (renderConfigFile progArch)
      it "generates no termina__app__disable_protection" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldNotContain` "termina__app__disable_protection")
          (renderMainFile progArch)
