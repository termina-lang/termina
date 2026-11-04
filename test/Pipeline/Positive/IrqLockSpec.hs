-- | Full-pipeline checks for the identifiers of the interrupt locks. A resource
-- shared by a task and a handler is protected by an interrupt lock, and the
-- runtime keeps what it needs to release each lock in a table indexed by its
-- identifier. The configuration file gives the number of locks and one
-- identifier per lock, and the protection installed at start-up writes that
-- identifier into the lock type of the resource.
module Pipeline.Positive.IrqLockSpec (spec) where

import Pipeline.Common

import Data.Text (unpack)
import Test.Hspec

fooInterface :: String
fooInterface =
    "interface IFoo {\n" ++
    "    procedure get(&mut self, result : &mut u32);\n" ++
    "};\n" ++
    "resource class FooRes provides IFoo {\n" ++
    "    value : u32;\n" ++
    "    procedure get(&mut self, result : &mut u32) {\n" ++
    "        *result = self->value;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

-- | A resource shared by a task and a handler, which takes an interrupt lock,
-- and one that only the task reaches, which takes none.
sharedApp :: String
sharedApp =
    fooInterface ++
    "task class UserTask {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    shared : access IFoo;\n" ++
    "    own : access IFoo;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->shared.get(&mut v);\n" ++
    "        self->own.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "handler class UserHandler {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    shared : access IFoo;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->shared.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter task_timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "emitter handler_timer : PeriodicTimer = { period = {tv_sec = 2, tv_usec = 0} };\n" ++
    "resource shared_res : FooRes = { value = 0 };\n" ++
    "resource own_res : FooRes = { value = 0 };\n" ++
    "#[priority(10)]\n" ++
    "task t : UserTask = { timer_port <- task_timer, shared <-> shared_res, own <-> own_res };\n" ++
    "handler h : UserHandler = { timer_port <- handler_timer, shared <-> shared_res };\n"

-- | A resource that only one task reaches: no interrupt lock at all.
taskOnlyApp :: String
taskOnlyApp =
    fooInterface ++
    "task class UserTask {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    own : access IFoo;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var v : u32 = 0 : u32;\n" ++
    "        self->own.get(&mut v);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter task_timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "resource own_res : FooRes = { value = 0 };\n" ++
    "#[priority(10)]\n" ++
    "task t : UserTask = { timer_port <- task_timer, own <-> own_res };\n"

spec :: Spec
spec = describe "Full pipeline: identifiers of the interrupt locks" $ do

  case runFullProjectApp [("test", sharedApp)] of
    Left err ->
      it "builds an application with a resource shared by a task and a handler" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) -> do
      it "counts one interrupt lock and gives it identifier 0" $
        either (expectationFailure . unpack)
          (\c -> do
            unpack c `shouldContain` "#define TERMINA__APP_CONFIG__IRQ_LOCKS 1\n"
            unpack c `shouldContain` "#define shared_res__irq_lock_id 0\n"
            unpack c `shouldNotContain` "own_res__irq_lock_id")
          (renderConfigFile progArch)
      it "writes the identifier into the lock type of the shared resource" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain`
            ("shared_res._lock_type.type = termina__resource_lock_type__irq;\n" ++
             "    shared_res._lock_type.irq.lock_id = shared_res__irq_lock_id;\n"))
          (renderMainFile progArch)

  case runFullProjectApp [("test", taskOnlyApp)] of
    Left err ->
      it "builds an application whose resource only one task reaches" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (progArch, _) ->
      it "defines the number of interrupt locks as 0 when there is none" $
        either (expectationFailure . unpack)
          (\c -> unpack c `shouldContain` "#define TERMINA__APP_CONFIG__IRQ_LOCKS 0\n")
          (renderConfigFile progArch)
