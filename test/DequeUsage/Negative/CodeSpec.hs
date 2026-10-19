-- | Deque check, negative tests: a put into a deque that may be full, directly
-- (DQE-001) or through a method (DQE-002), and a deque that is never taken
-- (DQE-003) or never filled (DQE-004).
module DequeUsage.Negative.CodeSpec (spec) where

import Pipeline.Common (compileErrorCode)
import DequeUsage.Common

import Test.Hspec
import Data.Text (pack)

-- | A method that allocates a box and puts it into the deque.
refill :: String
refill =
    "    method refill(&mut self) {\n" ++
    alloc "b" "self->pending.put(b);" ++
    "        return;\n" ++
    "    }\n"

-- | A method that empties the deque.
empty :: String
empty =
    "    method empty(&mut self) {\n" ++
    drain ++
    "        return;\n" ++
    "    }\n"

spec :: Spec
spec = do
  describe "DequeUsage: puts into a deque that may be full" $ do

    it "DQE-001: put at the start of an action, which may find the deque full" $ do
      let src = taskProgram (alloc "b" "self->pending.put(b);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: two puts in a row" $ do
      let src = taskProgram (drain ++ alloc "b" "self->pending.put(b);" ++ alloc "c" "self->pending.put(c);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put after an if where one arm puts" $ do
      let src = taskProgram (drain ++
                  alloc "b" "if (b.n > 0 : u32) { self->pending.put(b); } else { self->pool.free(b); }" ++
                  alloc "c" "self->pending.put(c);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put after taking and putting the box back" $ do
      let src = taskProgram (
                  "        var taken : Option<box Msg> = None;\n" ++
                  "        self->pending.take(&mut taken);\n" ++
                  "        match taken {\n" ++
                  "            case Some(o) => { self->pending.put(o); }\n" ++
                  "            case None => { }\n" ++
                  "        }\n" ++
                  alloc "c" "self->pending.put(c);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put in a loop without taking first in the same turn" $ do
      let src = taskProgram (drain ++
                  "        for i : usize in 0 : usize .. 3 : usize {\n" ++
                  alloc "b" "self->pending.put(b);" ++
                  "        }\n")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put_front at the start of an action" $ do
      let src = taskProgram (alloc "b" "self->pending.put_front(b);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put after a try_put, which may fill the deque" $ do
      let src = taskProgram (drain ++ alloc "b" (tryPut "b") ++ alloc "c" "self->pending.put(c);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put after a replace, which leaves the deque full" $ do
      let src = taskProgram (drain ++ alloc "b" (replace "b") ++ alloc "c" "self->pending.put(c);")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-001: put after a call to a method that may fill the deque" $ do
      let src = resourceProgram (refill ++ empty)
                  ("        self->empty();\n" ++
                   "        self->refill();\n" ++
                   alloc "c" "self->pending.put(c);" ++
                   "        self->empty();\n")
      compileErrorCode src `shouldBe` Just (pack "DQE-001")

    it "DQE-002: call to a method that puts, with the deque possibly full" $ do
      let src = resourceProgram (refill ++ empty)
                  ("        self->refill();\n" ++
                   "        self->empty();\n")
      compileErrorCode src `shouldBe` Just (pack "DQE-002")

    it "DQE-002: the put two methods down" $ do
      let src = resourceProgram (refill ++ empty ++
                  "    method cycle(&mut self) {\n" ++
                  "        self->empty();\n" ++
                  "        self->refill();\n" ++
                  "        return;\n" ++
                  "    }\n")
                  ("        self->cycle();\n" ++
                   "        self->refill();\n" ++
                   "        self->empty();\n")
      compileErrorCode src `shouldBe` Just (pack "DQE-002")

  describe "DequeUsage: deques that are not both filled and taken" $ do

    it "DQE-003: deque filled and never taken" $ do
      let src = "struct Msg { n : u32; };\n" ++
                "task class T {\n" ++
                "    timer_port : sink TimeVal triggers park;\n" ++
                "    pool : access Allocator<Msg>;\n" ++
                "    pending : access DequeAccess<Msg>;\n" ++
                "    action park(&priv self, _t : TimeVal) -> Status<i32> {\n" ++
                "        let ret : Status<i32> = Success;\n" ++
                alloc "b" "self->pending.put(b);" ++
                "        return ret;\n" ++
                "    }\n" ++
                "};\n" ++
                "resource pool_res : Pool<Msg; 4>;\n" ++
                "resource pending_deque : Deque<Msg; 1>;\n" ++
                "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
                "#[priority(10)]\n" ++
                "task t : T = { timer_port <- timer, pool <-> pool_res, pending <-> pending_deque };\n"
      compileErrorCode src `shouldBe` Just (pack "DQE-003")

    it "DQE-004: deque taken and never filled" $ do
      let src = taskProgram (alloc "b" "self->pool.free(b);")
      compileErrorCode src `shouldBe` Just (pack "DQE-004")
