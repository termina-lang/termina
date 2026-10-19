-- | Deque check, positive tests: the uses of a deque that keep a box between
-- activations without ever putting into a full deque.
module DequeUsage.Positive.AcceptSpec (spec) where

import Pipeline.Common (compileErrorCode)
import DequeUsage.Common

import Test.Hspec

spec :: Spec
spec = describe "DequeUsage: puts into a deque that is empty" $ do

    it "accepts take, then put at the start of an action" $
      compileErrorCode (taskProgram (drain ++ alloc "b" "self->pending.put(b);")) `shouldBe` Nothing

    it "accepts take, then put_front at the start of an action" $
      compileErrorCode (taskProgram (drain ++ alloc "b" "self->pending.put_front(b);")) `shouldBe` Nothing

    it "accepts a try_put at the start of an action, with the rejected box freed" $
      compileErrorCode (taskProgram (alloc "b" (tryPut "b"))) `shouldBe` Nothing

    it "accepts a replace at the start of an action, with the evicted box freed" $
      compileErrorCode (taskProgram (alloc "b" (replace "b"))) `shouldBe` Nothing

    it "accepts a box sent on or put back depending on a run-time condition" $ do
      let src = taskProgram (
                  "        var taken : Option<box Msg> = None;\n" ++
                  "        self->pending.take(&mut taken);\n" ++
                  "        match taken {\n" ++
                  "            case Some(o) => {\n" ++
                  "                if (o.n > 3 : u32) {\n" ++
                  "                    self->pool.free(o);\n" ++
                  "                } else {\n" ++
                  "                    o.n = o.n + 1 : u32;\n" ++
                  "                    self->pending.put(o);\n" ++
                  "                }\n" ++
                  "            }\n" ++
                  "            case None => {\n" ++
                  alloc "b" "self->pending.put(b);" ++
                  "            }\n" ++
                  "        }\n")
      compileErrorCode src `shouldBe` Nothing

    it "accepts a deque left full on one path and empty on another, with no put after" $
      compileErrorCode (taskProgram (drain ++
        alloc "b" "if (b.n > 0 : u32) { self->pending.put(b); } else { self->pool.free(b); }"))
        `shouldBe` Nothing

    it "accepts take, then put, in every turn of a loop" $
      compileErrorCode (taskProgram (
        "        for i : usize in 0 : usize .. 3 : usize {\n" ++
        drain ++
        alloc "b" "self->pending.put(b);" ++
        "        }\n")) `shouldBe` Nothing

    it "accepts a method that puts, called after one that empties" $
      compileErrorCode (resourceProgram (
          "    method refill(&mut self) {\n" ++
          alloc "b" "self->pending.put(b);" ++
          "        return;\n" ++
          "    }\n" ++
          "    method empty(&mut self) {\n" ++
          drain ++
          "        return;\n" ++
          "    }\n")
          ("        self->empty();\n" ++
           "        self->refill();\n"))
        `shouldBe` Nothing

    it "accepts a put after a method that always empties the deque" $
      compileErrorCode (resourceProgram (
          "    method empty(&mut self) {\n" ++
          drain ++
          "        return;\n" ++
          "    }\n")
          ("        self->empty();\n" ++
           alloc "b" "self->pending.put(b);"))
        `shouldBe` Nothing
