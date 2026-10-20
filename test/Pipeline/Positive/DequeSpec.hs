-- | Full-pipeline goldens for a deque: the C of its procedures, and the
-- initialization of the deque, which leaves it empty over the array of its
-- boxes.
module Pipeline.Positive.DequeSpec (spec) where

import Pipeline.Common
import Golden

import Data.Text (unpack)
import Test.Hspec

-- | A task that keeps a counter in a deque between activations and frees it
-- once it reaches four, and offers boxes to the deque with try_put and replace.
dequeApp :: String
dequeApp =
    "struct Msg { n : u32; };\n" ++
    "task class T {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    offer_port : sink TimeVal triggers offer;\n" ++
    "    pool : access Allocator<Msg>;\n" ++
    "    pending : access DequeAccess<Msg>;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        var taken : Option<box Msg> = None;\n" ++
    "        self->pending.take(&mut taken);\n" ++
    "        match taken {\n" ++
    "            case Some(m) => {\n" ++
    "                if (m.n > 3 : u32) {\n" ++
    "                    self->pool.free(m);\n" ++
    "                } else {\n" ++
    "                    m.n = m.n + 1 : u32;\n" ++
    "                    self->pending.put(m);\n" ++
    "                }\n" ++
    "            }\n" ++
    "            case None => {\n" ++
    "                var fresh : Option<box Msg> = None;\n" ++
    "                self->pool.alloc(&mut fresh);\n" ++
    "                match fresh {\n" ++
    "                    case Some(f) => {\n" ++
    "                        f.n = 0 : u32;\n" ++
    "                        self->pending.put(f);\n" ++
    "                    }\n" ++
    "                    case None => { }\n" ++
    "                }\n" ++
    "            }\n" ++
    "        }\n" ++
    "        return;\n" ++
    "    }\n" ++
    "    action offer(&priv self, _t : TimeVal) {\n" ++
    "        var b : Option<box Msg> = None;\n" ++
    "        self->pool.alloc(&mut b);\n" ++
    "        match b {\n" ++
    "            case Some(m) => {\n" ++
    "                var rejected : Option<box Msg> = None;\n" ++
    "                self->pending.try_put(m, &mut rejected);\n" ++
    "                match rejected {\n" ++
    "                    case Some(r) => { self->pool.free(r); }\n" ++
    "                    case None => { }\n" ++
    "                }\n" ++
    "            }\n" ++
    "            case None => { }\n" ++
    "        }\n" ++
    "        var c : Option<box Msg> = None;\n" ++
    "        self->pool.alloc(&mut c);\n" ++
    "        match c {\n" ++
    "            case Some(m) => {\n" ++
    "                var evicted : Option<box Msg> = None;\n" ++
    "                self->pending.replace(m, &mut evicted);\n" ++
    "                match evicted {\n" ++
    "                    case Some(e) => { self->pool.free(e); }\n" ++
    "                    case None => { }\n" ++
    "                }\n" ++
    "            }\n" ++
    "            case None => { }\n" ++
    "        }\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "resource pool_res : Pool<Msg; 4>;\n" ++
    "resource pending_deque : Deque<Msg; 2>;\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "emitter timer2 : PeriodicTimer = { period = {tv_sec = 2, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : T = { timer_port <- timer, offer_port <- timer2, pool <-> pool_res, pending <-> pending_deque };\n"

spec :: Spec
spec = describe "Full pipeline: deques" $ do
  it "calls the procedures of a deque" $
    goldenC "deque_take_put" (buildAndRenderModule "test" [("test", dequeApp)])
  case runFullProjectApp [("test", dequeApp)] of
    Left err ->
      it "builds the deque application" $
        expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
    Right (_, prjprogs) ->
      it "starts every deque empty (init file)" $
        either (expectationFailure . unpack) (goldenC "deque_init")
          (renderInitFile prjprogs)
