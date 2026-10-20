-- | Programs shared by the deque tests: a task with two actions that use a
-- deque, and a resource whose procedure and methods use one. Each deque holds one
-- box, so a put that the check accepts is one that cannot find it full.
module DequeUsage.Common (taskProgram, resourceProgram, alloc, drain, tryPut, replace) where

-- | A task whose action @park@ has the given body and whose action @drain@
-- empties the deque, so that the deque is always taken somewhere.
taskProgram :: String -> String
taskProgram body =
    "struct Msg { n : u32; };\n" ++
    "task class T {\n" ++
    "    timer_port : sink TimeVal triggers park;\n" ++
    "    drain_port : sink TimeVal triggers drain;\n" ++
    "    pool : access Allocator<Msg>;\n" ++
    "    pending : access DequeAccess<Msg>;\n" ++
    "    action park(&priv self, _t : TimeVal) {\n" ++
    body ++
    "        return;\n" ++
    "    }\n" ++
    "    action drain(&priv self, _t : TimeVal) {\n" ++
    drain ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "resource pool_res : Pool<Msg; 4>;\n" ++
    "resource pending_deque : Deque<Msg; 1>;\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "emitter timer2 : PeriodicTimer = { period = {tv_sec = 2, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : T = { timer_port <- timer, drain_port <- timer2, pool <-> pool_res, pending <-> pending_deque };\n"

-- | A resource whose procedure @flush@ has the given body, with the given
-- methods and a procedure @clear@ that empties the deque.
resourceProgram :: String -> String -> String
resourceProgram methods body =
    "struct Msg { n : u32; };\n" ++
    "interface IHolder {\n" ++
    "    procedure flush(&mut self);\n" ++
    "    procedure clear(&mut self);\n" ++
    "};\n" ++
    "resource class CHolder provides IHolder {\n" ++
    "    pool : access Allocator<Msg>;\n" ++
    "    pending : access DequeAccess<Msg>;\n" ++
    methods ++
    "    procedure flush(&mut self) {\n" ++
    body ++
    "        return;\n" ++
    "    }\n" ++
    "    procedure clear(&mut self) {\n" ++
    drain ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "task class T {\n" ++
    "    timer_port : sink TimeVal triggers tick;\n" ++
    "    holder : access IHolder;\n" ++
    "    action tick(&priv self, _t : TimeVal) {\n" ++
    "        self->holder.flush();\n" ++
    "        self->holder.clear();\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "resource pool_res : Pool<Msg; 4>;\n" ++
    "resource pending_deque : Deque<Msg; 1>;\n" ++
    "resource holder_res : CHolder = { pool <-> pool_res, pending <-> pending_deque };\n" ++
    "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = 0} };\n" ++
    "#[priority(10)]\n" ++
    "task t : T = { timer_port <- timer, holder <-> holder_res };\n"

-- | Allocates a box under the given name and runs the statement on it, freeing
-- nothing if the pool is empty.
alloc :: String -> String -> String
alloc name stmt =
    "        var " ++ name ++ "_opt : Option<box Msg> = None;\n" ++
    "        self->pool.alloc(&mut " ++ name ++ "_opt);\n" ++
    "        match " ++ name ++ "_opt {\n" ++
    "            case Some(" ++ name ++ ") => { " ++ stmt ++ " }\n" ++
    "            case None => { }\n" ++
    "        }\n"

-- | Takes the box of the deque, if any, and frees it.
drain :: String
drain =
    "        var old : Option<box Msg> = None;\n" ++
    "        self->pending.take(&mut old);\n" ++
    "        match old {\n" ++
    "            case Some(o) => { self->pool.free(o); }\n" ++
    "            case None => { }\n" ++
    "        }\n"

-- | Offers the box with the given name to the deque, freeing it if the deque is
-- full.
tryPut :: String -> String
tryPut name =
    "var rejected : Option<box Msg> = None; " ++
    "self->pending.try_put(" ++ name ++ ", &mut rejected); " ++
    "match rejected { case Some(r) => { self->pool.free(r); } case None => { } }"

-- | Puts the box with the given name into the deque, freeing the one at the
-- front if the deque is full.
replace :: String -> String
replace name =
    "var evicted : Option<box Msg> = None; " ++
    "self->pending.replace(" ++ name ++ ", &mut evicted); " ++
    "match evicted { case Some(e) => { self->pool.free(e); } case None => { } }"
