-- | VarUsage (move/borrow) positive tests: well-formed box/option-box usage
-- raises no error (the check returns 'Nothing').
module VarUsage.Positive.UsageSpec (spec) where

import VarUsage.Common (runNegativeTestBoxUsage, runNegativeTestVarUsage)

import Data.Maybe (isNothing)
import Test.Hspec

wrap :: String -> String
wrap body =
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, data : box u32);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    data_pool : access Allocator<u32>;\n" ++
  "    procedure proc0(&mut self, data : box u32) {\n" ++
  body ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

-- | A box parameter consumed exactly once (freed).
freesBoxParam :: String
freesBoxParam = wrap "        self->data_pool.free(data);\n"

-- | An option-box allocated and then consumed in every branch of its match.
allocAndConsumes :: String
allocAndConsumes = wrap $
  "        self->data_pool.free(data);\n" ++
  "        var opt : Option<box u32> = None;\n" ++
  "        self->data_pool.alloc(&mut opt);\n" ++
  "        match opt {\n" ++
  "            case Some(obj) => { self->data_pool.free(obj); }\n" ++
  "            case None => { }\n" ++
  "        }\n"

-- | A procedure whose body fills the option-box parameter @result@.
outWrap :: String -> String
outWrap body =
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, data : box u32, result : &mut Option<box u32>);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    data_pool : access Allocator<u32>;\n" ++
  "    inner : access Interface0;\n" ++
  "    procedure proc0(&mut self, data : box u32, result : &mut Option<box u32>) {\n" ++
  body ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

-- | The option-box parameter filled by an alloc.
fillsParamWithAlloc :: String
fillsParamWithAlloc = outWrap $
  "        self->data_pool.free(data);\n" ++
  "        self->data_pool.alloc(result);\n"

-- | The option-box parameter filled with the box parameter on one branch and
-- with None on the other.
fillsParamWithSomeOrNone :: String
fillsParamWithSomeOrNone = outWrap $
  "        var flag : bool = true;\n" ++
  "        if (flag) {\n" ++
  "            *result = Some(data);\n" ++
  "        } else {\n" ++
  "            self->data_pool.free(data);\n" ++
  "            *result = None;\n" ++
  "        }\n"

-- | The option-box parameter handed on to another procedure, which fills it.
forwardsParam :: String
forwardsParam = outWrap $
  "        self->inner.proc0(data, result);\n"

-- | A caller that hands an empty option-box to a procedure and moves what it
-- gets back.
callsProcWithOption :: String
callsProcWithOption =
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, result : &mut Option<box u32>);\n" ++
  "};\n" ++
  "task class TaskClass0 {\n" ++
  "    timer : sink TimeVal triggers run;\n" ++
  "    src : access Interface0;\n" ++
  "    data_pool : access Allocator<u32>;\n" ++
  "    action run(&priv self, _t : TimeVal) {\n" ++
  "        var opt : Option<box u32> = None;\n" ++
  "        self->src.proc0(&mut opt);\n" ++
  "        match opt {\n" ++
  "            case Some(obj) => { self->data_pool.free(obj); }\n" ++
  "            case None => { }\n" ++
  "        }\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

-- | A box consumed (freed) on every branch of an if.
freesBoxInBothBranches :: String
freesBoxInBothBranches = wrap $
  "        var flag : bool = true;\n" ++
  "        if (flag) {\n" ++
  "            self->data_pool.free(data);\n" ++
  "        } else {\n" ++
  "            self->data_pool.free(data);\n" ++
  "        }\n"

-- | A variable assigned in every branch of an if and read after it.
assignsInBothBranches :: String
assignsInBothBranches =
  "function fun0(c : bool) -> u32 {\n" ++
  "    var x : u32 = 0 : u32;\n" ++
  "    if (c) {\n" ++
  "        x = 1 : u32;\n" ++
  "    } else {\n" ++
  "        x = 2 : u32;\n" ++
  "    }\n" ++
  "    return x;\n" ++
  "}\n"

-- | A variable assigned in one branch of an if and read after it.
assignsInOneBranch :: String
assignsInOneBranch =
  "function fun0(c : bool) -> u32 {\n" ++
  "    var x : u32 = 0 : u32;\n" ++
  "    if (c) {\n" ++
  "        x = 1 : u32;\n" ++
  "    }\n" ++
  "    return x;\n" ++
  "}\n"

-- | A variable assigned in a loop and read in the guard of the next iteration.
assignsInLoopGuard :: String
assignsInLoopGuard =
  "function fun0(array0 : &[u16; 10]) -> bool {\n" ++
  "    var found : bool = false;\n" ++
  "    for i : usize in 0 : usize .. 10 : usize while found == false {\n" ++
  "        if (*array0)[i] == 1024 : u16 {\n" ++
  "            found = true;\n" ++
  "        }\n" ++
  "    }\n" ++
  "    return found;\n" ++
  "}\n"

-- | A variable assigned in a loop and read by the same assignment in the next
-- iteration.
accumulatesInLoop :: String
accumulatesInLoop =
  "function fun0(array0 : &[u32; 10]) -> u32 {\n" ++
  "    var acc : u32 = 0 : u32;\n" ++
  "    for i : usize in 0 : usize .. 10 : usize {\n" ++
  "        acc = acc + (*array0)[i];\n" ++
  "    }\n" ++
  "    return acc;\n" ++
  "}\n"

-- | A variable assigned at the end of the body of a loop and read at its
-- beginning in the next iteration.
readsPreviousIteration :: String
readsPreviousIteration =
  "function fun0(array0 : &[u32; 10]) -> u32 {\n" ++
  "    var prev : u32 = 0 : u32;\n" ++
  "    var sum : u32 = 0 : u32;\n" ++
  "    for i : usize in 0 : usize .. 10 : usize {\n" ++
  "        sum = sum + prev;\n" ++
  "        prev = (*array0)[i];\n" ++
  "    }\n" ++
  "    return sum;\n" ++
  "}\n"

-- | A field of the class read only through the dereference spelling. Both
-- spellings name the same field, so the class field counts as used.
readsFieldThroughDereference :: String
readsFieldThroughDereference =
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, data : &mut u8);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    field0 : u8;\n" ++
  "    procedure proc0(&mut self, data : &mut u8) {\n" ++
  "        *data = (*self).field0;\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

-- | Two objects with a field of the same name, both of them read. Neither the
-- field of the class nor the one of the struct is reported.
readsHomonymousFields :: String
readsHomonymousFields =
  "struct Struct0 {\n" ++
  "    field0 : u8;\n" ++
  "};\n" ++
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, s : &Struct0, data : &mut u8);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    field0 : u8;\n" ++
  "    procedure proc0(&mut self, s : &Struct0, data : &mut u8) {\n" ++
  "        *data = self->field0 + s->field0;\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

-- | A variable and a field of the same name, where the variable is read: the
-- variable answers for itself and the field for itself.
readsVariableAndHomonymousField :: String
readsVariableAndHomonymousField =
  "struct Struct0 {\n" ++
  "    field0 : u8;\n" ++
  "};\n" ++
  "function fun0(s : &Struct0) -> u8 {\n" ++
  "    var field0 : u8 = s->field0;\n" ++
  "    return field0;\n" ++
  "}\n"

spec :: Spec
spec = do
  describe "VarUsage: qualified field names" $
    mapM_ (\(name, src) -> it name $ runNegativeTestVarUsage src `shouldSatisfy` isNothing)
      [ ("accepts a class field read through (*self).field", readsFieldThroughDereference)
      , ("accepts homonymous fields of two objects, both read", readsHomonymousFields)
      , ("accepts a variable and a field of the same name", readsVariableAndHomonymousField)
      ]
  describe "VarUsage: well-formed usage raises no error" $
    mapM_ (\(name, src) -> it name $ runNegativeTestBoxUsage src `shouldSatisfy` isNothing)
    [ ("accepts a box parameter that is freed once", freesBoxParam)
    , ("accepts an option-box allocated and consumed in all branches", allocAndConsumes)
    , ("accepts an option-box parameter filled by an alloc", fillsParamWithAlloc)
    , ("accepts an option-box parameter filled with Some or None", fillsParamWithSomeOrNone)
    , ("accepts an option-box parameter handed on to another procedure", forwardsParam)
    , ("accepts an option-box filled by a procedure and then moved", callsProcWithOption)
    , ("accepts a box freed on every branch of an if", freesBoxInBothBranches)
    , ("accepts a variable assigned in every branch and read after them", assignsInBothBranches)
    , ("accepts a variable assigned in one branch and read after it", assignsInOneBranch)
    , ("accepts a variable assigned in a loop and read in its guard", assignsInLoopGuard)
    , ("accepts a variable accumulated in a loop", accumulatesInLoop)
    , ("accepts a variable read in the next iteration of a loop", readsPreviousIteration)
    ]
