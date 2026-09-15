-- | VarUsage (move/borrow) negative tests, *detail* flavour: each case asserts
-- the exact error constructor and the identifier it blames. The source programs
-- are exported so 'VarUsage.Negative.CodeSpec' can assert their VUE-NNN codes
-- over the same inputs without duplicating them.
module VarUsage.Negative.DetailSpec
  ( spec
  , testVUE001, testVUE002, testBE001, testBE001_1, testBE002, testBE002_1
  , testBE003, testBE004, testBE005, testBE006, testBE006_1, testBE007, testBE008
  , testBE009, testBE010, testBE011, testBE012, testVUE004, testVUE004_1
  , testVUE003, testVUE005, testVUE005_1, testVUE005_2, testVUE006, testVUE006_1
  , testVUE006_2, testVUE006_3, testVUE006_4, testVUE006_5, testVUE002_1, testVUE002_2
  , testVUE007, testVUE008
  ) where

import Test.Hspec
import Semantic.AST
import ControlFlow.BoxUsage.Errors hiding (Error)
import ControlFlow.VarUsage.Errors hiding (Error)
import qualified ControlFlow.BoxUsage.Errors as BE
import qualified ControlFlow.VarUsage.Errors as VE
import VarUsage.Common

testVUE001 :: String
testVUE001 = "function fun0(_data : u32) -> u32 {\n" ++
       "\n" ++
       "    let ret : u32 = _data + 1;\n" ++
       "\n" ++
       "    return ret;\n" ++
       "\n" ++
       "}\n"

testVUE002 :: String
testVUE002 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self, _data : box u32);\n" ++
       "\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    data_pool : access Allocator<u32>;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, _data : box u32) {\n" ++
       "\n" ++
       "        var opt : Option<box u32> = None;\n" ++
       "\n" ++
       "        \n" ++
       "        return;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testBE001 :: String
testBE001 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self, _data : box u32);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    data_pool : access Allocator<u32>;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, data : box u32) {\n" ++
       "\n" ++
       "        var opt : Option<box u32> = None;\n" ++
       "\n" ++
       "        self->data_pool.alloc(&mut opt);\n" ++
       "\n" ++
       "        match opt {\n" ++
       "\n" ++
       "            case Some(obj) => {\n" ++
       "\n" ++
       "            }\n" ++
       "\n" ++
       "            case None => {\n" ++
       "\n" ++
       "            }\n" ++
       "\n" ++
       "        }\n" ++
       "                \n" ++
       "        self->data_pool.free(data);\n" ++
       "\n" ++
       "        return;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testBE001_1 :: String
testBE001_1 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self, _data : box u32);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    data_pool : access Allocator<u32>;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, data : box u32) {\n" ++
       "\n" ++
       "        return;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testBE002 :: String
testBE002 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self, _data : box u32);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    data_pool : access Allocator<u32>;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, data : box u32) {\n" ++
       "\n" ++
       "        var opt : Option<box u32> = None;\n" ++
       "\n" ++
       "        opt = Some(data);\n" ++
       "        self->data_pool.free(data);\n" ++
       "\n" ++
       "        match opt {\n" ++
       "\n" ++
       "            case Some(obj) => {\n" ++
       "\n" ++
       "                self->data_pool.free(obj);\n" ++
       "\n" ++
       "            }\n" ++
       "\n" ++
       "            case None => {\n" ++
       "\n" ++
       "            }\n" ++
       "\n" ++
       "        }\n" ++
       "                \n" ++
       "        return;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testBE002_1 :: String
testBE002_1 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self, _data : box u32);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    data_pool : access Allocator<u32>;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, data : box u32) {\n" ++
       "\n" ++
       "        var opt : Option<box u32> = None;\n" ++
       "\n" ++
       "        self->data_pool.alloc(&mut opt);\n" ++
       "\n" ++
       "        self->data_pool.free(data);\n" ++
       "\n" ++
       "        match opt {\n" ++
       "\n" ++
       "            case Some(obj) => {\n" ++
       "\n" ++
       "                self->data_pool.free(obj);\n" ++
       "\n" ++
       "            }\n" ++
       "\n" ++
       "            case None => {\n" ++
       "\n" ++
       "            }\n" ++
       "\n" ++
       "        }\n" ++
       "\n" ++
       "        self->data_pool.free(data);\n" ++
       "                \n" ++
       "        return;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

-- A resource procedure that receives a box parameter (@data@) and owns an
-- allocator port (@data_pool@). The body is spliced verbatim before the final
-- @return@. Reused to trigger each box/option-box usage error in isolation.
veWrap :: String -> String
veWrap body =
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, _data : box u32);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    data_pool : access Allocator<u32>;\n" ++
  "    procedure proc0(&mut self, data : box u32) {\n" ++
  body ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

veFreeData, veDeclOpt, veAlloc, veMatchMove :: String
veFreeData   = "        self->data_pool.free(data);\n"
veDeclOpt    = "        var opt : Option<box u32> = None;\n"
veAlloc      = "        self->data_pool.alloc(&mut opt);\n"
veMatchMove  = "        match opt {\n" ++
               "            case Some(obj) => { self->data_pool.free(obj); }\n" ++
               "            case None => { }\n" ++
               "        }\n"

-- BE-003: the option-box @opt@ is allocated once but moved (matched) twice.
testBE003 :: String
testBE003 = veWrap (veFreeData ++ veDeclOpt ++ veAlloc ++ veMatchMove ++ veMatchMove)

-- BE-004: @opt@ ends one branch moved and the other branch allocated, so the
-- final option-box state differs across branches.
testBE004 :: String
testBE004 = veWrap (
  veDeclOpt ++ veAlloc ++
  "        if (data == 0) {\n" ++ veMatchMove ++
  "        } else {\n" ++ veAlloc ++ veMatchMove ++ "        }\n" ++
  veFreeData)

-- BE-005: @opt@ is moved inside a for loop, i.e. in a branch that may not run.
testBE005 :: String
testBE005 = veWrap (
  veFreeData ++ veDeclOpt ++ veAlloc ++
  "        for i : usize in 0 .. 4 {\n" ++ veMatchMove ++ "        }\n")

-- BE-007: the box @data@ is moved in the @if@ branch but not in the @else@.
testBE007 :: String
testBE007 = veWrap (
  "        if (data == 0) {\n" ++ veFreeData ++ "        } else {\n        }\n")

-- BE-008: the box @data@ is moved inside a for loop (a branch that may not run).
testBE008 :: String
testBE008 = veWrap ("        for i : usize in 0 .. 4 {\n" ++ veFreeData ++ "        }\n")

-- BE-009: @opt@ is allocated but never moved afterwards.
testBE009 :: String
testBE009 = veWrap (veFreeData ++ veDeclOpt ++ veAlloc)

-- BE-010: @opt@ is allocated twice before being moved.
testBE010 :: String
testBE010 = veWrap (veFreeData ++ veDeclOpt ++ veAlloc ++ veAlloc ++ veMatchMove)

-- BE-011: @opt@ is moved (matched) without ever having been allocated.
testBE011 :: String
testBE011 = veWrap (veFreeData ++ veDeclOpt ++ veMatchMove)

-- BE-006: @opt@ is allocated only in the @if@ branch (with an explicit @else@
-- that leaves it alone) and then used after the merge, so it is used in a
-- previous branch but missing in another.
testBE006 :: String
testBE006 = veWrap (
  veDeclOpt ++
  "        if (data == 0) {\n" ++ veAlloc ++ "        } else {\n        }\n" ++
  veMatchMove ++ veFreeData)

-- BE-006, with a field that shares the name of the option-box. The shape is
-- the one of 'testBE006', and the only addition is that the @else@ branch
-- reads @w->opt@, a field of a struct reached through a reference. The
-- option-box @opt@ is a local of the procedure and the field @opt@ belongs to
-- @Wrapper@, which is legal: a field may not have an option-box type, so the
-- two can never be the same object. The backward pass used to record the bare
-- name of a field, so the branch that only reads @w->opt@ passed for a branch
-- that used the option-box, and the answer came out as BE-004 instead.
testBE006_1 :: String
testBE006_1 =
  "struct Wrapper {\n" ++
  "    opt : u32;\n" ++
  "};\n" ++
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, data : box u32, w : &Wrapper);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    data_pool : access Allocator<u32>;\n" ++
  "    seen : u32;\n" ++
  "    procedure proc0(&mut self, data : box u32, w : &Wrapper) {\n" ++
  "        var opt : Option<box u32> = None;\n" ++
  "        if (self->seen == 0) {\n" ++
  veAlloc ++
  "        } else {\n" ++
  "            self->seen = w->opt;\n" ++
  "        }\n" ++
  veMatchMove ++ veFreeData ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

-- BE-012: matching an option-box with a default @case _@ instead of an explicit
-- @Some@ case. The type checker accepts the match as exhaustive, but the usage
-- analysis flags the missing @Some@ case.
testBE012 :: String
testBE012 =
  "interface Interface0 {\n" ++
  "    procedure proc0(&mut self, data : Option<box u32>);\n" ++
  "};\n" ++
  "resource class ResourceClass0 provides Interface0 {\n" ++
  "    procedure proc0(&mut self, data : Option<box u32>) {\n" ++
  "        match (data) {\n" ++
  "            case None => { }\n" ++
  "            case _ => { }\n" ++
  "        }\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

testVUE004 :: String
testVUE004 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    field0 : u32;\n" ++
       "\n" ++
       "    method method0(&mut self) {\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "    procedure proc0(&mut self) {\n" ++
       "        self->field0 = 0 : u32;\n" ++
       "        self->method0();\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testVUE004_1 :: String
testVUE004_1 = "task class TaskClass0 {\n" ++
       "\n" ++
       "    field0 : u32;\n" ++
       "\n" ++
       "    snk0 : sink u32 triggers action0;\n" ++
       "\n" ++
       "    viewer viewer0(&self, value : u32) -> u32 {\n" ++
       "        return value;\n" ++
       "    }\n" ++
       "\n" ++
       "    action action0(&priv self, input : u32) -> Status<i32> {\n" ++
       "        var ret : Status<i32> = Success;\n" ++
       "        self->field0 = self->viewer0(input);\n" ++
       "        return ret;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testVUE003 :: String
testVUE003 = "task class TaskClass0 {\n" ++
       "\n" ++
       "    snk0 : sink u32 triggers action0;\n" ++
       "\n" ++
       "    action action0(&priv self, _input : u32) -> Status<i32> {\n" ++
       "        var ret : Status<i32> = Success;\n" ++
       "        return ret;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testVUE005 :: String
testVUE005 = "interface Interface0 {\n" ++
       "\n" ++
       "    procedure proc0(&mut self);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    field0 : u32;\n" ++
       "\n" ++
       "    method method0(&mut self) {\n" ++
       "        self->field0 = 1 : u32;\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "    procedure proc0(&mut self) {\n" ++
       "        self->field0 = 0 : u32;\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testVUE005_1 :: String
testVUE005_1 = "task class TaskClass0 {\n" ++
       "\n" ++
       "    field0 : u32;\n" ++
       "\n" ++
       "    snk0 : sink u32 triggers action0;\n" ++
       "\n" ++
       "    viewer viewer0(&self) -> u32 {\n" ++
       "        return self->field0;\n" ++
       "    }\n" ++
       "\n" ++
       "    action action0(&priv self, input : u32) -> Status<i32> {\n" ++
       "        var ret : Status<i32> = Success;\n" ++
       "        self->field0 = input;\n" ++
       "        return ret;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testVUE006 :: String
testVUE006 = "function fun0() -> u32 {\n" ++
       "    var x : u32 = 0 : u32;\n" ++
       "    let first : u32 = x;\n" ++
       "    x = 1 : u32;\n" ++
       "    x = 2 : u32;\n" ++
       "    return x + first;\n" ++
       "}\n"

testVUE006_1 :: String
testVUE006_1 = "function fun0(c : bool) -> u32 {\n" ++
       "    var x : u32 = 0 : u32;\n" ++
       "    let first : u32 = x;\n" ++
       "    if (c) {\n" ++
       "        x = 1 : u32;\n" ++
       "    }\n" ++
       "    x = 2 : u32;\n" ++
       "    return x + first;\n" ++
       "}\n"

-- | The variable is read after the loop, so it is not an unused one, but the
-- value the loop assigns to it is overwritten before anybody reads it.
testVUE006_2 :: String
testVUE006_2 = "function fun0(array0 : &[u32; 10]) -> u32 {\n" ++
       "    var last : u32 = 0 : u32;\n" ++
       "    let first : u32 = last;\n" ++
       "    for i : usize in 0 : usize .. 10 : usize {\n" ++
       "        last = (*array0)[i];\n" ++
       "    }\n" ++
       "    last = 5 : u32;\n" ++
       "    return last + first;\n" ++
       "}\n"

-- | The value the declaration gives the object is overwritten before anybody
-- reads it, so either the real value belongs in the declaration or the object
-- is to be declared without an initializer.
testVUE006_3 :: String
testVUE006_3 = "function fun0() -> u32 {\n" ++
       "    var x : u32 = 0 : u32;\n" ++
       "    x = 1 : u32;\n" ++
       "    return x;\n" ++
       "}\n"

-- | A field and a variable of the same name: reading the field must not
-- rescue the value the declaration gives the variable.
testVUE006_4 :: String
testVUE006_4 = "struct Struct0 {\n" ++
       "    field0 : u8;\n" ++
       "};\n" ++
       "\n" ++
       "function fun0(s : &Struct0) -> u8 {\n" ++
       "    var field0 : u8 = 0 : u8;\n" ++
       "    if (s->field0 == 1 : u8) {\n" ++
       "        field0 = 2 : u8;\n" ++
       "    } else {\n" ++
       "        field0 = 3 : u8;\n" ++
       "    }\n" ++
       "    return field0;\n" ++
       "}\n"

-- | The same, with the field reached through the other spelling.
testVUE006_5 :: String
testVUE006_5 = "struct Struct0 {\n" ++
       "    field0 : u8;\n" ++
       "};\n" ++
       "\n" ++
       "function fun0(s : &Struct0) -> u8 {\n" ++
       "    var field0 : u8 = 0 : u8;\n" ++
       "    if ((*s).field0 == 1 : u8) {\n" ++
       "        field0 = 2 : u8;\n" ++
       "    } else {\n" ++
       "        field0 = 3 : u8;\n" ++
       "    }\n" ++
       "    return field0;\n" ++
       "}\n"

-- | A field of the class that nobody reads, while a field of the same name is
-- read from another object. The read of the latter must not answer for it.
testVUE002_1 :: String
testVUE002_1 = "struct Struct0 {\n" ++
       "    field0 : u8;\n" ++
       "};\n" ++
       "\n" ++
       "interface Interface0 {\n" ++
       "    procedure proc0(&mut self, s : &Struct0);\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    field0 : u8;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, s : &Struct0) {\n" ++
       "        self->field0 = s->field0;\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

-- | The same, with the field of the other object reached through @(*s).@ and
-- the field of the class written through @self->@.
testVUE002_2 :: String
testVUE002_2 = "struct Struct0 {\n" ++
       "    field0 : u8;\n" ++
       "};\n" ++
       "\n" ++
       "interface Interface0 {\n" ++
       "    procedure proc0(&mut self, s : &Struct0);\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    field0 : u8;\n" ++
       "\n" ++
       "    procedure proc0(&mut self, s : &Struct0) {\n" ++
       "        self->field0 = (*s).field0;\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

-- | A method that nobody calls, while a field of the class has its name. The
-- read of the field must not pass for a call to the method.
testVUE005_2 :: String
testVUE005_2 = "interface Interface0 {\n" ++
       "    procedure proc0(&mut self, data : &mut u8);\n" ++
       "};\n" ++
       "\n" ++
       "resource class ResourceClass0 provides Interface0 {\n" ++
       "\n" ++
       "    method0 : u8;\n" ++
       "    other0 : u8;\n" ++
       "\n" ++
       "    method method0(&self) -> u8 {\n" ++
       "        return self->other0;\n" ++
       "    }\n" ++
       "\n" ++
       "    procedure proc0(&mut self, data : &mut u8) {\n" ++
       "        *data = self->method0;\n" ++
       "        return;\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

-- | The declaration has no initializer and the branch that assigns it may not
-- be taken.
testVUE007 :: String
testVUE007 = "function fun0(c : bool) -> u32 {\n" ++
       "    var x : u32;\n" ++
       "    if (c) {\n" ++
       "        x = 1 : u32;\n" ++
       "    }\n" ++
       "    return x;\n" ++
       "}\n"

-- | An element is written before the array is assigned as a whole.
testVUE008 :: String
testVUE008 = "function fun0() -> u32 {\n" ++
       "    var buf : [u8; 4];\n" ++
       "    buf[0] = 1 : u8;\n" ++
       "    return 0 : u32;\n" ++
       "}\n"

spec :: Spec
spec = do
  describe "Semantic Errors" $ do
    it "VUE-001: invalid array indexing" $ do
     runNegativeTestVarUsage testVUE001
       `shouldSatisfy`
        isEUsedIgnoredParameter "_data"
    it "VUE-002: invalid array indexing" $ do
     runNegativeTestVarUsage testVUE002
       `shouldSatisfy`
        isENotUsed "opt"
    it "BE-001: box variable not moved (matched Some payload)" $ do
     runNegativeTestBoxUsage testBE001
       `shouldSatisfy`
        isEBoxNotMoved "obj"
    it "BE-001-1: box variable not moved (box parameter never consumed)" $ do
     runNegativeTestBoxUsage testBE001_1
       `shouldSatisfy`
        isEBoxNotMoved "data"
    it "BE-002: box variable moved twice (moved into option, then freed)" $ do
      runNegativeTestBoxUsage testBE002
        `shouldSatisfy`
          isEBoxMovedTwice "data"
    it "BE-002-1: box variable moved twice (freed twice)" $ do
      runNegativeTestBoxUsage testBE002_1
        `shouldSatisfy`
          isEBoxMovedTwice "data"
    it "BE-003: option-box variable moved twice" $ do
      runNegativeTestBoxUsage testBE003
        `shouldSatisfy`
          isEOptionBoxMovedTwice "opt"
    it "BE-004: option-box final state mismatch across branches" $ do
      runNegativeTestBoxUsage testBE004
        `shouldSatisfy`
          isEDifferentOptionBoxUse "opt"
    it "BE-005: option-box used in a branch that may not run" $ do
      runNegativeTestBoxUsage testBE005
        `shouldSatisfy`
          isEDifferentNewOptionBoxUse "opt"
    it "BE-007: box variable not moved in all branches" $ do
      runNegativeTestBoxUsage testBE007
        `shouldSatisfy`
          isEMissingBoxMove "data"
    it "BE-008: box variable moved in a branch that may not run" $ do
      runNegativeTestBoxUsage testBE008
        `shouldSatisfy`
          isEBoxMoveConditionalBranch "data"
    it "BE-009: option-box allocated but not moved" $ do
      runNegativeTestBoxUsage testBE009
        `shouldSatisfy`
          isEAllocNotMoved "opt"
    it "BE-010: option-box allocated twice" $ do
      runNegativeTestBoxUsage testBE010
        `shouldSatisfy`
          isEAllocTwice "opt"
    it "BE-011: option-box moved without being allocated" $ do
      runNegativeTestBoxUsage testBE011
        `shouldSatisfy`
          isEMovedWithoutAlloc "opt"
    it "BE-006: option-box used in a previous branch but missing in another" $ do
      runNegativeTestBoxUsage testBE006
        `shouldSatisfy`
          isEMissingOptionBox "opt"
    it "BE-012: option-box match missing the Some case" $ do
      runNegativeTestBoxUsage testBE012
        `shouldSatisfy`
          isEOptionBoxMatchMissingSomeCase
    it "VUE-004: method does not use self" $ do
      runNegativeTestVarUsage testVUE004
        `shouldSatisfy`
          isESelfNotUsed "method0"
    it "VUE-004: viewer does not use self" $ do
      runNegativeTestVarUsage testVUE004_1
        `shouldSatisfy`
          isESelfNotUsed "viewer0"
    it "VUE-003: action does not use self" $ do
      runNegativeTestVarUsage testVUE003
        `shouldSatisfy`
          isEActionSelfNotUsed "action0"
    it "VUE-005: method never called" $ do
      runNegativeTestVarUsage testVUE005
        `shouldSatisfy`
          isEMemberFunctionNotUsed "method0"
    it "VUE-005: viewer never called" $ do
      runNegativeTestVarUsage testVUE005_1
        `shouldSatisfy`
          isEMemberFunctionNotUsed "viewer0"
    it "VUE-006: value assigned and overwritten before being read" $ do
      runNegativeTestVarUsage testVUE006
        `shouldSatisfy`
          isEAssignedValueNotUsed "x"
    it "VUE-006: value assigned in a branch and overwritten after it" $ do
      runNegativeTestVarUsage testVUE006_1
        `shouldSatisfy`
          isEAssignedValueNotUsed "x"
    it "VUE-006: value assigned in a loop and never read" $ do
      runNegativeTestVarUsage testVUE006_2
        `shouldSatisfy`
          isEAssignedValueNotUsed "last"
    it "VUE-009: initializer overwritten before being read" $ do
      runNegativeTestVarUsage testVUE006_3
        `shouldSatisfy`
          isEInitializerNotUsed "x"
    it "VUE-009: initializer overwritten while a field of the same name is read" $ do
      runNegativeTestVarUsage testVUE006_4
        `shouldSatisfy`
          isEInitializerNotUsed "field0"
    it "VUE-009: the same, with the field reached through (*s).field" $ do
      runNegativeTestVarUsage testVUE006_5
        `shouldSatisfy`
          isEInitializerNotUsed "field0"
    it "VUE-002: class field not read while another object's field of the same name is" $ do
      runNegativeTestVarUsage testVUE002_1
        `shouldSatisfy`
          isENotUsed "field0"
    it "VUE-002: the same, with the other field reached through (*s).field" $ do
      runNegativeTestVarUsage testVUE002_2
        `shouldSatisfy`
          isENotUsed "field0"
    it "VUE-005: method never called while a field of the class has its name" $ do
      runNegativeTestVarUsage testVUE005_2
        `shouldSatisfy`
          isEMemberFunctionNotUsed "method0"
    it "VUE-007: object read on a path where it is not assigned" $ do
      runNegativeTestVarUsage testVUE007
        `shouldSatisfy`
          isEReadBeforeAssignment "x"
    it "VUE-008: element written before the array is assigned as a whole" $ do
      runNegativeTestVarUsage testVUE008
        `shouldSatisfy`
          isEPartialWriteBeforeAssignment "buf"

  where

    isEUsedIgnoredParameter :: Identifier -> Maybe VE.Error -> Bool
    isEUsedIgnoredParameter inIdent = \case Just (EUsedIgnoredParameter ident) -> (inIdent == ident); _ -> False

    isENotUsed :: Identifier -> Maybe VE.Error -> Bool
    isENotUsed inIdent = \case Just (ENotUsed ident) -> (inIdent == ident); _ -> False

    isEBoxNotMoved :: Identifier -> Maybe BE.Error -> Bool
    isEBoxNotMoved inIdent = \case Just (EBoxNotMoved ident) -> (inIdent == ident); _ -> False

    isEBoxMovedTwice :: Identifier -> Maybe BE.Error -> Bool
    isEBoxMovedTwice inIdent = \case Just (EBoxMovedTwice ident _) -> (inIdent == ident); _ -> False

    isEOptionBoxMovedTwice :: Identifier -> Maybe BE.Error -> Bool
    isEOptionBoxMovedTwice inIdent = \case Just (EOptionBoxMovedTwice ident _) -> (inIdent == ident); _ -> False

    isEDifferentOptionBoxUse :: Identifier -> Maybe BE.Error -> Bool
    isEDifferentOptionBoxUse inIdent = \case Just (EDifferentOptionBoxUse ident _ _) -> (inIdent == ident); _ -> False

    isEDifferentNewOptionBoxUse :: Identifier -> Maybe BE.Error -> Bool
    isEDifferentNewOptionBoxUse inIdent = \case Just (EDifferentNewOptionBoxUse ident _) -> (inIdent == ident); _ -> False

    isEMissingBoxMove :: Identifier -> Maybe BE.Error -> Bool
    isEMissingBoxMove inIdent = \case Just (EMissingBoxMove ident _) -> (inIdent == ident); _ -> False

    isEBoxMoveConditionalBranch :: Identifier -> Maybe BE.Error -> Bool
    isEBoxMoveConditionalBranch inIdent = \case Just (EBoxMoveConditionalBranch ident) -> (inIdent == ident); _ -> False

    isEAllocNotMoved :: Identifier -> Maybe BE.Error -> Bool
    isEAllocNotMoved inIdent = \case Just (EAllocNotMoved ident) -> (inIdent == ident); _ -> False

    isEAllocTwice :: Identifier -> Maybe BE.Error -> Bool
    isEAllocTwice inIdent = \case Just (EAllocTwice ident _) -> (inIdent == ident); _ -> False

    isEMovedWithoutAlloc :: Identifier -> Maybe BE.Error -> Bool
    isEMovedWithoutAlloc inIdent = \case Just (EMovedWithoutAlloc ident _) -> (inIdent == ident); _ -> False

    isEMissingOptionBox :: Identifier -> Maybe BE.Error -> Bool
    isEMissingOptionBox inIdent = \case Just (EMissingOptionBox ident _) -> (inIdent == ident); _ -> False

    isEOptionBoxMatchMissingSomeCase :: Maybe BE.Error -> Bool
    isEOptionBoxMatchMissingSomeCase = \case Just EOptionBoxMatchMissingSomeCase -> True; _ -> False

    isESelfNotUsed :: Identifier -> Maybe VE.Error -> Bool
    isESelfNotUsed inIdent = \case Just (ESelfNotUsed ident) -> (inIdent == ident); _ -> False

    isEActionSelfNotUsed :: Identifier -> Maybe VE.Error -> Bool
    isEActionSelfNotUsed inIdent = \case Just (EActionSelfNotUsed ident) -> (inIdent == ident); _ -> False

    isEAssignedValueNotUsed :: Identifier -> Maybe VE.Error -> Bool
    isEAssignedValueNotUsed inIdent = \case Just (EAssignedValueNotUsed ident) -> (inIdent == ident); _ -> False

    isEMemberFunctionNotUsed :: Identifier -> Maybe VE.Error -> Bool
    isEMemberFunctionNotUsed inIdent = \case Just (EMemberFunctionNotUsed ident) -> (inIdent == ident); _ -> False

    isEReadBeforeAssignment :: Identifier -> Maybe VE.Error -> Bool
    isEReadBeforeAssignment inIdent = \case Just (EReadBeforeAssignment ident) -> (inIdent == ident); _ -> False

    isEPartialWriteBeforeAssignment :: Identifier -> Maybe VE.Error -> Bool
    isEPartialWriteBeforeAssignment inIdent = \case Just (EPartialWriteBeforeAssignment ident) -> (inIdent == ident); _ -> False

    isEInitializerNotUsed :: Identifier -> Maybe VE.Error -> Bool
    isEInitializerNotUsed inIdent = \case Just (EInitializerNotUsed ident) -> (inIdent == ident); _ -> False