module VarUsage.VarUsageSpec (spec) where

import Test.Hspec
import Semantic.AST
import ControlFlow.VarUsage.Errors
import VarUsage.Common

testVE001 :: String
testVE001 = "function fun0(_data : u32) -> u32 {\n" ++
       "\n" ++
       "    let ret : u32 = _data + 1;\n" ++
       "\n" ++
       "    return ret;\n" ++
       "\n" ++
       "}\n"

testVE002 :: String
testVE002 = "interface Interface0 {\n" ++
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

testVE003 :: String
testVE003 = "interface Interface0 {\n" ++
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

testVE003_1 :: String
testVE003_1 = "interface Interface0 {\n" ++
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

testVE004 :: String
testVE004 = "interface Interface0 {\n" ++
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

testVE004_1 :: String
testVE004_1 = "interface Interface0 {\n" ++
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

-- VE-005: the option-box @opt@ is allocated once but moved (matched) twice.
testVE005 :: String
testVE005 = veWrap (veFreeData ++ veDeclOpt ++ veAlloc ++ veMatchMove ++ veMatchMove)

-- VE-006: @opt@ ends one branch moved and the other branch allocated, so the
-- final option-box state differs across branches.
testVE006 :: String
testVE006 = veWrap (
  veDeclOpt ++ veAlloc ++
  "        if (data == 0) {\n" ++ veMatchMove ++
  "        } else {\n" ++ veAlloc ++ veMatchMove ++ "        }\n" ++
  veFreeData)

-- VE-007: @opt@ is moved inside a for loop, i.e. in a branch that may not run.
testVE007 :: String
testVE007 = veWrap (
  veFreeData ++ veDeclOpt ++ veAlloc ++
  "        for i : usize in 0 .. 4 {\n" ++ veMatchMove ++ "        }\n")

-- VE-009: the box @data@ is moved in the @if@ branch but not in the @else@.
testVE009 :: String
testVE009 = veWrap (
  "        if (data == 0) {\n" ++ veFreeData ++ "        } else {\n        }\n")

-- VE-010: the box @data@ is moved inside a for loop (a branch that may not run).
testVE010 :: String
testVE010 = veWrap ("        for i : usize in 0 .. 4 {\n" ++ veFreeData ++ "        }\n")

-- VE-011: @opt@ is allocated but never moved afterwards.
testVE011 :: String
testVE011 = veWrap (veFreeData ++ veDeclOpt ++ veAlloc)

-- VE-012: @opt@ is allocated twice before being moved.
testVE012 :: String
testVE012 = veWrap (veFreeData ++ veDeclOpt ++ veAlloc ++ veAlloc ++ veMatchMove)

-- VE-013: @opt@ is moved (matched) without ever having been allocated.
testVE013 :: String
testVE013 = veWrap (veFreeData ++ veDeclOpt ++ veMatchMove)

-- VE-008: @opt@ is allocated only in the @if@ branch (with an explicit @else@
-- that leaves it alone) and then used after the merge, so it is used in a
-- previous branch but missing in another.
testVE008 :: String
testVE008 = veWrap (
  veDeclOpt ++
  "        if (data == 0) {\n" ++ veAlloc ++ "        } else {\n        }\n" ++
  veMatchMove ++ veFreeData)

-- VE-014: matching an option-box with a default @case _@ instead of an explicit
-- @Some@ case. The type checker accepts the match as exhaustive, but the usage
-- analysis flags the missing @Some@ case.
testVE014 :: String
testVE014 =
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

spec :: Spec
spec = do
  describe "Semantic Errors" $ do
    it "VE-001: invalid array indexing" $ do
     runNegativeTestVarUsage testVE001
       `shouldSatisfy`
        isEUsedIgnoredParameter "_data"
    it "VE-002: invalid array indexing" $ do
     runNegativeTestVarUsage testVE002
       `shouldSatisfy`
        isENotUsed "opt"
    it "VE-003: box variable not moved" $ do
     runNegativeTestVarUsage testVE003
       `shouldSatisfy`
        isEBoxNotMoved "obj"
    it "VE-003-1: box variable not moved" $ do
     runNegativeTestVarUsage testVE003_1
       `shouldSatisfy`
        isEBoxNotMoved "data"
    it "VE-004: box variable moved twice" $ do
      runNegativeTestVarUsage testVE004
        `shouldSatisfy`
          isEBoxMovedTwice "data"
    it "VE-004-1: box variable moved twice" $ do
      runNegativeTestVarUsage testVE004_1
        `shouldSatisfy`
          isEBoxMovedTwice "data"
    it "VE-005: option-box variable moved twice" $ do
      runNegativeTestVarUsage testVE005
        `shouldSatisfy`
          isEOptionBoxMovedTwice "opt"
    it "VE-006: option-box final state mismatch across branches" $ do
      runNegativeTestVarUsage testVE006
        `shouldSatisfy`
          isEDifferentOptionBoxUse "opt"
    it "VE-007: option-box used in a branch that may not run" $ do
      runNegativeTestVarUsage testVE007
        `shouldSatisfy`
          isEDifferentNewOptionBoxUse "opt"
    it "VE-009: box variable not moved in all branches" $ do
      runNegativeTestVarUsage testVE009
        `shouldSatisfy`
          isEMissingBoxMove "data"
    it "VE-010: box variable moved in a branch that may not run" $ do
      runNegativeTestVarUsage testVE010
        `shouldSatisfy`
          isEBoxMoveConditionalBranch "data"
    it "VE-011: option-box allocated but not moved" $ do
      runNegativeTestVarUsage testVE011
        `shouldSatisfy`
          isEAllocNotMoved "opt"
    it "VE-012: option-box allocated twice" $ do
      runNegativeTestVarUsage testVE012
        `shouldSatisfy`
          isEAllocTwice "opt"
    it "VE-013: option-box moved without being allocated" $ do
      runNegativeTestVarUsage testVE013
        `shouldSatisfy`
          isEMovedWithoutAlloc "opt"
    it "VE-008: option-box used in a previous branch but missing in another" $ do
      runNegativeTestVarUsage testVE008
        `shouldSatisfy`
          isEMissingOptionBox "opt"
    it "VE-014: option-box match missing the Some case" $ do
      runNegativeTestVarUsage testVE014
        `shouldSatisfy`
          isEOptionBoxMatchMissingSomeCase

  where

    isEUsedIgnoredParameter :: Identifier -> Maybe Error -> Bool
    isEUsedIgnoredParameter inIdent = \case Just (EUsedIgnoredParameter ident) -> (inIdent == ident); _ -> False

    isENotUsed :: Identifier -> Maybe Error -> Bool
    isENotUsed inIdent = \case Just (ENotUsed ident) -> (inIdent == ident); _ -> False

    isEBoxNotMoved :: Identifier -> Maybe Error -> Bool
    isEBoxNotMoved inIdent = \case Just (EBoxNotMoved ident) -> (inIdent == ident); _ -> False

    isEBoxMovedTwice :: Identifier -> Maybe Error -> Bool
    isEBoxMovedTwice inIdent = \case Just (EBoxMovedTwice ident _) -> (inIdent == ident); _ -> False

    isEOptionBoxMovedTwice :: Identifier -> Maybe Error -> Bool
    isEOptionBoxMovedTwice inIdent = \case Just (EOptionBoxMovedTwice ident _) -> (inIdent == ident); _ -> False

    isEDifferentOptionBoxUse :: Identifier -> Maybe Error -> Bool
    isEDifferentOptionBoxUse inIdent = \case Just (EDifferentOptionBoxUse ident _ _) -> (inIdent == ident); _ -> False

    isEDifferentNewOptionBoxUse :: Identifier -> Maybe Error -> Bool
    isEDifferentNewOptionBoxUse inIdent = \case Just (EDifferentNewOptionBoxUse ident _) -> (inIdent == ident); _ -> False

    isEMissingBoxMove :: Identifier -> Maybe Error -> Bool
    isEMissingBoxMove inIdent = \case Just (EMissingBoxMove ident _) -> (inIdent == ident); _ -> False

    isEBoxMoveConditionalBranch :: Identifier -> Maybe Error -> Bool
    isEBoxMoveConditionalBranch inIdent = \case Just (EBoxMoveConditionalBranch ident) -> (inIdent == ident); _ -> False

    isEAllocNotMoved :: Identifier -> Maybe Error -> Bool
    isEAllocNotMoved inIdent = \case Just (EAllocNotMoved ident) -> (inIdent == ident); _ -> False

    isEAllocTwice :: Identifier -> Maybe Error -> Bool
    isEAllocTwice inIdent = \case Just (EAllocTwice ident _) -> (inIdent == ident); _ -> False

    isEMovedWithoutAlloc :: Identifier -> Maybe Error -> Bool
    isEMovedWithoutAlloc inIdent = \case Just (EMovedWithoutAlloc ident _) -> (inIdent == ident); _ -> False

    isEMissingOptionBox :: Identifier -> Maybe Error -> Bool
    isEMissingOptionBox inIdent = \case Just (EMissingOptionBox ident _) -> (inIdent == ident); _ -> False

    isEOptionBoxMatchMissingSomeCase :: Maybe Error -> Bool
    isEOptionBoxMatchMissingSomeCase = \case Just EOptionBoxMatchMissingSomeCase -> True; _ -> False