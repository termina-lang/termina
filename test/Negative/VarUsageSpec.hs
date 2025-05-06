module Negative.VarUsageSpec (spec) where

import Test.Hspec
import Semantic.AST
import ControlFlow.VarUsage.Errors
import Negative.Common

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

  where

    isEUsedIgnoredParameter :: Identifier -> Maybe Error -> Bool
    isEUsedIgnoredParameter inIdent = \case Just (EUsedIgnoredParameter ident) -> (inIdent == ident); _ -> False

    isENotUsed :: Identifier -> Maybe Error -> Bool
    isENotUsed inIdent = \case Just (ENotUsed ident) -> (inIdent == ident); _ -> False

    isEBoxNotMoved :: Identifier -> Maybe Error -> Bool
    isEBoxNotMoved inIdent = \case Just (EBoxNotMoved ident) -> (inIdent == ident); _ -> False

    isEBoxMovedTwice :: Identifier -> Maybe Error -> Bool
    isEBoxMovedTwice inIdent = \case Just (EBoxMovedTwice ident _) -> (inIdent == ident); _ -> False