-- | VarUsage (move/borrow) positive tests: well-formed box/option-box usage
-- raises no error (the check returns 'Nothing').
module VarUsage.Positive.UsageSpec (spec) where

import VarUsage.Common (runNegativeTestVarUsage)

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

-- | A box consumed (freed) on every branch of an if.
freesBoxInBothBranches :: String
freesBoxInBothBranches = wrap $
  "        var flag : bool = true;\n" ++
  "        if (flag) {\n" ++
  "            self->data_pool.free(data);\n" ++
  "        } else {\n" ++
  "            self->data_pool.free(data);\n" ++
  "        }\n"

spec :: Spec
spec = describe "VarUsage: well-formed usage raises no error" $
  mapM_ (\(name, src) -> it name $ runNegativeTestVarUsage src `shouldSatisfy` isNothing)
    [ ("accepts a box parameter that is freed once", freesBoxParam)
    , ("accepts an option-box allocated and consumed in all branches", allocAndConsumes)
    , ("accepts a box freed on every branch of an if", freesBoxInBothBranches)
    ]
