module Negative.VarUsageSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors
import Negative.Common
import Semantic.Types

testVE001 :: String
testVE001 = "function fun0(_data : u32) -> u32 {\n" ++
       "\n" ++
       "    let ret : u32 = _data + 1;\n" ++
       "\n" ++
       "    return ret;\n" ++
       "\n" ++
       "}\n"

