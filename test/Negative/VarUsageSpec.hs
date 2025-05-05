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

spec :: Spec
spec = do
  describe "Semantic Errors" $ do
    it "VE-001: invalid array indexing" $ do
     runNegativeTestVarUsage testVE001
       `shouldSatisfy`
        isEUsedIgnoredParameter "_data"

  where

    isEUsedIgnoredParameter :: Identifier -> Maybe Error -> Bool
    isEUsedIgnoredParameter inIdent = \case Just (EUsedIgnoredParameter ident) -> (inIdent == ident); _ -> False