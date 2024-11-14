module Negative.Semantic.InvalidArrayIndexingSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors.Errors
import Negative.Common

test0:: String
test0 = "function test0() {\n" ++
        "\n" ++
        "    var foo : u32 = 0;\n" ++
        "\n" ++
        "    foo[0] = 1024;\n" ++
        "\n" ++
        "    return;\n" ++
        "\n" ++
        "}\n"

spec :: Spec
spec = do
  describe "Semantic Error SE-001" $ do
    it "Invalid array indexing" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEInvalidArrayIndexing
  
  where
    isEInvalidArrayIndexing :: Maybe Error -> Bool
    isEInvalidArrayIndexing = \case Just (EInvalidArrayIndexing TUInt32) -> True; _ -> False