module Negative.VarNotDefSpec (spec) where

import Test.Hspec
import Semantic.Errors
import Negative.Common

test0 :: String
test0 = "function test0() {\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Error Neg Test" $ do
    it "Undeclared variable" $ do
     runNegativeTest test0
       `shouldSatisfy`
        (\case Just (ENotNamedObject "foo") -> True; _ -> False)

