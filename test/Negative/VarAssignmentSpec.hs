module Negative.VarAssignmentSpec (spec) where

import Test.Hspec
import Semantic.Errors
import Negative.Common

test0 :: String
test0 = "function f (n : u8) -> u8 {\n" ++
        "    let ntimes : u8 = n;\n" ++
        "    var y : u8 = 0 : u8;\n" ++
        "\n" ++
        "    for i : u8 in 0 .. ntimes {\n" ++
        "        y = i + y;\n" ++
        "    }\n" ++
        "\n" ++
        "    return y;\n" ++
        "\n" ++
        "}"

test1 :: String
test1 = "function f (x : u8) -> u8 {\n" ++
        "     x = x * 2 : u8;\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

test2 :: String
test2 = "function f (n : u8) {\n" ++
        "\n" ++
        "    for i : u8 in 0 .. 10 {\n" ++
        "        i = i + n;\n" ++
        "    }\n" ++
        "\n" ++
        "    return;\n" ++
        "\n" ++
        "}"

test3 :: String
test3 = "function init_val (array : &[u8;6]) -> u8 {\n" ++
        "    (*array)[3] = 0 : u8;\n" ++
        "    return 0 : u8;\n" ++
        "}\n"

test4 :: String
test4 = "function f (n : u8) -> u8 {\n" ++
        "    let foo : u8 = n;\n" ++
        "    foo = foo + 32;\n" ++
        "    return foo;\n" ++
        "}\n"

spec :: Spec
spec = do
  describe "Variable mutability" $ do
    it "Assignment to input variable" $ do
     runNegativeTest test0
       `shouldSatisfy`
        (\case Just EExpressionNotConstant -> True; _ -> False)
    it "Loop bound not constant" $ do
     runNegativeTest test1
       `shouldSatisfy`
        isEAssignmentToImmutable
    it "Loop iterator assignment" $ do
     runNegativeTest test2
       `shouldSatisfy`
        isEAssignmentToImmutable
    it "Write to an immutable reference" $ do
     runNegativeTest test3
       `shouldSatisfy`
        isEAssignmentToImmutable
    it "Write to an immutable local variable" $ do
     runNegativeTest test4
       `shouldSatisfy`
        isEAssignmentToImmutable
  
  where
    isEAssignmentToImmutable :: Maybe Error -> Bool
    isEAssignmentToImmutable = \case Just EAssignmentToImmutable -> True; _ -> False