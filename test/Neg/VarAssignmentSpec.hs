module Neg.VarAssignmentSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Errors

runNegativeTest :: String -> Maybe (Errors Annotation)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> Just $ semError err
      Right _ -> Nothing

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

spec :: Spec
spec = do
  describe "Variable mutability" $ do
    it "Assignment to input variable" $ do
     runNegativeTest test0
       `shouldBe`
        Just (ENotNamedObject "ntimes")
    it "Loop bound not constant" $ do
     runNegativeTest test1
       `shouldSatisfy`
        (testError EAssignmentToImmutable)
    it "Loop iterator assignment" $ do
     runNegativeTest test2
       `shouldSatisfy`
        (testError EAssignmentToImmutable)
  
  where
    testError :: Errors Annotation -> Maybe (Errors Annotation) ->  Bool
    testError e = \case Just e -> True; _ -> False
