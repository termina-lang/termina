module Neg.VarNotDefSpec (spec) where

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
test0 = "function test0() {\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "function f (x : u8) -> u8 {\n" ++
        "     x = x * 2 : u8;\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

test2 :: String
test2 = "function f (n : u8) -> u8 {\n" ++
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

spec :: Spec
spec = do
  describe "Error Neg Test" $ do
    it "Undeclared variable" $ do
     runNegativeTest test0
       `shouldBe`
        Just (ENotNamedObject "foo")
{--    it "Assignment to input variable" $ do
     runNegativeTest test1
       `shouldBe`
        Just EAssignmentToImmutable --}
    it "Loop iterator not constant" $ do
     runNegativeTest test2
       `shouldBe`
        Just (ENotNamedObject "ntimes")
