module Neg.VarNotDefSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Errors
import AST.Parser

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

spec :: Spec
spec = do
  describe "Error Neg Test" $ do
    it "Undeclared variable" $ do
     runNegativeTest test0
       `shouldBe`
        Just (ENotNamedObject "foo")

