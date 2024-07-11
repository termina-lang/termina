module Neg.CastingSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Monad
import Semantic.Errors
import AST.Seman

runNegativeTest :: String -> Maybe (Errors Annotation)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking initialExpressionSt (typeTerminaModule ast) of
      Left err -> Just $ semError err
      Right _ -> Nothing

test0 :: String
test0 = "function f() {\n" ++
        "    let foo : bool = true;\n" ++
        "\n" ++
        "    var casting : u32 = foo as u32;\n" ++
        "\n" ++
        "    return;\n" ++
        "\n" ++
        "}"

spec :: Spec
spec = do
  describe "Casting errors" $ do
    it "Invalid casting from bool to u32" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isECasteable
  
  where
    isECasteable :: Maybe (Errors Annotation) -> Bool
    isECasteable = \case Just (ENotCasteable Bool UInt32) -> True; _ -> False