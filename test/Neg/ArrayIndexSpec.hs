module Neg.ArrayIndexSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Monad
import Semantic.Errors.Errors
import AST.Seman
import Utils.Annotations

runNegativeTest :: String -> Maybe (Error Annotation)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing

test0 :: String
test0 = "function access_to_slice() -> u8 {\n" ++
        "    var array0: [u8; 10] = [0; 10];\n" ++
        "\n" ++
        "    let x : u8 = array0[1..4][0];\n" ++
        "\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

spec :: Spec
spec = do
  describe "Array indexing" $ do
    it "Indexing an array slice" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEIfElseNoOtherwise
  
  where
    isEIfElseNoOtherwise :: Maybe (Error Annotation) -> Bool
    isEIfElseNoOtherwise = \case Just (EArray (Slice UInt8)) -> True; _ -> False