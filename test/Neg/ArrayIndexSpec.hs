module Neg.ArrayIndexSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Monad
import Semantic.Errors.Errors
import Utils.Annotations

runNegativeTest :: String -> Maybe (Error TLocation)
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
  describe "TArray indexing" $ do
    it "Indexing an array slice" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEIfElseNoOtherwise
  
  where
    isEIfElseNoOtherwise :: Maybe (Error TLocation) -> Bool
    isEIfElseNoOtherwise = \case Just ESliceInvalidUse -> True; _ -> False