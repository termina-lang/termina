module Neg.ReferenceSpec (spec) where

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
test0 = "function test_ref(input : &mut u16) {\n" ++
        "    *input = 1024;\n" ++
        "    return;\n" ++
        "}\n" ++
        "\n" ++
        "function test0() {\n" ++
        "    let foo : u16 = 1024;\n" ++
        "    test_ref(&mut foo);\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "References" $ do
    it "Mutable reference to immutable object" $ do
     runNegativeTest test0
       `shouldSatisfy`
        (\case Just EMutableReferenceToImmutable -> True; _ -> False)

