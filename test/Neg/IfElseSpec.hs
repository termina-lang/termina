module Neg.IfElseSpec (spec) where

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
test0 = "function keep_inside(n: usize, array : &mut[u8;64], range:[u8;2]) {\n" ++
        "    let lower_limit : u8 = range[1 : usize];\n" ++
        "    let upper_limit : u8 = range[2 : usize];\n" ++
        "    let x : u8 = (*array)[n];\n" ++
        "\n" ++
        "    if ((x > lower_limit) && (x < upper_limit)){\n" ++
        "        //do nothing\n" ++
        "    }\n" ++
        "    else if (x > lower_limit) && (x - lower_limit < 5 :u8) {\n" ++
        "        //do nothing\n" ++
        "    }\n" ++
        "    else if (x > upper_limit) && (x - upper_limit < 5 :u8) {\n" ++
        "        //do nothing\n" ++
        "    }\n" ++
        "    // else { (*array)[n] = 0 : u8; }\n" ++
        "    \n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Variable mutability" $ do
    it "Assignment to input variable" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEIfElseNoOtherwise
  
  where
    isEIfElseNoOtherwise :: Maybe (Errors Annotation) -> Bool
    isEIfElseNoOtherwise = (\case Just EIfElseNoOtherwise -> True; _ -> False)