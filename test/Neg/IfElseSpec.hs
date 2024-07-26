module Neg.IfElseSpec (spec) where

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
test0 = "function keep_inside(n: usize, array : &mut[u8;64], range: &[u8;2]) {\n" ++
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
        "    \n" ++
        "    return;\n" ++
        "}"

test1 :: String
test1 = "function keep_inside(n: usize, array : &mut[u8;64], range: &[u8;2]) {\n" ++
        "    let lower_limit : u8 = range[1 : usize];\n" ++
        "    let upper_limit : u8 = range[2 : usize];\n" ++
        "    let x : u8 = (*array)[n];\n" ++
        "\n" ++
        "    if (1 : u32) {\n" ++
        "        //do nothing\n" ++
        "    }\n" ++
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
    it "If condition not bool" $ do
     runNegativeTest test1
       `shouldSatisfy`
        isEIfElseIfCondNotBool
  
  where
    isEIfElseNoOtherwise :: Maybe (Error Annotation) -> Bool
    isEIfElseNoOtherwise = \case Just EIfElseNoOtherwise -> True; _ -> False

    isEIfElseIfCondNotBool :: Maybe (Error Annotation) -> Bool
    isEIfElseIfCondNotBool = \case Just (EIfElseIfCondNotBool UInt32) -> True; _ -> False