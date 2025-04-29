module Negative.Common where

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Environment
import qualified Semantic.Errors as Semantic
import Utils.Annotations
import Configuration.Platform
import Configuration.Configuration
import ControlFlow.BasicBlocks
import qualified ControlFlow.Architecture.Errors as BBlock

runNegativeTestTypeCheck :: String -> Maybe Semantic.Error
runNegativeTestTypeCheck input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing

runNegativeTestBBlock :: String -> Maybe BBlock.Error
runNegativeTestBBlock input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule ast) of
      Left err -> error $ "Typing Error: " ++ show err
      Right (typedProgram, _) -> case runGenBBModule typedProgram of
        Left err -> Just $ getError err
        Right _ -> Nothing
