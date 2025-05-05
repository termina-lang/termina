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
import qualified ControlFlow.VarUsage.Errors as VarUsage
import ControlFlow.VarUsage

runNegativeTestTypeCheck :: String -> Maybe Semantic.Error
runNegativeTestTypeCheck input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing

runNegativeTestVarUsage :: String -> Maybe VarUsage.Error
runNegativeTestVarUsage input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule ast) of
      Left err -> error $ "Typing Error: " ++ show err
      Right (typedProgram, _) -> case runGenBBModule typedProgram of
        Left err -> error $ "Basic Blocks Generator Error: " ++ show err
        Right bbProgram -> case runUDAnnotatedProgram bbProgram of
          Just err -> Just $ getError err
          Nothing -> Nothing