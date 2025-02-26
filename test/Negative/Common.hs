module Negative.Common where

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Environment
import Semantic.Errors
import Utils.Annotations
import Configuration.Platform
import Configuration.Configuration

runNegativeTest :: String -> Maybe Error
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing
