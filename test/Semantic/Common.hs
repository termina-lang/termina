module Semantic.Common where

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Environment
import qualified Semantic.Errors as Semantic
import Utils.Annotations
import Configuration.Platform
import Configuration.Configuration
import qualified Data.Set as S

-- | Parses and type-checks a single module named @test@, returning the
-- semantic error it is expected to raise (or 'Nothing' if it type-checks).
runNegativeTestTypeCheck :: String -> Maybe Semantic.Error
runNegativeTestTypeCheck input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing
