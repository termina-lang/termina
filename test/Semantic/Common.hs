module Semantic.Common where

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Environment
import qualified Semantic.Errors as Semantic
import Utils.Annotations
import Utils.Errors (ErrorMessage(errorIdent))
import Configuration.Platform
import Configuration.Configuration
import Data.Text (Text)
import qualified Data.Set as S

-- | Parses and type-checks a single module named @test@, returning the
-- semantic error it is expected to raise (or 'Nothing' if it type-checks).
runNegativeTestTypeCheck :: String -> Maybe Semantic.Error
runNegativeTestTypeCheck input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing

-- | Type-checks module @test@ on a given target platform, returning the
-- diagnostic code of the first error (or 'Nothing' if it type-checks). Lets a
-- spec contrast platform-gated checks, e.g. the packed-member reference rule
-- that only fires on strict-alignment targets.
typeCheckErrorOn :: Platform -> String -> Maybe Text
typeCheckErrorOn plt input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" plt in
    case runTypeChecking (makeInitialGlobalEnv (Just config) plt []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> Just $ errorIdent err
      Right _ -> Nothing
