-- | Runs the exit-path (basic-block) check on a single in-memory module named
-- @test@ in isolation: parse, type-check, lower to basic blocks, then run the
-- exit-path check, returning the rich error it raises (or 'Nothing' if every
-- path is well-formed).
module ExitPaths.Common (exitPathsError) where

import Text.Parsec (runP)
import Parser.Parsing (contents, topLevel)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Environment (makeInitialGlobalEnv)
import Configuration.Platform (Platform(TestPlatform))
import Configuration.Configuration (defaultConfig)
import ControlFlow.BasicBlocks (runGenBBModule)
import ControlFlow.BasicBlocks.Checks.ExitPaths (runCheckExitPaths)
import ControlFlow.BasicBlocks.Checks.ExitPaths.Errors (PathsCheckError)
import qualified Data.Set as S

exitPathsError :: String -> Maybe PathsCheckError
exitPathsError input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) [])
           (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing error: " ++ show err
      Right (typed, _) -> case runGenBBModule typed of
        Left err -> error $ "Basic Blocks error: " ++ show err
        Right bb -> either Just (const Nothing) (runCheckExitPaths bb)
