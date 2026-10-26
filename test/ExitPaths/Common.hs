-- | Runs the exit-path (basic-block) check on a single in-memory module named
-- @test@ in isolation: parse, type-check, lower to basic blocks, then run the
-- exit-path check, returning the rich error it raises (or 'Nothing' if every
-- path is well-formed). 'exitPathsTestError' type-checks it as a test build,
-- where test classes are allowed.
module ExitPaths.Common (exitPathsError, exitPathsTestError) where

import Text.Parsec (runP)
import Parser.Parsing (contents, topLevel)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Environment (makeInitialGlobalEnv)
import Configuration.Platform (Platform(TestPlatform))
import Configuration.Configuration (TerminaConfig(..), DeploymentKind(..), defaultConfig)
import Lowering (runLowerModule)
import ControlFlow.ExitPaths (runCheckExitPaths)
import ControlFlow.ExitPaths.Errors (PathsCheckError)
import qualified Data.Set as S
import Data.Text (pack)

exitPathsError :: String -> Maybe PathsCheckError
exitPathsError = exitPathsErrorWith (defaultConfig "test" TestPlatform)

exitPathsTestError :: String -> Maybe PathsCheckError
exitPathsTestError = exitPathsErrorWith
  ((defaultConfig "test" TestPlatform) { deploymentKind = TestBuild (pack "test") })

exitPathsErrorWith :: TerminaConfig -> String -> Maybe PathsCheckError
exitPathsErrorWith config input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser error: " ++ show err
  Right ast ->
    case runTypeChecking (makeInitialGlobalEnv (Just config) TestPlatform [])
           (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing error: " ++ show err
      Right (typed, _) -> case runLowerModule typed of
        Left err -> error $ "Basic Blocks error: " ++ show err
        Right bb -> either Just (const Nothing) (runCheckExitPaths bb)
