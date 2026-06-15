-- | Runs the constant-folding pass on a single in-memory module named @test@ in
-- isolation: parse, type-check, lower to basic blocks, wrap as a basic-blocks
-- module and fold it. Returns the rich error raised (or 'Nothing' if every
-- constant folds cleanly).
module ConstFolding.Common (constFoldError) where

import Text.Parsec (runP)
import Data.Text (pack)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Parser.Parsing (contents, topLevel)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Environment (makeInitialGlobalEnv)
import Configuration.Platform (Platform(TestPlatform))
import Configuration.Configuration (defaultConfig)
import Generator.Environment (getPlatformInitialGlobalEnv)
import ControlFlow.BasicBlocks (runGenBBModule)
import ControlFlow.ConstFolding (runConstFolding, constFoldModule)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ConstFolding.Errors (ConstFoldError)
import Command.Types (BasicBlocksData(BasicBlockData))
import Modules.Modules (TerminaModuleData(..))

constFoldError :: String -> Maybe ConstFoldError
constFoldError input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform
        env = makeInitialGlobalEnv (Just config) (getPlatformInitialGlobalEnv config TestPlatform)
    in case runTypeChecking env (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing error: " ++ show err
      Right (typed, _) -> case runGenBBModule typed of
        Left err -> error $ "Basic Blocks error: " ++ show err
        Right bb ->
          let bbModule = TerminaModuleData "test" "test" dummyTime [] [] (pack input) (BasicBlockData bb)
          in either Just (const Nothing) (runConstFolding (ConstFoldEnv M.empty) (constFoldModule bbModule))

dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 1997 8 29) (secondsToDiffTime 0)
