-- | Runs the architecture stage on a single in-memory module named @test@ in
-- isolation: parse, type-check, lower to basic blocks, generate the
-- architecture, then run the five wiring checks (emitter/channel/resource/pool/
-- box). Returns the rich error raised (or 'Nothing' if the wiring is sound).
module Architecture.Common (architectureError) where

import Text.Parsec (runP)
import Parser.Parsing (contents, topLevel)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Environment (makeInitialGlobalEnv)
import Configuration.Platform (Platform(TestPlatform))
import Configuration.Configuration (defaultConfig)
import Generator.Environment (getPlatformInitialGlobalEnv, getPlatformInitialProgram)
import ControlFlow.BasicBlocks (runGenBBModule)
import ControlFlow.Architecture (runGenArchitecture)
import ControlFlow.Architecture.Checks
  ( runCheckEmitterConnections, runCheckChannelConnections
  , runCheckResourceUsage, runCheckPoolUsage, runCheckBoxSources )
import ControlFlow.Architecture.Errors (ArchitectureError)
import qualified Data.Set as S

architectureError :: String -> Maybe ArchitectureError
architectureError input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform
        env = makeInitialGlobalEnv (Just config) (getPlatformInitialGlobalEnv config TestPlatform)
        initialProg = getPlatformInitialProgram config TestPlatform
    in case runTypeChecking env (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing error: " ++ show err
      Right (typed, _) -> case runGenBBModule typed of
        Left err -> error $ "Basic Blocks error: " ++ show err
        Right bb -> case runGenArchitecture initialProg "test" bb of
          Left err -> Just err
          Right progArch -> either Just (const Nothing) $ sequence_
            [ runCheckEmitterConnections progArch
            , runCheckChannelConnections progArch
            , runCheckResourceUsage progArch
            , runCheckPoolUsage progArch
            , runCheckBoxSources progArch ]
