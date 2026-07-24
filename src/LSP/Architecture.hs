{-# LANGUAGE OverloadedStrings #-}

-- | LSP-side generation of the program architecture.
--
-- The build command runs the post-typecheck pipeline in 'IO' and aborts the
-- process on the first error. The language server cannot do that: it must stay
-- alive and must not write to stdout, which is its transport. This module reruns
-- the same pipeline (basic blocks, path and use\/def checks, constant folding
-- and architecture generation) purely, returning either the architecture or a
-- short error message, so the server can serve it through a custom request.
module LSP.Architecture (buildArchitectureFromStore) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified LSP.Modules as Stored

import Command.Types (BasicBlocksProject, TypedModule, TypedProject, basicBlocksAST)
import Command.Utils
    ( basicBlockPathsCheckModules
    , genBasicBlocks
    , sortProjectDepsOrLoop
    , useDefCheckModules
    )
import Configuration.Configuration (TerminaConfig, appFilename)
import Configuration.Platform (Platform)
import ControlFlow.Architecture (runGenArchitecture)
import ControlFlow.Architecture.Types (TerminaProgArch)
import ControlFlow.ConstFolding (constFoldModule, runConstFolding)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv (..))
import Generator.Environment (getPlatformInitialProgram)
import Modules.Modules (TerminaModuleData (..))
import Semantic.Types (SemanticAnn)
import System.FilePath (isAbsolute, takeFileName, (<.>))
import Utils.Annotations (QualifiedName)

-- | Reconstruct the typed project from the server's stored modules and, when
-- every module has been type checked, run the pipeline that produces the
-- program architecture. Returns a short diagnostic message on failure.
buildArchitectureFromStore
  :: Platform
  -> TerminaConfig
  -> M.Map QualifiedName Stored.TerminaStoredModule
  -> Either T.Text (TerminaProgArch SemanticAnn)
buildArchitectureFromStore plt cfg stored = do
  typedModules <- maybe (Left "project not fully type checked") Right
                    (M.traverseWithKey storedToTyped stored)
  -- The server keys the application module by its absolute path, whereas the
  -- rest of the pipeline (and the build command) names it by the configured
  -- application filename. Rekey it so the emitted module names match the build
  -- output and never leak absolute paths.
  let typedProject = M.mapKeys rekeyApp typedModules
  ordered <- either (const (Left "dependency cycle in project")) Right
                    (sortProjectDepsOrLoop (M.map importedModules typedProject))
  rawBB <- either (const (Left "basic block generation failed")) Right
                    (genBasicBlocks typedProject)
  maybe (Right ()) (const (Left "basic block path check failed"))
        (basicBlockPathsCheckModules rawBB)
  maybe (Right ()) (const (Left "use/def check failed"))
        (useDefCheckModules rawBB)
  foldedBB <- constFold ordered rawBB
  genArch (getPlatformInitialProgram cfg plt) foldedBB ordered

  where

    -- | Map the absolute-path key of the application module to its configured
    -- name; leave source-module keys (already qualified names) untouched.
    rekeyApp :: QualifiedName -> QualifiedName
    rekeyApp k
      | isAbsolute k && takeFileName k == appFilename cfg <.> "fin" = appFilename cfg
      | otherwise = k

    -- | A stored module becomes a typed module only if it carries semantic data.
    -- The modification time and the visible-module list are not read by the
    -- pipeline, so placeholders are used for them.
    storedToTyped :: QualifiedName -> Stored.TerminaStoredModule -> Maybe TypedModule
    storedToTyped qn sm = do
      sem <- Stored.semantic sm
      pure $ TerminaModuleData
               qn
               (Stored.fullPath sm)
               epoch
               (Stored.importedModules sm)
               []
               (Stored.sourcecode sm)
               sem

    epoch :: UTCTime
    epoch = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

    -- | Fold the modules in dependency order, threading the constant environment.
    constFold :: [QualifiedName] -> BasicBlocksProject -> Either T.Text BasicBlocksProject
    constFold ordered bb = go (ConstFoldEnv M.empty) M.empty ordered
      where
        go _ acc [] = Right acc
        go env acc (m:ms) =
          case runConstFolding env (constFoldModule (bb M.! m)) of
            Left _ -> Left "constant folding failed"
            Right (folded, env') -> go env' (M.insert m folded acc) ms

    -- | Accumulate the architecture module by module, in dependency order.
    genArch :: TerminaProgArch SemanticAnn -> BasicBlocksProject -> [QualifiedName]
            -> Either T.Text (TerminaProgArch SemanticAnn)
    genArch tp _ [] = Right tp
    genArch tp bb (m:ms) =
      case runGenArchitecture tp m (basicBlocksAST . metadata $ bb M.! m) of
        Left _ -> Left "architecture generation failed"
        Right tp' -> genArch tp' bb ms
