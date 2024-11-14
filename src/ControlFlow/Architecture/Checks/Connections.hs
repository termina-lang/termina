module ControlFlow.Architecture.Checks.Connections where

import Control.Monad.Except
import ControlFlow.Architecture.Errors.Errors
import Control.Monad.Reader
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import Semantic.Types
import qualified Data.Map as M
import Utils.Annotations

type ConnectionsCheckMonad = ExceptT ArchitectureError (Reader (TerminaProgArch SemanticAnn))

checkEmitterConnections :: ConnectionsCheckMonad ()
checkEmitterConnections = do
    tp <- ask
    mapM_ (\e ->
        let emitterId = getEmitterIdentifier e in
        when (emitterId `M.notMember` emitterTargets tp) (throwError $ annotateError (getLocation . getAnnotation $ e) (EDisconnectedEmitter emitterId))) . M.elems . emitters $ tp

checkChannelConnections :: ConnectionsCheckMonad ()
checkChannelConnections = do
    tp <- ask
    mapM_ (
        \case TPMsgQueue channelId _ _ _ ann -> do
                when (channelId `M.notMember` channelSources tp) (throwError $ annotateError (getLocation ann) (EChannelWithoutSources channelId))
                when (channelId `M.notMember` channelTargets tp) (throwError $ annotateError (getLocation ann) (EChannelWithoutTarget channelId))) . M.elems . channels $ tp

checkPoolUsage :: ConnectionsCheckMonad ()
checkPoolUsage = do
    tp <- ask
    mapM_ (\(TPPool poolId _ _ _ ann) ->
        when (poolId `M.notMember` resourceSources tp) (throwError $ annotateError (getLocation ann) (EUnusedPool poolId))) . M.elems . pools $ tp

checkResourceUsage :: ConnectionsCheckMonad ()
checkResourceUsage = do
    tp <- ask
    mapM_ (\(TPResource resId _ _ _ ann) ->
        when (resId `M.notMember` resourceSources tp) (throwError $ annotateError (getLocation ann) (EUnusedResource resId))) . M.elems . resources $ tp
