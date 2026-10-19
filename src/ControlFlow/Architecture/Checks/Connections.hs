module ControlFlow.Architecture.Checks.Connections where

import Control.Monad
import Control.Monad.Except
import ControlFlow.Architecture.Errors
import Control.Monad.Reader
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import Semantic.Types
import Lowering.AST
import qualified Data.Map.Strict as M
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

-- | Every deque is connected to exactly one access port, so only the members of
-- one element touch it, and gets boxes from somewhere other than itself.
checkDequeConnections :: ConnectionsCheckMonad ()
checkDequeConnections = do
    tp <- ask
    mapM_ (\(TPDeque dequeId _ _ _ ann) ->
        case M.lookup dequeId (resourceSources tp) of
            Nothing -> throwError $ annotateError (getLocation ann) (EUnusedDeque dequeId)
            Just [(owner, port, _)] ->
                case ownerBoxIOMaps tp owner of
                    Just ioMaps | null (dequeSources ioMaps port) ->
                        throwError $ annotateError (getLocation ann) (EUnfilledDeque dequeId owner port)
                    _ -> return ()
            Just (conn : prev : _) ->
                let (_, _, connAnn) = conn
                    (_, _, prevAnn) = prev in
                throwError $ annotateError (getLocation connAnn) (ESharedDeque dequeId (getLocation prevAnn))
            Just [] -> return ()) . M.elems . deques $ tp

    where

        -- | The box maps of the class of a task, a handler or a resource
        ownerBoxIOMaps tp owner = classBoxIOMaps <$>
            case (M.lookup owner (tasks tp), M.lookup owner (handlers tp), M.lookup owner (resources tp)) of
                (Just task, _, _) -> M.lookup (taskClass task) (taskClasses tp)
                (_, Just handler, _) -> M.lookup (handlerClass handler) (handlerClasses tp)
                (_, _, Just resource) -> M.lookup (resourceClass resource) (resourceClasses tp)
                _ -> Nothing

checkTaskPriorities :: ConnectionsCheckMonad ()
checkTaskPriorities = do
    tp <- ask
    foldM_ (\seen tsk -> do
        let TInteger priority _ = getPriority tsk
            loc = getLocation (taskAnns tsk)
        case M.lookup priority seen of
            Just (prevTask, prevLoc) ->
                throwError $ annotateError loc (EDuplicatedTaskPriority (taskName tsk) priority prevTask prevLoc)
            Nothing -> return $ M.insert priority (taskName tsk, loc) seen) M.empty . M.elems . tasks $ tp

-- | The action of the handler that attends system_except neither sends a
-- message, nor reboots, nor calls a noreturn function: the runtime reboots once
-- the action has run.
checkSystemExceptAction :: ConnectionsCheckMonad ()
checkSystemExceptAction = do
    tp <- ask
    case attendingAction tp of
        Nothing -> return ()
        Just (cls, TPFunction action _ _ (Block body _) _) ->
            case forbiddenIn body of
                [] -> return ()
                (err : _) -> throwError (err cls action)

    where

        attendingAction tp = do
            (target, port, _) <- M.lookup "system_except" (emitterTargets tp)
            handler <- M.lookup target (handlers tp)
            cls <- M.lookup (handlerClass handler) (handlerClasses tp)
            (_, action) <- M.lookup port (sinkPorts cls)
            fn <- M.lookup action (classActions cls)
            return (classIdentifier cls, fn)

        forbiddenIn = concatMap forbidden

        forbidden (SendMessage _ _ ann) = [\cls action -> annotateError (getLocation ann) (ESendInSystemExceptAction cls action)]
        forbidden (RebootBlock ann) = [\cls action -> annotateError (getLocation ann) (EExitInSystemExceptAction cls action)]
        forbidden (NoReturnCall _ ann) = [\cls action -> annotateError (getLocation ann) (EExitInSystemExceptAction cls action)]
        forbidden (IfElseBlock (CondIf _ ifBlk _) elseIfs mElse _) =
            forbiddenIn (blockBody ifBlk)
            ++ concat [forbiddenIn (blockBody blk) | CondElseIf _ blk _ <- elseIfs]
            ++ maybe [] (forbiddenIn . blockBody . condElseBody) mElse
        forbidden (MatchBlock _ cases mDefault _) =
            concat [forbiddenIn (blockBody blk) | MatchCase _ _ blk _ <- cases]
            ++ maybe [] (\(DefaultCase blk _) -> forbiddenIn (blockBody blk)) mDefault
        forbidden (ForLoopBlock _ _ _ _ _ blk _) = forbiddenIn (blockBody blk)
        forbidden _ = []

checkResourceUsage :: ConnectionsCheckMonad ()
checkResourceUsage = do
    tp <- ask
    mapM_ (\(TPResource resId _ _ _ _ ann) ->
        when (resId `M.notMember` resourceSources tp) (throwError $ annotateError (getLocation ann) (EUnusedResource resId))) . M.elems . resources $ tp
