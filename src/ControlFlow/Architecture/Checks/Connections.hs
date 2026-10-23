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

-- | A channel connects exactly one out port to exactly one in port. A second
-- in port gives AE-002 when the architecture is built.
checkChannelConnections :: ConnectionsCheckMonad ()
checkChannelConnections = do
    tp <- ask
    mapM_ (
        \case TPMsgQueue channelId _ _ _ ann -> do
                case M.findWithDefault [] channelId (channelSources tp) of
                    [] -> throwError $ annotateError (getLocation ann) (EChannelWithoutSources channelId)
                    [_] -> return ()
                    -- | The sources are kept with the last connection first
                    ((_, _, connAnn) : (_, _, prevAnn) : _) ->
                        throwError $ annotateError (getLocation connAnn) (EDuplicatedChannelSource channelId (getLocation prevAnn))
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

-- | The response to an event ends. From the action that each emitter
-- triggers, the check follows every message that an action sends, to the
-- action that the in port of the receiving task triggers, and every continue,
-- to the next action of the same task, and fails when a chain reaches an
-- action that is already on it. A send or a continue in a branch that never
-- runs counts as well, since the check reads the code and does not run it.
-- In a unit test, a message that a case sends starts a chain too, and the
-- methods of the test class count as part of each of its cases.
checkActivationCycles :: ConnectionsCheckMonad ()
checkActivationCycles = do
    tp <- ask
    forM_ (M.toList (emitterTargets tp)) $ \(emitterId, (target, port, ann)) ->
        forM_ (sinkAction tp target port) $ \action ->
            walk tp emitterId [] ((target, action), getLocation ann)
    forM_ (M.elems (testInstances tp)) $ \test ->
        forM_ (M.lookup (testInstanceClass test) (testClasses tp)) $ \cls ->
            forM_ (M.elems (classCases cls)) $ \(TPFunction caseId _ _ (Block body _) _) ->
                let methodBodies = [blockBody blk | TPFunction _ _ _ blk _ <- M.elems (classMethods cls)] in
                forM_ (concatMap (follow tp (testInstanceName test)) (blocksIn (body ++ concat methodBodies))) $
                    walk tp (testInstanceName test ++ "." ++ caseId) []

    where

        -- | The chain so far, from the event on, and the next action with the
        -- location of what sets it off, which closes the cycle when the action
        -- is already on the chain
        walk :: TerminaProgArch SemanticAnn -> Identifier -> [(Identifier, Identifier)]
            -> ((Identifier, Identifier), Location) -> ConnectionsCheckMonad ()
        walk tp emitterId chain (step@(entity, action), loc) =
            case dropWhile (/= step) chain of
                [] -> forM_ (nextSteps tp entity action) $ walk tp emitterId (chain ++ [step])
                cycleSteps -> throwError $ annotateError loc (EActivationCycle emitterId (cycleSteps ++ [step]))

        sinkAction tp entity port = do
            cls <- entityClass tp entity
            snd <$> M.lookup port (sinkPorts cls)

        -- | The actions that run next after an action, each with the location
        -- of the send or the continue that sets it off
        nextSteps tp entity action =
            case entityClass tp entity >>= M.lookup action . classActions of
                Nothing -> []
                Just (TPFunction _ _ _ (Block body _) _) ->
                    concatMap (follow tp entity) (blocksIn body)

        follow tp entity (SendMessage obj _ ann) =
            [ ((receiver, nextAction), getLocation ann)
            | Right port <- [getPortName obj]
            , Just (channel, _) <- [M.lookup port (outputConnections tp entity)]
            , Just (receiver, inPort, _) <- [M.lookup channel (channelTargets tp)]
            , Just cls <- [entityClass tp receiver]
            , Just (_, nextAction) <- [M.lookup inPort (inputPorts cls)] ]
        follow _ entity (ContinueBlock (MemberFunctionCall _ nextAction _ _) ann) = [((entity, nextAction), getLocation ann)]
        follow _ entity (ContinueBlock (DerefMemberFunctionCall _ nextAction _ _) ann) = [((entity, nextAction), getLocation ann)]
        follow _ _ _ = []

        entityClass tp entity =
            case (M.lookup entity (tasks tp), M.lookup entity (handlers tp)) of
                (Just task, _) -> M.lookup (taskClass task) (taskClasses tp)
                (_, Just handler) -> M.lookup (handlerClass handler) (handlerClasses tp)
                _ -> Nothing

        outputConnections tp entity =
            case (M.lookup entity (tasks tp), M.lookup entity (handlers tp), M.lookup entity (testInstances tp)) of
                (Just task, _, _) -> taskOutputPortConns task
                (_, Just handler, _) -> handlerOutputPortConns handler
                (_, _, Just test) -> testInstanceOutputPortConns test
                _ -> M.empty

checkResourceUsage :: ConnectionsCheckMonad ()
checkResourceUsage = do
    tp <- ask
    mapM_ (\(TPResource resId _ _ _ _ ann) ->
        when (resId `M.notMember` resourceSources tp) (throwError $ annotateError (getLocation ann) (EUnusedResource resId))) . M.elems . resources $ tp
