module ControlFlow.Architecture.Checks.Tests (
    checkTestDeployment
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import ControlFlow.Architecture.Errors
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import Semantic.Types
import Lowering.AST
import qualified Data.Map.Strict as M
import Utils.Annotations

type TestsCheckMonad = ExceptT ArchitectureError (Reader (TerminaProgArch SemanticAnn))

-- | The rules of a deployment of a test (section 4.6 of the design of the
-- testInstances). A deployment with test instances is a unit test, and one without
-- them a concurrent test. A production deployment has neither test
-- instances nor components of a unit test, since the type checker rejects
-- them outside a test, so none of these checks fails on it.
checkTestDeployment :: TestsCheckMonad ()
checkTestDeployment = do
    tp <- ask
    if M.null (testInstances tp)
        then checkConcurrentLevel
        else checkUnitLevel
    checkUnitConnections
    checkTestConnections

-- | A unit test has no emitter other than its test emitters, the harness
-- attends system_except, and nothing reaches test_scenario, which ends a
-- concurrent test.
checkUnitLevel :: TestsCheckMonad ()
checkUnitLevel = do
    tp <- ask
    forM_ (M.elems (emitters tp)) $ \emitter ->
        let emitterId = getEmitterIdentifier emitter
            loc = maybe (getLocation (getAnnotation emitter)) (\(_, _, ann) -> getLocation ann) (M.lookup emitterId (emitterTargets tp)) in
        throwError $ annotateError loc (EEmitterInUnitTest emitterId)
    case M.findWithDefault [] "test_scenario" (resourceSources tp) of
        (_, _, connAnn) : _ ->
            throwError $ annotateError (getLocation connAnn) (ETestOfOtherLevel UnitTest "Test resource" "test_scenario")
        [] -> return ()

-- | A concurrent test has no components of a unit test. Nothing in it can reach
-- test_clock: only a test class declares an access to it (SE-237), and a
-- deployment with test instances is a unit test.
checkConcurrentLevel :: TestsCheckMonad ()
checkConcurrentLevel = do
    tp <- ask
    forM_ (M.elems (testEmitters tp)) $ \(TPTestEmitter ident _ _ ann) ->
        throwError $ annotateError (getLocation ann) (ETestOfOtherLevel ConcurrentTest "Test emitter" ident)
    forM_ (M.elems (testChannels tp)) $ \(TPMsgQueue ident _ _ _ ann) ->
        throwError $ annotateError (getLocation ann) (ETestOfOtherLevel ConcurrentTest "Test channel" ident)
    forM_ (M.elems (testPools tp)) $ \(TPPool ident _ _ _ ann) ->
        throwError $ annotateError (getLocation ann) (ETestOfOtherLevel ConcurrentTest "Test pool" ident)

-- | On the side of the unit, every test emitter is connected to a sink port,
-- every test channel to exactly one out port, and every test pool to at least
-- one allocator port. A second sink port gives AE-001 when the architecture is
-- built.
checkUnitConnections :: TestsCheckMonad ()
checkUnitConnections = do
    tp <- ask
    forM_ (M.elems (testEmitters tp)) $ \(TPTestEmitter ident _ _ ann) ->
        when (ident `M.notMember` emitterTargets tp)
            (throwError $ annotateError (getLocation ann) (EDisconnectedEmitter ident))
    forM_ (M.elems (testChannels tp)) $ \(TPMsgQueue ident _ _ _ ann) ->
        case M.findWithDefault [] ident (channelSources tp) of
            [] -> throwError $ annotateError (getLocation ann) (EChannelWithoutSources ident)
            [_] -> return ()
            -- | The sources are kept with the last connection first
            ((_, _, connAnn) : (_, _, prevAnn) : _) ->
                throwError $ annotateError (getLocation connAnn) (EDuplicatedChannelSource ident (getLocation prevAnn))
    forM_ (M.elems (testPools tp)) $ \(TPPool ident _ _ _ ann) ->
        let unitAllocators = [() | (source, port, _) <- M.findWithDefault [] ident (resourceSources tp),
                                   source `M.notMember` testInstances tp,
                                   Just (TAllocator _) <- [portType tp source port]] in
        when (null unitAllocators)
            (throwError $ annotateError (getLocation ann) (ETestPoolWithoutAllocator ident))

-- | Every component of a unit test is reached by at least one test instance,
-- a test pool through TestPoolAccess
checkTestConnections :: TestsCheckMonad ()
checkTestConnections = do
    tp <- ask
    forM_ (M.elems (testEmitters tp)) $ \(TPTestEmitter ident _ _ ann) ->
        unless (reachedByTest tp ident)
            (throwError $ annotateError (getLocation ann) (ETestComponentWithoutTest "Test emitter" ident))
    forM_ (M.elems (testChannels tp)) $ \(TPMsgQueue ident _ _ _ ann) ->
        unless (reachedByTest tp ident)
            (throwError $ annotateError (getLocation ann) (ETestComponentWithoutTest "Test channel" ident))
    forM_ (M.elems (testPools tp)) $ \(TPPool ident _ _ _ ann) ->
        let testAccesses = [() | (source, port, _) <- M.findWithDefault [] ident (resourceSources tp),
                                 source `M.member` testInstances tp,
                                 Just (TInterface _ "TestPoolAccess") <- [portType tp source port]] in
        when (null testAccesses)
            (throwError $ annotateError (getLocation ann) (ETestComponentWithoutTest "Test pool" ident))

-- | Whether a test instance is connected to the element through an access port
reachedByTest :: TerminaProgArch SemanticAnn -> Identifier -> Bool
reachedByTest tp ident =
    any (\(source, _, _) -> source `M.member` testInstances tp) (M.findWithDefault [] ident (resourceSources tp))

-- | The type of an access port of an instance
portType :: TerminaProgArch SemanticAnn -> Identifier -> Identifier -> Maybe (TerminaType SemanticAnn)
portType tp entity port = do
    cls <- case (M.lookup entity (tasks tp), M.lookup entity (handlers tp), M.lookup entity (resources tp), M.lookup entity (testInstances tp)) of
        (Just t, _, _, _) -> M.lookup (taskClass t) (taskClasses tp)
        (_, Just h, _, _) -> M.lookup (handlerClass h) (handlerClasses tp)
        (_, _, Just r, _) -> M.lookup (resourceClass r) (resourceClasses tp)
        (_, _, _, Just t) -> M.lookup (testInstanceClass t) (testClasses tp)
        _ -> Nothing
    fst <$> M.lookup port (accessPorts cls)
