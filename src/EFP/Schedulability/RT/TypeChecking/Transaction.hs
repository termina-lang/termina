module EFP.Schedulability.RT.TypeChecking.Transaction where

import qualified Data.Map.Strict as M

import EFP.Schedulability.RT.Semantic.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.Parser.AST
import qualified EFP.Schedulability.RT.Semantic.AST as SAST
import Control.Monad
import EFP.Schedulability.WCEPath.AST
import qualified Data.Set as S
import EFP.Schedulability.Core.Types
import EFP.Schedulability.RT.Monad
import EFP.Schedulability.RT.TypeChecking.ConstExpression
import EFP.Schedulability.RT.Utils


getPathContinuations :: Identifier -> M.Map Identifier (Identifier, a) -> WCEPath b -> RTMonad [Continuation]
getPathContinuations componentName outputConns (WCEPath _ _ _ _ blocks _) =
    foldM extractContinuations [] blocks

    where

    extractContinuations :: [Continuation] -> WCEPathBlock b -> RTMonad [Continuation]
    extractContinuations acc (WCEPSendMessage portName _ _) =
        case M.lookup portName outputConns of
            Nothing -> throwError . annotateError Internal $ EUnknownOutputPort portName
            Just (channelId, _) -> do
                arch <- ST.gets progArch
                (targetTask, targetPort) <-
                    case M.lookup channelId (channelTargets arch) of
                        Nothing -> throwError . annotateError Internal $ EChannelNotConnected channelId
                        Just (targetTask, targetPort, _) -> return (targetTask, targetPort)
                taskClassId <- case M.lookup targetTask (tasks arch) of
                    Nothing -> throwError . annotateError Internal $ EInvalidTask targetTask
                    Just tsk -> return (taskClass tsk)
                tpCls <- case M.lookup taskClassId (taskClasses arch) of
                    Nothing -> throwError . annotateError Internal $ EInvalidTaskClass taskClassId
                    Just cls -> return cls
                targetAction <- case M.lookup targetPort (inputPorts tpCls) of
                    Nothing -> throwError . annotateError Internal $ EInvalidTargetPort targetPort
                    Just (_, act) -> return act
                return $ (targetTask, targetAction) : acc
    extractContinuations acc (WCEPathCondIf ifBlks _ _) =
        foldM extractContinuations acc ifBlks
    extractContinuations acc (WCEPathCondElseIf elifBlks _ _) =
        foldM extractContinuations acc elifBlks
    extractContinuations acc (WCEPathCondElse elseBlks _ _) =
        foldM extractContinuations acc elseBlks
    extractContinuations acc (WCEPathMatchCase caseBlks _ _) =
        foldM extractContinuations acc caseBlks
    extractContinuations acc (WCEPContinue actionId _ _) =
        return $ (componentName, actionId) : acc
    extractContinuations acc _ = return acc

checkConditionalExprRange :: ConstExpression RTSemAnn -> RTMonad ()
checkConditionalExprRange expr = do
    case expr of
        ConstInt (TInteger v _) _ ->
            unless (v > 0 && v <= 100) $
                throwError . annotateError (getLocation . getAnnotation $ expr) $ EConditionalExpressionOutOfRange v
        _ -> throwError . annotateError Internal $ EInvalidConditionalExpressionType

checkStepNameUnique :: Identifier -> Location -> RTMonad ()
checkStepNameUnique stepName loc = do
    -- Check that the step name is unique in the current context
    prvSteps <- ST.gets currentSteps
    case M.lookup stepName prvSteps of
        Just prevLoc -> throwError . annotateError loc $ EDuplicatedStepName stepName prevLoc
        Nothing ->
            -- Insert the step name in the current context
            ST.modify (\s -> s { currentSteps = M.insert stepName loc (currentSteps s) })

typeCondExpression :: ConstExpression RTSemAnn -> RTMonad TInteger
typeCondExpression (ConstInt i@(TInteger intVal _) ann) = do
    unless (intVal > 0 && intVal <= 100) $
        throwError . annotateError (getLocation ann) $ EConditionalExpressionOutOfRange intVal
    return i
typeCondExpression expr = throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidConditionalExpression

typeTransStep :: [Continuation]-> RTTransStep ParserAnn -> RTMonad (SAST.RTTransStep RTSemAnn)
typeTransStep [] (RTTransStepAction stepName componentName actionId pathName nextStep ann) = do
    -- Check that the step name is unique in the current context
    checkStepNameUnique stepName (getLocation ann)
    -- | If we are here, it means that we are typing the initial step of a transaction
    -- Which means that we do not have any valid continuations yet.
    -- First, we must check that the component exists in the architecture. If the
    -- component exists, we will get its class and output connections
    arch <- ST.gets progArch
    (tpCls, outputConns) <- case M.lookup componentName (tasks arch) of
        Just tsk -> return (taskClasses arch M.! taskClass tsk, taskOutputPortConns tsk)
        Nothing -> case M.lookup componentName (handlers arch) of
            Just hdl -> return (handlerClasses arch M.! handlerClass hdl, handlerOutputPortConns hdl)
            Nothing -> throwError . annotateError (getLocation ann) $ EUnknownComponent componentName
    -- Check that the action exists in the component class
    unless (M.member actionId (classActions tpCls)) $
        throwError . annotateError (getLocation ann) $ EUnknownAction actionId (classIdentifier tpCls, getLocation . classAnns $ tpCls)
    -- Check that the transactional path exists
    trPathMap <- ST.gets transPaths
    trPath <- case M.lookup (classIdentifier tpCls, actionId) trPathMap of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransPath (classIdentifier tpCls) actionId pathName
        Just paths -> do
            case M.lookup pathName paths of
                Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransPath (classIdentifier tpCls) actionId pathName
                Just p -> return p
    -- | Get all possible continuations from the transactional path
    continuations <-
            getPathContinuations componentName outputConns trPath
    typedNextStep <- case (continuations, nextStep) of
        -- | No expected continuations and no next step: ok
        ([], Nothing) -> return Nothing
        -- | Expected continuations but no next step: error
        (cs, Nothing) -> throwError . annotateError (getLocation ann) $ EActionMustContinue (classIdentifier tpCls) actionId pathName cs
        -- | No expected continuations but next step provided: error
        ([], Just _) -> throwError . annotateError (getLocation ann) $ EActionMustNotContinue (classIdentifier tpCls) actionId pathName (getLocation . getAnnotation $ trPath)
        (validContinuations, Just next) -> Just <$> typeTransStep validContinuations next
    return $ SAST.RTTransStepAction stepName componentName actionId pathName typedNextStep (RTStepTy (getLocation ann))
typeTransStep [] (RTTransStepConditional (c:cs) ann) =
    case c of
        (condExpr, step@(RTTransStepAction _ componentName actionId _ _ _)) -> do
            typedCondExpr <- typeConstExpression condExpr >>= evalConstExpression >>= typeCondExpression
            typedStep <- typeTransStep [] step
            typedCondTransSteps <- forM cs $ \(condExpr', step') -> do
                tyCondExpr' <- typeConstExpression condExpr' >>= evalConstExpression >>= typeCondExpression
                case step' of
                    RTTransStepAction _ componentName' actionId' _ _ _ -> do
                        unless (componentName == componentName' && actionId == actionId') $
                            throwError . annotateError (getLocation ann) $ EConditionalComponentMismatch (componentName, actionId) (componentName', actionId')
                        typedStep' <- typeTransStep [] step'
                        return (tyCondExpr', typedStep')
                    _ -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
            -- | TODO: Check that the sum of all conditions is 100 and that they are all non-negative integer expressions
            return $ SAST.RTTransStepConditional ((typedCondExpr, typedStep) : typedCondTransSteps) (RTStepTy (getLocation ann))
        _ -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
typeTransStep [] (RTTransStepMuticast _ ann) = throwError . annotateError (getLocation ann) $ EInvalidInitialStepMulticast
typeTransStep [cont] (RTTransStepMuticast _ ann) = throwError . annotateError (getLocation ann) $ EInvalidMulticastSingleContinuation cont
typeTransStep [(taskId, actionId)] (RTTransStepAction stepName targetTask targetAction pathName nextStep ann) = do
    -- Check that the step name is unique in the current context
    checkStepNameUnique stepName (getLocation ann)
    -- Check that the task exists in the architecture
    arch <- ST.gets progArch
    (tpCls, outputConns) <- case M.lookup targetTask (tasks arch) of
        Just tsk -> return (taskClasses arch M.! taskClass tsk, taskOutputPortConns tsk)
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTask targetTask
    -- Check that the action exists in the task class
    case M.lookup targetAction (classActions tpCls) of
        Just _ -> return ()
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownAction targetAction (classIdentifier tpCls, getLocation . classAnns $ tpCls)
    -- Check that the continuation matches the expected one
    unless (taskId == targetTask) $
        throwError . annotateError (getLocation ann) $ EInvalidContinuationTaskMismatch targetTask (taskId, actionId)
    unless (actionId == targetAction) $
        throwError . annotateError (getLocation ann) $ EInvalidContinuationActionMismatch targetAction (taskId, actionId)
    -- Check that the transactional path exists
    trPathMap <- ST.gets transPaths
    trPath <- case M.lookup (classIdentifier tpCls, targetAction) trPathMap of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransPath (classIdentifier tpCls) targetAction pathName
        Just paths -> case M.lookup pathName paths of
                Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransPath (classIdentifier tpCls) targetAction pathName
                Just p -> return p
    -- | Get all possible continuations from the transactional path
    continuations <- getPathContinuations targetTask outputConns trPath
    typedNextStep <- case (continuations, nextStep) of
        ([], Nothing) -> return Nothing
        (cs, Nothing) -> throwError . annotateError (getLocation ann) $ EActionMustContinue (classIdentifier tpCls) targetAction pathName cs
        ([], Just _) -> throwError . annotateError (getLocation ann) $ EActionMustNotContinue (classIdentifier tpCls) targetAction pathName (getLocation . getAnnotation $ trPath)
        (validContinuations, Just next) -> Just <$> typeTransStep validContinuations next
    return $ SAST.RTTransStepAction stepName targetTask targetAction pathName typedNextStep (RTStepTy (getLocation ann))
typeTransStep [cont] (RTTransStepConditional condTransSteps ann) = do
    -- | Check that there are at least two branches
    when (length condTransSteps < 2) $
        throwError . annotateError (getLocation ann) $ EConditionalStepsMustHaveMultipleBranches
    typedCondTransSteps <- forM condTransSteps $ \(condExpr, step) -> do
        tyCondExpr <- typeConstExpression condExpr >>= evalConstExpression >>= typeCondExpression
        case step of
            RTTransStepAction {} -> do
                typedStep <- typeTransStep [cont] step
                return (tyCondExpr, typedStep)
            RTTransStepConditional {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
            RTTransStepMuticast {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
    -- TODO: Check that the sum of all conditions is 100
    return $ SAST.RTTransStepConditional typedCondTransSteps (RTStepTy (getLocation ann))
typeTransStep validContinuations (RTTransStepMuticast transSteps ann) = do
    (_, _, typedTransSteps) <- foldM typeMultiCastStep (S.fromList validContinuations, M.empty, []) transSteps
    return $ SAST.RTTransStepMuticast typedTransSteps (RTStepTy (getLocation ann))

    where

        typeMultiCastStep ::
            (S.Set Continuation, M.Map Continuation Location, [SAST.RTTransStep RTSemAnn])
            -> RTTransStep ParserAnn
            -> RTMonad (S.Set Continuation, M.Map Continuation Location, [SAST.RTTransStep RTSemAnn])
        typeMultiCastStep (contSet, visitedSteps, typedTransSteps) step =
            case step of
                RTTransStepAction _ targetTask targetAction _ _ stepAnn ->
                    -- | Check that the continuation has not been used before
                    case M.lookup (targetTask, targetAction) visitedSteps of
                        -- | If it has been used before, throw an error
                        Just prevLoc -> throwError . annotateError (getLocation stepAnn) $ EDuplicatedMulticastContinuation (targetTask, targetAction) prevLoc
                        Nothing ->
                            -- | Check that the continuation is valid
                            if S.notMember (targetTask, targetAction) contSet then
                                throwError . annotateError (getLocation stepAnn) $ EInvalidMulticastContinuation (targetTask, targetAction) validContinuations
                            else do
                                -- | Type the step
                                typedTransStep <- typeTransStep [(targetTask, targetAction)] step
                                return (S.delete (targetTask, targetAction) contSet,
                                        M.insert (targetTask, targetAction) (getLocation stepAnn) visitedSteps,
                                        typedTransStep : typedTransSteps)
                RTTransStepConditional ((_, RTTransStepAction _ targetTask targetAction _ _ _):_cs) stepAnn -> 
                    -- | Check that the continuation has not been used before
                    case M.lookup (targetTask, targetAction) visitedSteps of
                        -- | If it has been used before, throw an error
                        Just prevLoc -> throwError . annotateError (getLocation stepAnn) $ EDuplicatedMulticastContinuation (targetTask, targetAction) prevLoc
                        Nothing ->
                            -- | Check that the continuation is valid
                            if S.notMember (targetTask, targetAction) contSet then
                                throwError . annotateError (getLocation stepAnn) $ EInvalidMulticastContinuation (targetTask, targetAction) validContinuations
                            else do
                                -- | Type the step
                                typedTransStep <- typeTransStep [(targetTask, targetAction)] step
                                return (S.delete (targetTask, targetAction) contSet,
                                        M.insert (targetTask, targetAction) (getLocation stepAnn) visitedSteps,
                                        typedTransStep : typedTransSteps)
                RTTransStepConditional {} ->
                    throwError . annotateError (getLocation ann) $ EConditionalStepsMustHaveMultipleBranches
                -- | If the step is a multicast, throw an error
                RTTransStepMuticast {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation

typeTransStep validContinuations (RTTransStepAction _ _ _ _ _ ann) = throwError . annotateError (getLocation ann) $ EExpectedMulticastContinuation validContinuations
typeTransStep validContinuations (RTTransStepConditional _ ann) = throwError . annotateError (getLocation ann) $ EExpectedMulticastContinuation validContinuations
