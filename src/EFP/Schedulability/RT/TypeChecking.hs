{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.RT.TypeChecking where

import qualified Data.Map as M
import qualified Semantic.Types as STYPES
import qualified EFP.Schedulability.RT.Types as RTYPES
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.AST
import Control.Monad
import qualified EFP.Schedulability.TransPath.Types as TTYPES
import Utils.Monad
import EFP.Schedulability.TransPath.AST
import qualified Data.Set as S

----------------------------------------
-- Termina Programs definitions

type TPGlobalConstsEnv = M.Map Identifier Location


data RTState = RTState
    {
        progArch :: TerminaProgArch STYPES.SemanticAnn
        , transPaths :: TTYPES.TransPathMap TTYPES.SemanticAnn
        , currentSteps :: M.Map Identifier Location
        , transactions :: RTYPES.RTTransactionMap RTYPES.SemanticAnn
        , situations :: RTYPES.RTSituationMap RTYPES.SemanticAnn
    } deriving Show

type RTMonad = ExceptT RTErrors (ST.State RTState)

typeConstExpression :: (Located a) => ConstExpression a -> RTMonad (ConstExpression RTYPES.SemanticAnn)
typeConstExpression (ConstInt i ann) = 
    return $ ConstInt i (RTYPES.SemanticAnn (getLocation ann))
typeConstExpression (ConstObject ident ann) = do
    -- | We do not support constants in these models (yet), so throw an error
    throwError . annotateError (getLocation ann) $ EUnknownConstant ident
typeConstExpression (ConstBinOp op left right ann) = do
    left' <- typeConstExpression left
    right' <- typeConstExpression right
    return $ ConstBinOp op left' right' (RTYPES.SemanticAnn (getLocation ann))
typeConstExpression (ConstStructInitializer fieldAssignments ann) = do
    fieldAssignments' <- mapM typeFieldAssignment fieldAssignments
    return $ ConstStructInitializer fieldAssignments' (RTYPES.SemanticAnn (getLocation ann))
    
    where

    typeFieldAssignment (ConstFieldAssignment field identExpr) = do
        identExpr' <- typeConstExpression identExpr
        return $ ConstFieldAssignment field identExpr'


getPathContinuations :: Identifier -> M.Map Identifier (Identifier, a) -> TransactionalWCEPath b -> RTMonad [RTYPES.Continuation]
getPathContinuations componentName outputConns (TransactionalWCEPath _ _ _ _ blocks _) = 
    foldM extractContinuations [] blocks
    -- TODO

    where

    extractContinuations :: [RTYPES.Continuation] -> WCEPathBlock b -> RTMonad [RTYPES.Continuation]
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

checkStepNameUnique :: Identifier -> Location -> RTMonad ()
checkStepNameUnique stepName loc = do
    -- Check that the step name is unique in the current context
    prvSteps <- ST.gets currentSteps
    case M.lookup stepName prvSteps of
        Just prevLoc -> throwError . annotateError loc $ EDuplicatedStepName stepName prevLoc
        Nothing -> 
            -- Insert the step name in the current context
            ST.modify (\s -> s { currentSteps = M.insert stepName loc (currentSteps s) })

typeTransStep :: (Located a) => [RTYPES.Continuation]-> RTTransStep a -> RTMonad (RTTransStep RTYPES.SemanticAnn)
typeTransStep [] (RTTransStepAction stepName componentName actionId pathName nextStep ann) = do
    -- | If we are here, it means that we are typing the initial step of a transaction
    -- Which means that we do not have any valid continuations yet
    -- Check that the component exists in the architecture
    arch <- ST.gets progArch
    (tpCls, outputConns) <- case M.lookup componentName (tasks arch) of
        Just tsk -> return (taskClasses arch M.! taskClass tsk, taskOutputPortConns tsk)
        Nothing -> case M.lookup componentName (handlers arch) of
            Just hdl -> return (handlerClasses arch M.! handlerClass hdl, handlerOutputPortConns hdl)
            Nothing -> throwError . annotateError (getLocation ann) $ EUnknownComponent componentName
    -- Check that the action exists in the component class
    case M.lookup actionId (classActions tpCls) of
        Just _ -> return ()
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownAction actionId (classIdentifier tpCls, getLocation . classAnns $ tpCls)
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
        (continuations, Nothing) -> throwError . annotateError (getLocation ann) $ EActionMustContinue (classIdentifier tpCls) actionId pathName continuations
        -- | No expected continuations but next step provided: error
        ([], Just _) -> throwError . annotateError (getLocation ann) $ EActionMustNotContinue (classIdentifier tpCls) actionId pathName (getLocation . getAnnotation $ trPath)
        (validContinuations, Just next) -> Just <$> typeTransStep validContinuations next
    return $ RTTransStepAction stepName componentName actionId pathName typedNextStep (RTYPES.SemanticAnn (getLocation ann))        
typeTransStep [] (RTTransStepConditional (c:cs) ann) = 
    case c of
        (condExpr, step@(RTTransStepAction _ componentName actionId _ _ _)) -> do
            typedCondExpr <- typeConstExpression condExpr
            typedStep <- typeTransStep [] step
            typedCondTransSteps <- forM cs $ \(condExpr', step') -> do
                tyCondExpr' <- typeConstExpression condExpr'
                case step' of
                    RTTransStepAction _ componentName' actionId' _ _ _ -> do
                        unless (componentName == componentName' && actionId == actionId') $
                            throwError . annotateError (getLocation ann) $ EConditionalComponentMismatch (componentName, actionId) (componentName', actionId')
                        typedStep' <- typeTransStep [] step'
                        return (tyCondExpr', typedStep')
                    _ -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
            -- | TODO: Check that the sum of all conditions is 100 and that there are no duplicated paths
            return $ RTTransStepConditional ((typedCondExpr, typedStep) : typedCondTransSteps) (RTYPES.SemanticAnn (getLocation ann))
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
        (continuations, Nothing) -> throwError . annotateError (getLocation ann) $ EActionMustContinue (classIdentifier tpCls) targetAction pathName continuations
        ([], Just _) -> throwError . annotateError (getLocation ann) $ EActionMustNotContinue (classIdentifier tpCls) targetAction pathName (getLocation . getAnnotation $ trPath)
        (validContinuations, Just next) -> Just <$> typeTransStep validContinuations next
    return $ RTTransStepAction stepName targetTask targetAction pathName typedNextStep (RTYPES.SemanticAnn (getLocation ann))        
typeTransStep [cont] (RTTransStepConditional condTransSteps ann) = do
    typedCondTransSteps <- forM condTransSteps $ \(condExpr, step) -> do
        tyCondExpr <- typeConstExpression condExpr
        case step of
            RTTransStepAction {} -> do
                typedStep <- typeTransStep [cont] step
                return (tyCondExpr, typedStep)
            RTTransStepConditional {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
            RTTransStepMuticast {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
    -- TODO: Check that the sum of all conditions is 100 and that there are no duplicated paths
    return $ RTTransStepConditional typedCondTransSteps (RTYPES.SemanticAnn (getLocation ann))
typeTransStep validContinuations (RTTransStepMuticast transSteps ann) = do
    (_, _, typedTransSteps) <- foldM typeMultiCastStep (S.fromList validContinuations, M.empty, []) transSteps
    return $ RTTransStepMuticast typedTransSteps (RTYPES.SemanticAnn (getLocation ann))

    where

        typeMultiCastStep :: (Located a) => 
            (S.Set RTYPES.Continuation, M.Map RTYPES.Continuation Location, [RTTransStep RTYPES.SemanticAnn])
            -> RTTransStep a
            -> RTMonad (S.Set RTYPES.Continuation, M.Map RTYPES.Continuation Location, [RTTransStep RTYPES.SemanticAnn])
        typeMultiCastStep (contSet, visitedSteps, typedTransSteps) step =
            case step of
                -- | All multicast steps must be action steps
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
                -- | If the step is not an action, throw an error
                RTTransStepMuticast {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation
                RTTransStepConditional {} -> throwError . annotateError (getLocation ann) $ EExpectedStepActionContinuation

typeTransStep validContinuations (RTTransStepAction _ _ _ _ _ ann) = throwError . annotateError (getLocation ann) $ EExpectedMulticastContinuation validContinuations
typeTransStep validContinuations (RTTransStepConditional _ ann) = throwError . annotateError (getLocation ann) $ EExpectedMulticastContinuation validContinuations

typeRTElement :: (Located a) => RTElement a -> RTMonad (RTElement RTYPES.SemanticAnn)
typeRTElement (RTTransaction transId firstStep ann) = do
    -- Check that the transaction identifier is unique
    prvTransactions <- ST.gets transactions
    prvSituations <- ST.gets situations
    case M.lookup transId prvTransactions of
        Just prev@(RTTransaction {}) -> throwError . annotateError (getLocation ann) $ EPreviousTransactionWithSameName transId (getLocation . getAnnotation $ prev)
        Just (RTSituation {}) -> throwError . annotateError Internal $ EInvalidTransactionMap
        Nothing -> return ()
    case M.lookup transId prvSituations of
        Just _ -> throwError . annotateError (getLocation ann) $ EPreviousSituationWithSameName transId (getLocation ann)
        Nothing -> return ()
    tyFirstStep <- localScope $ typeTransStep [] firstStep
    return $ RTTransaction transId tyFirstStep (RTYPES.SemanticAnn (getLocation ann))
typeRTElement (RTSituation _sitId (ConstStructInitializer _fields _ann) _loc) = undefined
typeRTElement (RTSituation _sitId _ _loc) = undefined

typeRTElements :: (Located a) => [RTElement a] -> RTMonad ()
typeRTElements = 
    mapM_ (typeRTElement >=> insertElement)

    where 

    insertElement :: RTElement RTYPES.SemanticAnn -> RTMonad ()
    insertElement trans@(RTTransaction transId _ _) =
        ST.modify $ \s -> s { 
            transactions = M.insert transId trans (transactions s) }
    insertElement sit@(RTSituation sitId (ConstStructInitializer _ _) _) =
        ST.modify $ \s -> s {
            situations = M.insert sitId sit (situations s) }
    insertElement _ = throwError . annotateError Internal $ EInvalidRTElementDefinition
    
runRTTypeChecking :: (Located a) => TerminaProgArch STYPES.SemanticAnn
    -> TTYPES.TransPathMap TTYPES.SemanticAnn
    -> [RTElement a]
    -> Either RTErrors (RTYPES.RTTransactionMap RTYPES.SemanticAnn, RTYPES.RTSituationMap RTYPES.SemanticAnn)
runRTTypeChecking arch transPath elements =
    let initialState = RTState arch transPath M.empty M.empty M.empty in
    case ST.runState (runExceptT (typeRTElements elements)) initialState of
        (Left err, _) -> Left err
        (_, st) -> Right (transactions st, situations st)