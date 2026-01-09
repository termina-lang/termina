module EFP.Schedulability.MAST.Generator where

import EFP.Schedulability.MAST.AST
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.MAST.Monad
import qualified Data.Set as S
import Control.Monad
import ControlFlow.Architecture.Types
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Utils.Annotations
import EFP.Schedulability.MAST.Errors
import EFP.Schedulability.MAST.Utils
import EFP.Schedulability.MAST.Platform
import EFP.Schedulability.RT.Semantic.Types
import EFP.Schedulability.RT.Semantic.AST
import Configuration.Configuration
import Semantic.Types
import ControlFlow.Architecture.Utils
import qualified Control.Monad.State as ST


data SelectedEvent a =
    SelectedEventBursty 
        Identifier -- ^ Event identifier
        Identifier -- ^ Emitter identifier
        Identifier -- ^ Transaction identifier
        Identifier -- ^ Initial step identifier
        (TRPStepMap a) -- ^ Steps map
        TInteger -- ^ Interval
        TInteger -- ^ Arrivals expression
        (RTDeadlineMap a) -- ^ Deadlines map
    | SelectedEventPeriodic
        Identifier -- ^ Event identifier
        Identifier -- ^ Emitter identifier
        Identifier -- ^ Transaction identifier
        Identifier -- ^ Initial step identifier
        (TRPStepMap a) -- ^ Steps map
        (RTDeadlineMap a) -- ^ Deadlines map
    deriving Show


getEnclosingOperations :: S.Set Identifier -> TransPathBlock a -> MASTGenMonad (S.Set Identifier)
getEnclosingOperations acc (TPBlockCondIf blks _ _) = foldM getEnclosingOperations acc blks
getEnclosingOperations acc (TPBlockCondElseIf blks _ _) = foldM getEnclosingOperations acc blks
getEnclosingOperations acc (TPBlockCondElse blks _ _) = foldM getEnclosingOperations acc blks
getEnclosingOperations acc (TPBlockForLoop _ blks _ _) = foldM getEnclosingOperations acc blks
getEnclosingOperations acc (TPBlockMatchCase blks _ _) = foldM getEnclosingOperations acc blks
getEnclosingOperations acc (TPBlockMemberFunctionCall args operation _ _) =
    genMASTOperation False args operation >>= \opId ->
        return $ S.insert opId acc
getEnclosingOperations acc (TPBlockProcedureInvoke args operation _ _) = 
    genMASTOperation True args operation >>= \opId ->
        return $ S.insert opId acc
getEnclosingOperations acc (TPBlockAllocBox poolId _ _) = do 
    let opId = getAllocBoxMASTOperationId poolId
    ops <- gets operations
    case M.lookup opId ops of
        Just _ -> return $ S.insert opId acc
        Nothing -> do
            rlockingMap <- gets resourceLockingMap
            case M.lookup poolId rlockingMap of
                Just ResourceLockNone ->
                    genUnprotectedPool poolId
                    >> return (S.insert opId acc)
                Just ResourceLockIrq -> do
                    genIrqLockSharedPool poolId
                    >> return (S.insert opId acc)
                Just (ResourceLockMutex (TInteger ceil _)) -> do
                    genMutexLockSharedPool poolId ceil
                    >> return (S.insert opId acc)
                Nothing -> throwError . annotateError Internal $ EUnknownResource poolId

getEnclosingOperations acc (TPBlockFreeBox poolId _ _) = do
    let opId = getFreeBoxMASTOperationId poolId
    ops <- gets operations
    case M.lookup opId ops of
        Just _ -> return $ S.insert opId acc
        Nothing -> do
            rlockingMap <- gets resourceLockingMap
            case M.lookup poolId rlockingMap of
                Just ResourceLockNone ->
                    genUnprotectedPool poolId
                    >> return (S.insert opId acc)
                Just ResourceLockIrq -> do
                    genIrqLockSharedPool poolId
                    >> return (S.insert opId acc)
                Just (ResourceLockMutex (TInteger ceil _)) -> do
                    genMutexLockSharedPool poolId ceil
                    >> return (S.insert opId acc)
                Nothing -> throwError . annotateError Internal $ EUnknownResource poolId
getEnclosingOperations acc (TPBlockSystemCall systemCallName _ _ _) = do
    let opId = getSystemCallMASTOperationId systemCallName
    ops <- gets operations
    case M.lookup opId ops of
        Just _ -> return $ S.insert opId acc
        Nothing -> throwError . annotateError Internal $ EUnsupportedSystemCall systemCallName
getEnclosingOperations acc _ = return acc

genBodyOperation :: Identifier -> [Identifier] -> WCETime -> MASTGenMonad ()
genBodyOperation bodyOpId [] wcet = do
    let operation = MASTSimpleOperation
            bodyOpId
            wcet [] []
    insertOperation operation
genBodyOperation bodyOpId enclosingOps wcet = do
    let operation = MASTEnclosingOperation
            bodyOpId
            wcet
            enclosingOps
    insertOperation operation

genMASTOperation :: Bool -> [ConstExpression a] -> TRPOperation a -> MASTGenMonad Identifier
genMASTOperation _isInvoke _args (TRPTaskOperation _stepName taskId actionId pathId blks _ wcet _) = do
    -- | The operation identifier is the name of the step defined in the transaction
    let opId = getTaskMASTOperationId taskId actionId pathId
    ops <- gets operations
    case M.lookup opId ops of
        Just _ -> return opId
        Nothing -> do
            enclosingOps <- foldM getEnclosingOperations S.empty blks
            genBodyOperation opId (S.toList enclosingOps) wcet
            >> return opId
genMASTOperation _isInvoke _args (TRPHandlerOperation _stepName handlerId actionId pathId blks _ wcet _) = do
    let opId = getHandlerMASTOperationId handlerId actionId pathId
    ops <- gets operations
    case M.lookup opId ops of
        Just _ -> return opId
        Nothing -> do
            enclosingOps <- foldM getEnclosingOperations S.empty blks
            genBodyOperation opId (S.toList enclosingOps) wcet
            >> return opId
genMASTOperation isInvoke args (TRPResourceOperation resName functionName pathName blks wcet _) = do
    -- | First, we need to check if the resource operation has already been generated
    opId <- getResourceMASTOperationId resName functionName pathName args
    ops <- gets operations
    case M.lookup opId ops of
        Just _ -> return opId
        Nothing -> do
            -- | Generate the MAST operation
            enclosingOps <- foldM getEnclosingOperations S.empty blks
            -- | If the operation is an invoke, we need to check the resource locking mechanism
            if isInvoke then do
                rlockingMap <- gets resourceLockingMap
                case M.lookup resName rlockingMap of
                    Just ResourceLockNone ->
                        genBodyOperation opId (S.toList enclosingOps) wcet
                        >> return opId
                    Just ResourceLockIrq -> do
                        genBodyOperation (bodyMASTOperationId opId) (S.toList enclosingOps) wcet
                        shRes <- gets sharedResources
                        unless (M.member resName shRes) $
                            genIrqLockSharedResource resName
                        insertOperation (MASTCompositeOperation opId [
                                getIrqLockMASTOperationId resName,
                                bodyMASTOperationId opId,
                                getIrqUnlockMASTOperationId resName
                            ]) >> return opId
                    Just (ResourceLockMutex (TInteger ceil _)) -> do
                        genBodyOperation (bodyMASTOperationId opId) (S.toList enclosingOps) wcet
                        shRes <- gets sharedResources
                        unless (M.member resName shRes) $ genMutexLockSharedResource resName ceil
                        insertOperation (MASTCompositeOperation opId [
                                getMutexLockMASTOperationId resName,
                                bodyMASTOperationId opId,
                                getMutexUnlockMASTOperationId resName
                            ]) >> return opId
                    Nothing -> throwError . annotateError Internal $ EUnknownResource resName
            else 
                genBodyOperation opId (S.toList enclosingOps) wcet
                >> return opId

genMASTTransactionStep :: MASTTransaction -> Identifier -> MASTGenMonad MASTTransaction
genMASTTransactionStep acc stepName = undefined

genMASTTransaction :: SelectedEvent RTSemAnn -> MASTGenMonad MASTTransaction
genMASTTransaction (SelectedEventBursty eventId emitterId transactionId initialStepId stepMap interval arrivals deadlines) = 
    undefined
genMASTTransaction (SelectedEventPeriodic eventId emitterId transId initialStepId stMap deadlines) = do
    let mastTransactionId = getMASTTransactionId eventId emitterId transId
    arch <- gets progArch
    period <- case M.lookup emitterId (emitters arch) of
        Just (TPPeriodicTimerEmitter _ expr _ _) -> 
            getTimerPeriod expr
        _ -> throwError . annotateError Internal $ EInvalidEmitterType
    emitterTarget <- case M.lookup emitterId (emitterTargets arch) of
        Just (target, _, _) -> return target
        Nothing -> throwError . annotateError Internal $ EUnknownEmitter emitterId

    let externalEventHandlerId = getMASTExternalEventId eventId emitterId
    ops <- gets operations
    schServers <- gets schedulingServers

    initialOperation <- case M.lookup initialStepId stMap of
        Just step -> return step
        Nothing -> throwError . annotateError Internal $ EInvalidInitialStep initialStepId
    
    case initialOperation of 
        TRPTaskOperation {} -> do
            -- | If the timer is connected to a task, we need to generate two event handlers:
            -- | 1. An external event handler for the timer's top half
            -- | 2. An internal event handler for the task's activity
            -- | First, we generate the top half operation in case it does not exist
            unless (M.member timerTopHalfMASTOperationId ops) $ genTimerTopHalfMASTOperation >>= insertOperation
            -- | And the scheduling server
            unless (M.member timerTopHalfSchedulingServerId schServers) $ genTimerTopHalfSchedulingServer >>= insertSchedulingServer
            -- | Now we have to create the external event handler
            let externalEventHandler = MASTTimedActivityEventHandler
                    externalEventHandlerId -- ^ event handler id
                    (getMASTInternalEventId initialStepId) -- ^ output event
                    timerTopHalfMASTOperationId -- ^ operation
                    timerTopHalfSchedulingServerId -- ^ scheduling server
            -- | Now, we need to create the internal event handler for the task activity
            let initialTransaction = MASTRegularTransaction
                    mastTransactionId
                    [] []
                    [externalEventHandler]
            genMASTTransactionStep initialTransaction initialStepId
        (TRPHandlerOperation _ _ _ _ _ [] _ _) -> throwError . annotateError Internal $ EInvalidStepType initialStepId
        op@(TRPHandlerOperation _ _ _ _ _ [c] _ _) -> do
            opId <- genMASTOperation False [] op
            -- | If the timer is connected to a handler, we only need to create an external event handler
            let externalEventHandler = MASTActivityEventHandler
                    externalEventHandlerId -- ^ event handler id
                    (getMASTInternalEventId c) -- ^ output event
                    opId -- ^ operation
                    timerTopHalfSchedulingServerId -- ^ scheduling server
            -- | Now, we need to create the internal event handler for the task activity
            let initialTransaction = MASTRegularTransaction
                    mastTransactionId
                    [] []
                    [externalEventHandler]
            genMASTTransactionStep initialTransaction c
        op@(TRPHandlerOperation _ _ _ _ _ continuations _ _) -> do
            opId <- genMASTOperation False [] op
            undefined
        _ -> throwError . annotateError Internal $ EInvalidStepType initialStepId
        
runMASTModelGenerator :: TerminaConfig -> TerminaProgArch SemanticAnn -> [SelectedEvent RTSemAnn] -> Either MASTGenErrors MASTModel
runMASTModelGenerator cfg arch peList =
    let initialState = MASTGenEnv
            {
                progArch = arch,
                configParams = cfg,
                resourceLockingMap = genResourceLockings arch,
                stepMap = M.empty,
                schedulingServers = M.empty,
                operations = M.empty,
                sharedResources = M.empty,
                transactions = M.empty
            }
    in
    case ST.runState (runExceptT (mapM_ genMASTTransaction peList)) initialState of 
        (Left err, _) -> Left err
        (_, st) -> undefined
