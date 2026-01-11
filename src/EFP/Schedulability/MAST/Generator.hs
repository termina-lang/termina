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
import EFP.Schedulability.RT.Semantic.AST
import Configuration.Configuration
import Semantic.Types
import ControlFlow.Architecture.Utils
import qualified Control.Monad.State as ST
import EFP.Schedulability.TransPath.Types


data SelectedEvent a =
    SelectedEventBursty
        Identifier -- ^ Event identifier
        Identifier -- ^ Emitter identifier
        Identifier -- ^ Transaction identifier
        Identifier -- ^ Initial step identifier
        (TRPStepMap a) -- ^ Steps map
        Time -- ^ Interval
        Integer -- ^ Arrivals expression
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

genMASTInternalEvent :: Identifier -> Identifier -> MASTGenMonad MASTInternalEvent
genMASTInternalEvent externalEventId stepName = do
    dlMap <- gets deadlinesMap
    let mDeadline = flip MASTGlobalDeadline externalEventId <$> M.lookup stepName dlMap
    return $ MASTRegularInternalEvent (getMASTInternalEventId stepName) mDeadline

genMASTTransactionStep :: MASTTransaction -> Identifier -> MASTGenMonad MASTTransaction
genMASTTransactionStep (MASTRegularTransaction mastTransactionId [extEvent] intEvents evHandlers) stepName = do
    arch <- gets progArch
    let externalEventId = case extEvent of
            MASTBurstyExternalEvent eid _ _ -> eid
            MASTPeriodicExternalEvent eid _ -> eid
    -- | First, we need to create the internal event for the step
    internalEvent <- genMASTInternalEvent externalEventId stepName
    stMap <- gets stepMap
    case M.lookup stepName stMap of
        Just op@(TRPTaskOperation _ taskId _ _ _ continuations _ _) -> do
            tsk <- case M.lookup taskId (tasks arch) of
                Just t -> return t
                Nothing -> throwError . annotateError Internal $ EUnknownTask taskId
            opId <- genMASTOperation False [] op
            -- | Now, we need to create the event handler for the operation
            schServers <- gets schedulingServers
            unless (M.member taskId schServers) $ genTaskSchedulingServer tsk >>= insertSchedulingServer

            case continuations of
                [] -> throwError . annotateError Internal $ EInvalidStepType stepName
                [c] -> do
                    let eventHandler = MASTActivityEventHandler
                            (getMASTInternalEventId stepName) -- ^ event handler id
                            (getMASTInternalEventId c) -- ^ output event
                            opId
                            taskId
                    let newTransaction = MASTRegularTransaction mastTransactionId [extEvent] 
                            (internalEvent : intEvents) 
                            (eventHandler : evHandlers)
                    genMASTTransactionStep newTransaction c
                _ -> do
                    let multicastEventHandler = MASTMulticastEventHandler
                            (getMASTInternalEventId stepName)
                            (map getMASTInternalEventId continuations)
                    let eventHandler = MASTActivityEventHandler
                            (getMASTInternalEventId stepName) -- ^ event handler id
                            (genMASTMulticastEventId stepName) -- ^ output event
                            opId
                            taskId
                    foldM genMASTTransactionStep
                        (MASTRegularTransaction mastTransactionId [extEvent] 
                            (internalEvent : intEvents) 
                            (eventHandler : multicastEventHandler : evHandlers))
                        continuations
        Just _ -> do
            throwError . annotateError Internal $ EInvalidStepType stepName
        Nothing ->
            -- | If we are here, it means that the step is an end step
            return $ MASTRegularTransaction mastTransactionId [extEvent] (internalEvent : intEvents) evHandlers
genMASTTransactionStep _ _ = throwError . annotateError Internal $ EInvalidTransactionStructure

genMASTTransaction :: SelectedEvent TRPSemAnn -> MASTGenMonad MASTTransaction
genMASTTransaction (SelectedEventBursty eventId emitterId transId initialStepId stMap interval arrivals deadlines) = do

    let externalEventId = getMASTExternalEventId eventId emitterId
        mastTransactionId = getMASTTransactionId eventId emitterId transId
    
    let externalEvent = MASTBurstyExternalEvent
            externalEventId
            interval 
            arrivals
    
    ST.modify $ \env -> env
        { stepMap = stMap
        , deadlinesMap = deadlines
        }

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
            unless (M.member (irqTopHalfMASTOperationId emitterId) ops) $ 
                genIrqTopHalfMASTOperation emitterId >>= insertOperation
            unless (M.member (irqHandlerSchedulerServerId emitterId) schServers) $
                genIrqHandlerSchedulingServer emitterId >>= insertSchedulingServer
            -- | Now we have to create the external event handler
            let externalEventHandler = MASTTimedActivityEventHandler
                    externalEventId -- ^ event handler id
                    (getMASTInternalEventId initialStepId) -- ^ output event
                    (irqTopHalfMASTOperationId emitterId) -- ^ operation
                    (irqHandlerSchedulerServerId emitterId) -- ^ scheduling server
            -- | Now, we need to create the internal event handler for the task activity
            let initialTransaction = MASTRegularTransaction
                    mastTransactionId
                    [externalEvent] []
                    [externalEventHandler]
            genMASTTransactionStep initialTransaction initialStepId
        op@(TRPHandlerOperation _ _ _ _ _ continuations _ _) -> do
            opId <- genMASTOperation False [] op
            case continuations of
                [] -> throwError . annotateError Internal $ EInvalidStepType initialStepId
                [c] -> do
                    unless (M.member (irqHandlerSchedulerServerId emitterId) schServers) $
                        genIrqHandlerSchedulingServer emitterId >>= insertSchedulingServer
                    let externalEventHandler = MASTActivityEventHandler
                            externalEventId -- ^ event handler id
                            (getMASTInternalEventId c) -- ^ output event
                            opId -- ^ operation
                            (irqHandlerSchedulerServerId emitterId) -- ^ scheduling server
                    -- | Now, we need to create the internal event handler for the task activity
                    let initialTransaction = MASTRegularTransaction
                            mastTransactionId
                            [externalEvent] []
                            [externalEventHandler]
                    genMASTTransactionStep initialTransaction c
                _ -> do
                    let multicastEventHandler = MASTMulticastEventHandler
                            (getMASTInternalEventId initialStepId)
                            (map getMASTInternalEventId continuations)

                    unless (M.member (irqHandlerSchedulerServerId emitterId) schServers) $
                        genIrqHandlerSchedulingServer emitterId >>= insertSchedulingServer

                    let externalEventHandler = MASTActivityEventHandler
                            externalEventId -- ^ event handler id
                            (genMASTMulticastEventId initialStepId) -- ^ output event
                            opId
                            (irqHandlerSchedulerServerId emitterId)
                    foldM genMASTTransactionStep
                        (MASTRegularTransaction externalEventId [externalEvent] 
                            [] 
                            [multicastEventHandler, externalEventHandler])
                        continuations
        _ -> throwError . annotateError Internal $ EInvalidStepType initialStepId
    
genMASTTransaction (SelectedEventPeriodic eventId emitterId transId initialStepId stMap deadlines) = do

    let externalEventId = getMASTExternalEventId eventId emitterId
        mastTransactionId = getMASTTransactionId eventId emitterId transId

    arch <- gets progArch
    period <- case M.lookup emitterId (emitters arch) of
        Just (TPPeriodicTimerEmitter _ expr _ _) ->
            getTimerPeriod expr
        _ -> throwError . annotateError Internal $ EInvalidEmitterType
    
    let externalEvent = MASTPeriodicExternalEvent
            externalEventId
            period
    
    ST.modify $ \env -> env
        { stepMap = stMap
        , deadlinesMap = deadlines
        }
    
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
                    externalEventId -- ^ event handler id
                    (getMASTInternalEventId initialStepId) -- ^ output event
                    timerTopHalfMASTOperationId -- ^ operation
                    timerTopHalfSchedulingServerId -- ^ scheduling server
            -- | Now, we need to create the internal event handler for the task activity
            let initialTransaction = MASTRegularTransaction
                    mastTransactionId
                    [externalEvent] []
                    [externalEventHandler]
            genMASTTransactionStep initialTransaction initialStepId
        op@(TRPHandlerOperation _ _ _ _ _ continuations _ _) -> do
            opId <- genMASTOperation False [] op
            case continuations of
                [] -> throwError . annotateError Internal $ EInvalidStepType initialStepId
                [c] -> do
                    unless (M.member timerTopHalfSchedulingServerId schServers) $ 
                        genTimerTopHalfSchedulingServer >>= insertSchedulingServer
                    let externalEventHandler = MASTActivityEventHandler
                            externalEventId -- ^ event handler id
                            (getMASTInternalEventId c) -- ^ output event
                            opId -- ^ operation
                            timerTopHalfSchedulingServerId -- ^ scheduling server
                    -- | Now, we need to create the internal event handler for the task activity
                    let initialTransaction = MASTRegularTransaction
                            mastTransactionId
                            [externalEvent] []
                            [externalEventHandler]
                    genMASTTransactionStep initialTransaction c
                _ -> do
                    let multicastEventHandler = MASTMulticastEventHandler
                            (getMASTInternalEventId initialStepId)
                            (map getMASTInternalEventId continuations)

                    unless (M.member timerTopHalfSchedulingServerId schServers) $ 
                        genTimerTopHalfSchedulingServer >>= insertSchedulingServer

                    let externalEventHandler = MASTActivityEventHandler
                            externalEventId -- ^ event handler id
                            (genMASTMulticastEventId initialStepId) -- ^ output event
                            opId
                            timerTopHalfSchedulingServerId
                    foldM genMASTTransactionStep
                        (MASTRegularTransaction mastTransactionId [externalEvent] 
                            [] 
                            [multicastEventHandler, externalEventHandler])
                        continuations
        _ -> throwError . annotateError Internal $ EInvalidStepType initialStepId

genMASTModel :: Identifier -> [SelectedEvent TRPSemAnn] -> MASTGenMonad MASTModel
genMASTModel modelName peList = do
    -- | First, we generate the system call operations
    syscallOps <- genSystemCallMASTOperations
    -- | Insert them into the operations map
    ST.modify $ \env -> env { operations = syscallOps }
    -- | Now, we generate the transactions
    trans <- mapM genMASTTransaction peList
    ops <- gets operations
    schServers <- gets schedulingServers
    shRes <- gets sharedResources
    prResources <- getProcessingResources
    scheds <- getSchedulers
    return $ MASTModel
        { 
          mastModelName = modelName
        , mastOperations = ops
        , mastSchedServers = schServers
        , mastSharedResources = shRes
        , mastTransactions = M.fromList [(transId, t) | t@(MASTRegularTransaction transId _ _ _) <- trans]
        , mastProcessingResources = prResources
        , mastSchedulers = scheds
        }

runMASTModelGenerator :: TerminaConfig 
    -> TerminaProgArch SemanticAnn 
    -> Identifier 
    -> [SelectedEvent TRPSemAnn] -> Either MASTGenErrors MASTModel
runMASTModelGenerator cfg arch modelName peList =
    let initialState = MASTGenEnv
            {
                progArch = arch,
                configParams = cfg,
                resourceLockingMap = genResourceLockings arch,
                schedulingServers = M.empty,
                operations = M.empty,
                sharedResources = M.empty,
                transactions = M.empty,
                stepMap = M.empty,
                deadlinesMap = M.empty
            }
    in
    ST.evalState (runExceptT (genMASTModel modelName peList)) initialState
