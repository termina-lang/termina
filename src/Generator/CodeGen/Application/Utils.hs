{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Utils where

import ControlFlow.BasicBlocks.AST
import Control.Monad
import Control.Monad.Except
import Generator.CodeGen.Common
import ControlFlow.Architecture.Types
import Semantic.Types
import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.LanguageC.Embedded
import Generator.CodeGen.Expression
import Generator.CodeGen.Types
import qualified Data.Set as S
import ControlFlow.Architecture.Utils
import Utils.Annotations

-- | Data type used to represent a message queue in OSAL
--  There are different types depending on the original source code element that
--  generated the message queue
data OSALMsgQueue =
    OSALTaskMsgQueue
      Identifier -- ^ task identifier
      Identifier -- ^ task class identifier
      (Expression SemanticAnn) -- ^ message queue size
    | OSALChannelMsgQueue
      Identifier -- ^ name of the channel
      (TerminaType SemanticAnn) -- ^ type of the elements of the message queue
      (Expression SemanticAnn) -- ^ message queue size
      Identifier -- ^ name of the task that will receive the messages
      Identifier -- ^ name of the port to wich the messages will be sent
    | OSALSinkPortMsgQueue
      Identifier -- ^ identifier of the receiving task
      Identifier -- ^ identifier of the class of the receiving task
      Identifier -- ^ identifier of the port that will receive the messages
      (TerminaType SemanticAnn) -- ^ type of the elements of the message queue
      (Expression SemanticAnn) -- ^ message queue size
    deriving Show

-- | Data type used to represent a resource lock in OSAL
data OSALResourceLock =
    OSALResourceLockNone -- ^ no resource lock
    | OSALResourceLockIrq  -- ^ interrupt lock (disable and enable interrupts)
    | OSALResourceLockMutex TInteger -- ^ mutex lock with priority-ceiling protocol
    deriving Show

-- | This function generates the list of OSAL message queues that must be created
-- from the list of tasks in the program architecture. For each task, a message queue
-- is created for each sink port of the task.
getSinkPortMessageQueues :: (MonadError CGeneratorError m) => TerminaProgArch SemanticAnn -> m [OSALMsgQueue]
getSinkPortMessageQueues progArchitecture = return $ foldr (\glb acc ->
        case glb of
            TPTask identifier classId _ sinkPConns _ _ _ _ _ -> 
                let tpClass = taskClasses progArchitecture M.! classId in
                foldr (\portId acc' ->
                    let (ts, _) = sinkPorts tpClass M.! portId in
                    OSALSinkPortMsgQueue identifier classId portId ts (Constant (I (TInteger 1 DecRepr) Nothing) (buildExpAnn Internal (TConstSubtype TUSize))) : acc'
                ) acc (M.keys sinkPConns)
    ) [] (tasks progArchitecture)

getTasksMessageQueues :: (MonadError CGeneratorError m) => TerminaProgArch SemanticAnn -> [OSALMsgQueue] -> m [OSALMsgQueue]
getTasksMessageQueues progArchitecture msgqs = do
    foldM (\acc (TPTask identifier classId _ _ _ _ _ _ _) -> do
        filteredQueues <- filterM (\case {
            OSALTaskMsgQueue {} -> throwError $ InternalError "getTasksMessageQueues: there is already a task message queue";
            OSALChannelMsgQueue _ _ _ target _ -> return $ target == identifier;
            OSALSinkPortMsgQueue target _ _ _ _  -> return $ target == identifier;
        }) msgqs
        curr <- getTaskMessageQueue identifier classId filteredQueues
        return $ curr : acc) [] (tasks progArchitecture)
    
    where 

        getTaskMessageQueue :: (MonadError CGeneratorError m) => Identifier -> Identifier -> [OSALMsgQueue] -> m OSALMsgQueue
        getTaskMessageQueue identifier classId (queue : msgQueues) = do
            size <- case queue of 
                OSALTaskMsgQueue {} -> throwError $ InternalError "getTaskMessageQueue: there is already a task message queue"
                OSALChannelMsgQueue _ _ size' _ _ -> return size'
                OSALSinkPortMsgQueue _ _ _ _ size' -> return size'
            size' <- foldM (\acc q ->
                case q of
                    OSALTaskMsgQueue {} -> throwError $ InternalError "getTaskMessageQueue: there is already a task message queue"
                    OSALChannelMsgQueue _ _ size' _ _ -> return $ BinOp Addition acc size' (buildExpAnn Internal (TConstSubtype TUSize))
                    OSALSinkPortMsgQueue _ _ _ _ size' -> return $ BinOp Addition acc size' (buildExpAnn Internal (TConstSubtype TUSize))
                ) size msgQueues
            return $ OSALTaskMsgQueue identifier classId size'
        getTaskMessageQueue _ _ _ = throwError $ InternalError "getTaskMessageQueue: the task does not have message queues"


-- | This function generates the list of OSAL message queues that must be created
-- from the list of channels in the program architecture. For each channel, a message
-- queue is created for the channel itself.
getChannelsMessageQueues :: (MonadError CGeneratorError m) => TerminaProgArch SemanticAnn -> m [OSALMsgQueue]
getChannelsMessageQueues progArchitecture = 
    foldM (\acc (TPMsgQueue identifier ts size _ _) ->
        case M.lookup identifier (channelTargets progArchitecture) of
            Just (task, port, _) -> return (OSALChannelMsgQueue identifier ts size task port : acc)
            Nothing -> throwError $ InternalError ("channel not connected: " ++ show identifier)
    ) [] (channels progArchitecture)

genVariantForPort ::
    -- | Name of the task class
    Identifier
    -- | Name of the port
    -> Identifier -> CGenerator Identifier
genVariantForPort taskCls port = return $ namefy $ taskCls <::> port

genVariantsForTaskPorts :: TPClass SemanticAnn -> CGenerator [CFileItem]
genVariantsForTaskPorts tpClass@(TPClass classId _ _ _ _ _ _ _) =
    genDefineVariantsForPorts ports
    where

        ports = M.keys (sinkPorts tpClass) ++ M.keys (inputPorts tpClass)

        genDefineVariantsForPorts :: [Identifier] -> CGenerator [CFileItem]
        genDefineVariantsForPorts [] = return []
        genDefineVariantsForPorts (port : xs) = do
            this_variant <- genVariantForPort classId port
            rest <- genDefineVariantsForPorts' xs 1
            return $ pre_cr (_define this_variant (Just [show (0 :: Integer)])) : rest

        genDefineVariantsForPorts' :: [Identifier] -> Integer -> CGenerator [CFileItem]
        genDefineVariantsForPorts' [] _ = return []
        genDefineVariantsForPorts' (port : xs) value = do
            rest <- genDefineVariantsForPorts' xs (value + 1)
            this_variant <- genVariantForPort classId port
            return $ _define this_variant (Just [show value]) : rest

genPoolMemoryArea :: Bool -> TPPool SemanticAnn -> CGenerator CFileItem
genPoolMemoryArea before (TPPool identifier ts size _ _) = do
    cSize <- genExpression size
    cType <- genType noqual ts
    let poolSize = __termina_pool__size @@ [_sizeOfType cType, cSize]
    if before then 
        return $ pre_cr $ static_global (var (poolMemoryArea identifier) (CTArray uint8_t poolSize))
    else
        return $ static_global (var (poolMemoryArea identifier) (CTArray uint8_t poolSize))

genPoolMemoryAreas :: [TPPool SemanticAnn] -> CGenerator [CFileItem]
genPoolMemoryAreas [] = return []
genPoolMemoryAreas (obj : objs) = do
    memArea <- genPoolMemoryArea True obj
    rest <- mapM (genPoolMemoryArea False) objs
    return $ memArea : rest

genAtomicDeclaration :: Bool -> TPAtomic SemanticAnn -> CGenerator CFileItem
genAtomicDeclaration before (TPAtomic identifier ts _ _) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType atomic ts
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cType) (Just identifier) Nothing)) declStmt

genAtomicDeclarations :: [TPAtomic SemanticAnn] -> CGenerator [CFileItem]
genAtomicDeclarations [] = return []
genAtomicDeclarations (obj : objs) = do
    decl <- genAtomicDeclaration True obj
    rest <- mapM (genAtomicDeclaration False) objs
    return $ decl : rest

genAtomicArrayDeclaration :: Bool -> TPAtomicArray SemanticAnn -> CGenerator CFileItem
genAtomicArrayDeclaration before (TPAtomicArray identifier ts size _ _) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cSize <- genExpression size
    cType <- genType atomic ts
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec (CTArray cType cSize)) (Just identifier) Nothing)) declStmt

genAtomicArrayDeclarations :: [TPAtomicArray SemanticAnn] -> CGenerator [CFileItem]
genAtomicArrayDeclarations [] = return []
genAtomicArrayDeclarations (obj : objs) = do
    decl <- genAtomicArrayDeclaration True obj
    rest <- mapM (genAtomicArrayDeclaration False) objs
    return $ decl : rest

-- | Generates a map between the resources and the locking mechanism that must be used
genResLockingMap :: (MonadError CGeneratorError m) => TerminaProgArch a -> M.Map Identifier (S.Set Identifier) -> m (M.Map Identifier OSALResourceLock)
genResLockingMap progArchitecture = foldM (\acc (resId, resDeps) -> do
        resLocking <- getResLocking (S.toList resDeps)
        return $ M.insert resId resLocking acc
    ) M.empty . M.toList

    where

        -- | Obtains the locking mechanism that must be used for a resource
        getResLocking :: (MonadError CGeneratorError m) => [Identifier] -> m OSALResourceLock
        getResLocking [] = return OSALResourceLockNone
        getResLocking [_] = return OSALResourceLockNone
        getResLocking (ident: ids) = 
            case M.lookup ident (handlers progArchitecture) of
                Just _ -> return OSALResourceLockIrq
                Nothing -> case M.lookup ident (tasks progArchitecture) of
                    Just tsk -> 
                        getResLocking' (getPriority tsk) ids
                    Nothing -> throwError $ InternalError "genResLockingMap: the resource does not depend on a task nor a handler"

        getResLocking' :: (MonadError CGeneratorError m) =>  TInteger -> [Identifier] -> m OSALResourceLock
        -- | If we have reach the end of the list, it means that there are at least two different tasks that
        -- access the resource. We are going to force the use of the priority ceiling algorithm. In the
        -- (hopefully near) future, we will support algorithm selection via the configuration file.
        getResLocking' ceilPrio [] = return $ OSALResourceLockMutex ceilPrio
        getResLocking' ceilPrio (ident' : ids') = 
            case M.lookup ident' (handlers progArchitecture) of
                Just _ -> return OSALResourceLockIrq
                Nothing -> case M.lookup ident' (tasks progArchitecture) of
                    Just tsk -> 
                        getResLocking' (min ceilPrio (getPriority tsk)) ids'
                    Nothing -> throwError $ InternalError "genResLockingMap: the resource does not depend on a task nor a handler"

genDefineMutexIdLabel :: Identifier -> CGenerator Identifier
genDefineMutexIdLabel m = return $ namefy m <::> "mutex_id"

genDefineMutexId :: [Identifier] -> CGenerator [CFileItem]
genDefineMutexId [] = return []
genDefineMutexId (mutex : xs) = do
    this_mutex <- genDefineMutexIdLabel mutex
    rest <- genDefineMutexId' xs 1
    return $ pre_cr (_define this_mutex (Just [show (0 :: Integer)])) : rest

    where

        genDefineMutexId' :: [Identifier] -> Integer -> CGenerator [CFileItem]
        genDefineMutexId' [] _ = return []
        genDefineMutexId' (m : xm) value = do
            rest <- genDefineMutexId' xm (value + 1)
            this_mutex <- genDefineMutexIdLabel m
            return $ _define this_mutex (Just [show value]) : rest

genDefineTaskIdLabel :: Identifier -> CGenerator Identifier
genDefineTaskIdLabel t = return $ namefy t <::> "task_id"

genDefineHandlerIdLabel :: Identifier -> CGenerator Identifier
genDefineHandlerIdLabel t = return $ namefy t <::> "handler_id"

genDefineHandlerId :: [Identifier] -> CGenerator [CFileItem]
genDefineHandlerId [] = return []
genDefineHandlerId (task : xs) = do
    this_task <- genDefineHandlerIdLabel task
    rest <- genDefineHandlerId' xs 1
    return $ pre_cr (_define this_task (Just [show (0 :: Integer)])) : rest

    where

        genDefineHandlerId' :: [Identifier] -> Integer -> CGenerator [CFileItem]
        genDefineHandlerId' [] _ = return []
        genDefineHandlerId' (t : xt) value = do
            rest <- genDefineHandlerId' xt (value + 1)
            this_task <- genDefineHandlerIdLabel t
            return $ _define this_task (Just [show value]) : rest


genDefineTaskId :: [Identifier] -> CGenerator [CFileItem]
genDefineTaskId [] = return []
genDefineTaskId (task : xs) = do
    this_task <- genDefineTaskIdLabel task
    rest <- genDefineTaskId' xs 1
    return $ pre_cr (_define this_task (Just [show (0 :: Integer)])) : rest

    where

        genDefineTaskId' :: [Identifier] -> Integer -> CGenerator [CFileItem]
        genDefineTaskId' [] _ = return []
        genDefineTaskId' (t : xt) value = do
            rest <- genDefineTaskId' xt (value + 1)
            this_task <- genDefineTaskIdLabel t
            return $ _define this_task (Just [show value]) : rest

genDefinePoolIdLabel :: Identifier -> CGenerator Identifier
genDefinePoolIdLabel p = return $ namefy p <::> "pool_id"

genDefinePoolId :: [Identifier] -> CGenerator [CFileItem]
genDefinePoolId [] = return []
genDefinePoolId (pl : xs) = do
    this_pool <- genDefinePoolIdLabel pl
    rest <- genDefinePoolId' xs 1
    return $ pre_cr (_define this_pool (Just [show (0 :: Integer)])) : rest

    where

        genDefinePoolId' :: [Identifier] -> Integer -> CGenerator [CFileItem]
        genDefinePoolId' [] _ = return []
        genDefinePoolId' (p : xp) value = do
            rest <- genDefinePoolId' xp (value + 1)
            this_pool <- genDefinePoolIdLabel p
            return $ _define this_pool (Just [show value]) : rest

genDefineTimerIdLabel :: Identifier -> CGenerator Identifier
genDefineTimerIdLabel t = return $ namefy t <::> "timer_id"

genDefineTimerId :: [Identifier] -> CGenerator [CFileItem]
genDefineTimerId [] = return []
genDefineTimerId (timer : xmp) = do
    this_timer <- genDefineTimerIdLabel timer
    rest <- genDefineTimerId' xmp 1
    return $ pre_cr (_define this_timer (Just [show (0 :: Integer)])) : rest

    where

        genDefineTimerId' :: [Identifier] -> Integer -> CGenerator [CFileItem]
        genDefineTimerId' [] _ = return []
        genDefineTimerId' (timer' : xmp') value = do
            rest <- genDefineTimerId' xmp' (value + 1)
            this_timer <- genDefineTimerIdLabel timer'
            return $ _define this_timer (Just [show value]) : rest

genDefineTaskMsgQueueIdLabel :: Identifier -> CGenerator Identifier
genDefineTaskMsgQueueIdLabel t = return $ namefy t <::> "task_msg_queue_id"

genDefineChannelMsgQueueIdLabel :: Identifier -> CGenerator Identifier
genDefineChannelMsgQueueIdLabel c = return $ namefy c <::> "channel_msg_queue_id"

genDefineSinkMsgQueueIdLabel :: Identifier -> Identifier -> CGenerator Identifier
genDefineSinkMsgQueueIdLabel t p = return $ namefy t <::> p <::> "sink_msg_queue_id"

genDefineMsgQueueIdLabel :: OSALMsgQueue -> CGenerator Identifier
genDefineMsgQueueIdLabel (OSALTaskMsgQueue t _ _) = genDefineTaskMsgQueueIdLabel t
genDefineMsgQueueIdLabel (OSALChannelMsgQueue c _ _ _ _) = genDefineChannelMsgQueueIdLabel c
genDefineMsgQueueIdLabel (OSALSinkPortMsgQueue t _ p _ _) = genDefineSinkMsgQueueIdLabel t p

genDefineMsgQueueId :: [OSALMsgQueue] -> CGenerator [CFileItem]
genDefineMsgQueueId [] = return []
genDefineMsgQueueId (mq : xmp) = do
    this_msg_queue <- genDefineMsgQueueIdLabel mq
    rest <- genDefineMsgQueueId' xmp 1
    return $ pre_cr (_define this_msg_queue (Just [show (0 :: Integer)])) : rest

    where

        genDefineMsgQueueId' :: [OSALMsgQueue] -> Integer -> CGenerator [CFileItem]
        genDefineMsgQueueId' [] _ = return []
        genDefineMsgQueueId' (mq' : xmq') value = do
            rest <- genDefineMsgQueueId' xmq' (value + 1)
            this_msg_queue <- genDefineMsgQueueIdLabel mq'
            return $ _define this_msg_queue (Just [show value]) : rest