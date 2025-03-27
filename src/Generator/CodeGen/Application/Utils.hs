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
import Generator.CodeGen.Types
import qualified Data.Set as S

-- | Data type used to represent a message queue in OSAL
--  There are different types depending on the original source code element that
--  generated the message queue
data OSALMsgQueue =
    OSALTaskMsgQueue
      Identifier -- ^ task identifier
      Identifier -- ^ task class identifier
      Size -- ^ message queue size
    | OSALChannelMsgQueue
      Identifier -- ^ name of the channel
      TerminaType -- ^ type of the elements of the message queue
      Size -- ^ message queue size
      Identifier -- ^ name of the task that will receive the messages
      Identifier -- ^ name of the port to wich the messages will be sent
    | OSALSinkPortMsgQueue
      Identifier -- ^ identifier of the receiving task
      Identifier -- ^ identifier of the class of the receiving task
      Identifier -- ^ identifier of the port that will receive the messages
      TerminaType -- ^ type of the elements of the message queue
      Size -- ^ message queue size
    deriving Show

-- | Data type used to represent a resource lock in OSAL
data OSALResourceLock =
    OSALResourceLockNone -- ^ no resource lock
    | OSALResourceLockIrq  -- ^ interrupt lock (disable and enable interrupts)
    | OSALResourceLockMutex TInteger -- ^ mutex lock with priority-ceiling protocol
    deriving Show

-- | Returns the value of the "priority" modifier, if present in the list of modifiers.
-- If not, it returns 255, which is the default value for the priority (the lowest).
getPriority :: [Modifier] -> TInteger
getPriority [] = TInteger 255 DecRepr
getPriority ((Modifier "priority" (Just (I priority _))) : _) = priority
getPriority (_ : modifiers) = getPriority modifiers

-- | Returns the value of the "stack_size" modifier, if present in the list of modifiers.
-- If not, it returns 4096, which is the default value for the stack size (RTEMS_MINIUMUM_STACK_SIZE)
getStackSize :: [Modifier] -> TInteger
getStackSize [] = TInteger 4096 DecRepr
getStackSize ((Modifier "stack_size" (Just (I stackSize _))) : _) = stackSize
getStackSize (_ : modifiers) = getStackSize modifiers

-- | This function generates the list of OSAL message queues that must be created
-- from the list of tasks in the program architecture. For each task, a message queue
-- is created for the task itself and for each sink port of the task.
getTasksMessageQueues :: (MonadError CGeneratorError m) => TerminaProgArch SemanticAnn -> m [OSALMsgQueue]
getTasksMessageQueues progArchitecture = return $ foldr (\glb acc ->
        case glb of
            TPTask identifier classId _ sinkPConns _ _ _ _ _ -> OSALTaskMsgQueue identifier classId (K (TInteger 1 DecRepr)) :
                let tpClass = taskClasses progArchitecture M.! classId in
                foldr (\portId acc' ->
                    let (ts, _) = sinkPorts tpClass M.! portId in
                    OSALSinkPortMsgQueue identifier classId portId ts (K (TInteger 1 DecRepr)) : acc'
                ) acc (M.keys sinkPConns)
    ) [] (tasks progArchitecture)

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
genVariantsForTaskPorts (TPClass classId _ (Class _ _ members _ _) _ _ _ _ _) =
    genDefineVariantsForPorts ports
    where

        ports = foldr (\fld acc ->
                        case fld of
                            ClassField (FieldDefinition prt (TSinkPort {})) _ -> prt : acc
                            ClassField (FieldDefinition prt (TInPort {})) _ -> prt : acc
                            _ -> acc ) [] members

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

genVariantsForTaskPorts def = throwError $ InternalError $ "Definition not a class: " ++ show def

genPoolMemoryArea :: Bool -> TPPool a -> CGenerator CFileItem
genPoolMemoryArea before (TPPool identifier ts size _ _) = do
    cSize <- genArraySize size
    cType <- genType noqual ts
    let poolSize = __termina_pool__size @@ [_sizeOfType cType, cSize]
    if before then 
        return $ pre_cr $ static_global (poolMemoryArea identifier) (CTArray uint8_t poolSize)
    else
        return $ static_global (poolMemoryArea identifier) (CTArray uint8_t poolSize)

genPoolMemoryAreas :: [TPPool a] -> CGenerator [CFileItem]
genPoolMemoryAreas [] = return []
genPoolMemoryAreas (obj : objs) = do
    memArea <- genPoolMemoryArea True obj
    rest <- mapM (genPoolMemoryArea False) objs
    return $ memArea : rest

genAtomicDeclaration :: Bool -> TPAtomic a -> CGenerator CFileItem
genAtomicDeclaration before (TPAtomic identifier ts _ _) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType atomic ts
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cType) (Just identifier) Nothing)) declStmt

genAtomicDeclarations :: [TPAtomic a] -> CGenerator [CFileItem]
genAtomicDeclarations [] = return []
genAtomicDeclarations (obj : objs) = do
    decl <- genAtomicDeclaration True obj
    rest <- mapM (genAtomicDeclaration False) objs
    return $ decl : rest

genAtomicArrayDeclaration :: Bool -> TPAtomicArray a -> CGenerator CFileItem
genAtomicArrayDeclaration before (TPAtomicArray identifier ts size _ _) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cSize <- genArraySize size
    cType <- genType atomic ts
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec (CTArray cType cSize)) (Just identifier) Nothing)) declStmt

genAtomicArrayDeclarations :: [TPAtomicArray a] -> CGenerator [CFileItem]
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
                    Just (TPTask _ _ _ _ _ _ modifiers _ _) -> 
                        getResLocking' (getPriority modifiers) ids
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
                    Just (TPTask _ _ _ _ _ _ modifiers _ _) -> 
                        getResLocking' (min ceilPrio (getPriority modifiers)) ids'
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

genDefineMsgQueueIdLabel :: OSALMsgQueue -> CGenerator Identifier
genDefineMsgQueueIdLabel (OSALTaskMsgQueue t _ _) = return $ namefy t <::> "msg_queue_id"
genDefineMsgQueueIdLabel (OSALChannelMsgQueue c _ _ _ _) = return $ namefy c <::> "msg_queue_id"
genDefineMsgQueueIdLabel (OSALSinkPortMsgQueue t _ p _ _) = return $ namefy t <::> p <::> "msg_queue_id"

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