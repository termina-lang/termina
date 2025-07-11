{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Glue where

import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.CodeGen.Common
import Semantic.Types
import ControlFlow.Architecture.Types
import Configuration.Configuration
import Generator.LanguageC.Embedded
import ControlFlow.Architecture.Utils
import System.FilePath
import Control.Monad.Except
import Generator.CodeGen.Application.Utils
import Generator.CodeGen.Types
import Semantic.AST
import Control.Monad (forM)
import Data.List (find)
import Generator.CodeGen.Expression
import Generator.Monadic
import Control.Monad.State
import qualified Data.Set as S
import Utils.Annotations

genInitHandlers :: TerminaProgArch a -> CGenerator CFileItem
genInitHandlers progArchitecture = do
    let progHandlers = M.elems $ handlers progArchitecture
    initHandlers <- mapM genOSALHandlerInit progHandlers
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_handlers") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initHandlers

    where

        genOSALHandlerInit :: TPHandler a -> CGenerator CCompoundBlockItem
        genOSALHandlerInit hndlr = do
            handlerId <- genDefineHandlerIdLabel (handlerName hndlr)
            return $ pre_cr $
                handlerName hndlr @: typeDef (handlerClass hndlr) @. handlerIDField @: __termina_id_t @= handlerId @: __termina_id_t

genInitTasks :: TerminaProgArch a -> CGenerator CFileItem
genInitTasks progArchitecture = do
    let progTasks = M.elems $ tasks progArchitecture
    initTasks <- mapM genOSALTaskInit progTasks
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_tasks") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initTasks

    where
        genOSALTaskInit :: TPTask a -> CGenerator CCompoundBlockItem
        genOSALTaskInit tsk = do
            taskId <- genDefineTaskIdLabel (taskName tsk)
            taskMsgQueueId <- genDefineTaskMsgQueueIdLabel (taskName tsk)
            let tskName = taskName tsk
                classId = taskClass tsk
                taskPrio = getCInteger . getPriority $ tsk
                taskStackSize = getCInteger . getStackSize $ tsk
            cTaskFunctionName <- taskFunctionName classId
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ tskName @: typeDef classId @. taskIDField @: __termina_id_t
                            @= taskId @: __termina_id_t,
                        pre_cr $ tskName @: typeDef classId @. namefy "task_msg_queue_id" @: __termina_id_t
                            @= taskMsgQueueId @: __termina_id_t,
                        pre_cr $ __termina_task__init @@ [
                            taskId @: __termina_id_t,
                            taskPrio @: __termina_task_prio_t,
                            taskStackSize @: size_t,
                            cTaskFunctionName @: __termina_task_entry_t,
                            addrOf (tskName @: typeDef classId),
                            "status" @: (_const . ptr $ int32_t)
                        ]
                ]

-- | Function __termina_app__install_emitters. This function is called from the
-- Init task.  The function installs the ISRs and the periodic timers. The
-- function is called AFTER the initialization of the tasks and handlers.
genInitEmitters :: TerminaProgArch a -> CGenerator CFileItem
genInitEmitters progArchitecture = do
    let progEmitters = M.elems $ emitters progArchitecture
    initEmitter <- mapM genOSALEmitterInit $ filter (\case { TPSystemInitEmitter {} -> False; _ -> True }) progEmitters
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_emitters") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initEmitter

    where

        genEmitterConnection :: TPEmitter a -> CGenerator [CCompoundBlockItem]
        genEmitterConnection (TPPeriodicTimerEmitter timer _ _) = do
            timerId <- genDefineTimerIdLabel timer
            emitterId <- genDefineEmitterIdLabel timer
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            (targetEntity, targetPort) <- case M.lookup timer (emitterTargets progArchitecture) of
                Just (entity, port, _) -> return (entity, port)
                -- | If the interrupt emitter is not connected, throw an error
                Nothing -> throwError $ InternalError $ "Periodic timer emitter not connected: " ++ show timer
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier classId _ _ _ _ _ _) -> do
                    let cls = handlerClasses progArchitecture M.! classId
                        (_, targetAction) = sinkPorts cls M.! targetPort
                    handlerId <- genDefineHandlerIdLabel identifier
                    return [
                            pre_cr $ var "connection" __termina_periodic_timer_connection_t,
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "type" @: enumFieldType
                                @= "__termina_emitter_connection_type__handler" @: enumFieldType,
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "handler" @: __termina_periodic_timer_handler_connection_t
                                @. "handler_object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "handler" @: __termina_periodic_timer_handler_connection_t
                                @. "handler_id" @: __termina_periodic_timer_action_t @= handlerId @: size_t,
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "handler" @: __termina_periodic_timer_handler_connection_t
                                @. "handler_action" @: __termina_periodic_timer_action_t @= classId <::> targetAction @: __termina_periodic_timer_action_t,
                            pre_cr $ __termina_periodic_timer__init @@ [
                                timerId @: __termina_id_t,
                                emitterId @: __termina_id_t,
                                addrOf ("connection" @: __termina_periodic_timer_connection_t),
                                addrOf (timer @: __termina_periodic_timer_t @. "period" @: _TimeVal),
                                "status" @: (_const . ptr $ int32_t)
                            ]
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetEntity
                        sinkMsgQueueId <- genDefineSinkMsgQueueIdLabel targetEntity targetPort
                        return [
                                pre_cr $ var "connection" __termina_periodic_timer_connection_t,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "type" @: enumFieldType
                                    @= "__termina_emitter_connection_type__task" @: enumFieldType,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "task_msg_queue_id" @: __termina_id_t @= taskMsgQueueId @: __termina_id_t,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_msgq_id" @: __termina_id_t @= sinkMsgQueueId @:  __termina_id_t,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_port_id" @: __termina_id_t @= variantForPort @:  __termina_id_t,
                                pre_cr $ targetEntity @: typeDef tskCls @. targetPort @: __termina_id_t
                                    @= sinkMsgQueueId @: __termina_id_t,
                                pre_cr $ __termina_periodic_timer__init @@ [
                                    timerId @: __termina_id_t,
                                    emitterId @: __termina_id_t,
                                    addrOf ("connection" @: __termina_periodic_timer_connection_t),
                                    addrOf (timer @: __termina_periodic_timer_t @. "period" @: _TimeVal),
                                    "status" @: (_const . ptr $ int32_t)
                                ]
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for timer: " ++ show targetEntity
        genEmitterConnection (TPInterruptEmittter irq _) = do
            emitterId <- genDefineEmitterIdLabel irq
            irqMap <- gets interruptsMap
            irqVector <- case M.lookup irq irqMap of
                Just v -> return v
                Nothing -> throwError $ InternalError $ "Invalid interrupt emitter: " ++ show irq
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            (targetEntity, targetPort) <- case M.lookup irq (emitterTargets progArchitecture) of
                Just (entity, port, _) -> return (entity, port)
                -- | If the interrupt emitter is not connected, throw an error
                Nothing -> throwError $ InternalError $ "Interrupt emitter not connected: " ++ show irq
            -- | Now we have to check if the target entity is a task or a handler
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier classId _ _ _ _ _ _) -> do
                    let cls = handlerClasses progArchitecture M.! classId
                        (_, targetAction) = sinkPorts cls M.! targetPort
                    handlerId <- genDefineHandlerIdLabel identifier
                    return [
                            pre_cr $ var "connection" __termina_interrupt_connection_t,
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "type" @: enumFieldType
                                @= "__termina_emitter_connection_type__handler" @: enumFieldType,
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "handler" @: __termina_interrupt_handler_connection_t
                                @. "handler_object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "handler" @: __termina_interrupt_handler_connection_t
                                @. "handler_id" @: __termina_interrupt_action_t @= handlerId @: size_t,
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "handler" @: __termina_interrupt_handler_connection_t
                                @. "handler_action" @: __termina_interrupt_action_t @= classId <::> targetAction @: __termina_interrupt_action_t,
                            pre_cr $ __termina_interrupt__init @@ [
                                dec irqVector @: __termina_id_t,
                                emitterId @: __termina_id_t,
                                addrOf ("connection" @: __termina_interrupt_connection_t),
                                "status" @: (_const . ptr $ int32_t)
                            ]
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetEntity
                        sinkMsgQueueId <- genDefineSinkMsgQueueIdLabel targetEntity targetPort
                        return [
                                pre_cr $ var "connection" __termina_interrupt_connection_t,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "type" @: enumFieldType
                                    @= "__termina_emitter_connection_type__task" @: enumFieldType,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "task_msg_queue_id" @: __termina_id_t @= taskMsgQueueId @: __termina_id_t,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_msgq_id" @: __termina_id_t @= sinkMsgQueueId @:  __termina_id_t,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_port_id" @: __termina_id_t @= variantForPort @:  __termina_id_t,
                                pre_cr $ targetEntity @: typeDef tskCls @. targetPort @: __termina_id_t
                                    @= sinkMsgQueueId @: __termina_id_t,
                                pre_cr $ __termina_interrupt__init @@ [
                                    dec irqVector @: __termina_id_t,
                                    emitterId @: __termina_id_t,
                                    addrOf ("connection" @: __termina_interrupt_connection_t),
                                    "status" @: (_const . ptr $ int32_t)
                                ]
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for interrupt: " ++ show targetEntity
        genEmitterConnection _ = throwError $ InternalError "Invalid event emitter"

        genOSALEmitterInit :: TPEmitter a -> CGenerator CCompoundBlockItem
        genOSALEmitterInit timer@(TPPeriodicTimerEmitter {}) = do
            periodicTimerConnection <- genEmitterConnection timer
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ periodicTimerConnection
        genOSALEmitterInit irq@(TPInterruptEmittter {}) = do
            interruptConnection <- genEmitterConnection irq
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ interruptConnection
        genOSALEmitterInit _ = throwError $ InternalError "Invalid event emitter"

-- | Function __termina_app__init_mutexes. This function is called from the
-- Init task.  The function initializes the mutexes. The function is called AFTER
-- the execution of the init handler (if any) and before the initialization of the
-- resource locking mechanism.
genInitMutexes :: M.Map Identifier ResourceLock -> CGenerator CFileItem
genInitMutexes mutexes = do
    let mutexesList = M.toList mutexes
    initMutexes <- mapM genOSALMutexInit mutexesList
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_mutexes") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initMutexes

    where
        genOSALMutexInit :: (Identifier, ResourceLock) -> CGenerator CCompoundBlockItem
        genOSALMutexInit (identifier, ResourceLockMutex ceilingPriority) = do
            mutexId <- genDefineMutexIdLabel identifier
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ __termina_mutex__init @@ [
                            mutexId @: __termina_id_t,
                            "__termina_mutex_policy__ceiling" @: enumFieldType,
                            getCInteger ceilingPriority @: __termina_task_prio_t,
                            "status" @: (_const . ptr $ int32_t)
                        ]
                ]
        genOSALMutexInit _ = throwError $ InternalError "Invalid resource lock"

genChannelConnections :: TerminaProgArch a -> CGenerator CFileItem
genChannelConnections progArchitecture = do
    let targets = M.toList $ channelTargets progArchitecture
    channelConnections <- concat <$> traverse genChannelConnection targets
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_channel_connections") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : channelConnections

    where

        genChannelConnection :: (Identifier, (Identifier, Identifier, a)) -> CGenerator [CCompoundBlockItem]
        genChannelConnection (channelName, (targetName, targetPort, _)) = do
            let classId = taskClass $ tasks progArchitecture M.! targetName
            let (TPMsgQueue _ dty _ _ _) = channels progArchitecture M.! channelName
            taskId <- genDefineTaskIdLabel targetName
            taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetName
            channelMsgQueueId <- genDefineChannelMsgQueueIdLabel channelName
            portVariant <- genVariantForPort classId targetPort
            return $ pre_cr (channelName @: __termina_msg_queue_t @. "task_id" @: __termina_id_t
                        @= taskId @: __termina_id_t) :
                    no_cr (channelName @: __termina_msg_queue_t @. "task_msg_queue_id" @: __termina_id_t
                        @= taskMsgQueueId @: __termina_id_t) :
                    (case dty of
                        TUnit -> [
                            no_cr $ channelName @: __termina_msg_queue_t @. "channel_msg_queue_id" @: __termina_id_t
                                @= "__TERMINA_ID_INVALID" @: __termina_id_t
                            ]
                        _ -> [
                            no_cr $ channelName @: __termina_msg_queue_t @. "channel_msg_queue_id" @: __termina_id_t
                                @= channelMsgQueueId @: __termina_id_t
                            ]) ++ [
                    no_cr $ channelName @: __termina_msg_queue_t @. "port_id" @: __termina_id_t
                        @= portVariant @: __termina_id_t,
                    pre_cr $ targetName @: typeDef classId @. targetPort @: __termina_id_t
                        @= channelMsgQueueId @: __termina_id_t
                ]

genInitPools :: [TPPool SemanticAnn] -> CGenerator CFileItem
genInitPools pls = do
    initPools <- mapM genPoolInit pls
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_pools") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initPools

    where

        genPoolInit :: TPPool SemanticAnn -> CGenerator CCompoundBlockItem
        genPoolInit (TPPool identifier ts _ _ _) = do
            cTs <- genType noqual ts
            poolId <- genDefinePoolIdLabel identifier
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ identifier @: __termina_pool_t @. namefy "pool_id" @: __termina_id_t
                            @= poolId @: __termina_id_t,
                        pre_cr $ __termina_pool__init @@
                                [
                                    addrOf (identifier @: ptr __termina_pool_t),
                                    cast (ptr void) (poolMemoryArea identifier @: ptr uint8_t),
                                    _sizeOfExpr (poolMemoryArea identifier @: ptr uint8_t),
                                    _sizeOfType cTs,
                                    "status" @: (_const . ptr $ int32_t)
                                ]
                ]

genInitMessageQueues :: [OSALMsgQueue] -> CGenerator CFileItem
genInitMessageQueues queues = do
    initMsgQueues <- concat <$> traverse genOSALMsgQueueInit queues
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_msg_queues") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initMsgQueues

    where
        genOSALMsgQueueInit :: OSALMsgQueue -> CGenerator [CCompoundBlockItem]
        genOSALMsgQueueInit mq@(OSALTaskMsgQueue _ _ size) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genExpression size
            return [
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            _sizeOfType __termina_event_t,
                            cSize,
                            "status" @: (_const . ptr $ int32_t)
                        ]
                    ]
                ]
        -- | Message queues with unit type do not need a definition
        genOSALMsgQueueInit (OSALChannelMsgQueue _ TUnit _ _ _) = return []
        genOSALMsgQueueInit mq@(OSALChannelMsgQueue _ ty size _ _) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genExpression size
            cTs <- genType noqual ty
            return [
                    pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_msg_queue__init @@ [
                                msgQueueId @: __termina_id_t,
                                _sizeOfType cTs,
                                cSize,
                                "status" @: (_const . ptr $ int32_t)
                            ]
                    ]
                ]
        genOSALMsgQueueInit mq@(OSALSinkPortMsgQueue _ _ _ ty size) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genExpression size
            cTs <- genType noqual ty
            return [
                    pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_msg_queue__init @@ [
                                msgQueueId @: __termina_id_t,
                                _sizeOfType cTs,
                                cSize,
                                "status" @: (_const . ptr $ int32_t)
                            ]
                    ]
                ]

genEnableProtection :: TerminaProgArch SemanticAnn -> CGenerator CFileItem
genEnableProtection progArchitecture = do
    resourceProtections <- concat <$> forM (M.elems $ resources progArchitecture) genEnableProtectionResource
    poolProtections <- concat <$> forM (M.elems $ pools progArchitecture) genEnableProtectionPool
    return $ pre_cr $ static_function (namefy "termina_app" <::> "enable_protection") [] @-> void $
            trail_cr . block $
                resourceProtections ++ poolProtections

    where

        resourceLockingMap = genResourceLockings progArchitecture

        genEnableProtectionResource :: TPResource SemanticAnn -> CGenerator [CCompoundBlockItem]
        genEnableProtectionResource res = do
            let resourceId = resourceName res
                classId = resourceClass res
            case M.lookup resourceId resourceLockingMap of
                Just ResourceLockNone -> return []
                Just ResourceLockIrq -> return [
                        pre_cr $ resourceId @: typeDef classId @. resourceLockTypeField @: __termina_resource_lock_type_t @. "type" @: enumFieldType @= 
                            "__termina_resource_lock_type__irq" @: enumFieldType
                    ]
                Just (ResourceLockMutex _) -> do
                    mutexId <- genDefineMutexIdLabel resourceId
                    return [
                        pre_cr $ resourceId @: typeDef classId @. resourceLockTypeField @: __termina_resource_lock_type_t @. "type" @: enumFieldType @= 
                            "__termina_resource_lock_type__mutex" @: enumFieldType,
                        no_cr $ resourceId @: typeDef classId @. resourceLockTypeField @: __termina_resource_lock_type_t @. "mutex" @: __enum_termina_resource_lock_type__mutex_params_t @. "mutex_id" @: __termina_id_t
                            @= mutexId @: __termina_id_t
                        ]
                Nothing -> throwError $ InternalError $ "Resource " ++ show resourceId
                    ++ " not found in resource locking map" 
        
        genEnableProtectionPool :: TPPool SemanticAnn -> CGenerator [CCompoundBlockItem]
        genEnableProtectionPool (TPPool poolId _ _ _ _) = do
            case M.lookup poolId resourceLockingMap of
                Just ResourceLockNone -> return []
                Just ResourceLockIrq -> return [
                        pre_cr $ poolId @: __termina_pool_t @. resourceLockTypeField @: __termina_resource_lock_type_t @. "type" @: enumFieldType @= 
                            "__termina_resource_lock_type__irq" @: enumFieldType
                    ]
                Just (ResourceLockMutex _) -> do
                    mutexId <- genDefineMutexIdLabel poolId
                    return [
                        pre_cr $ poolId @: __termina_pool_t @. resourceLockTypeField @: __termina_resource_lock_type_t @. "type" @: enumFieldType @= 
                            "__termina_resource_lock_type__mutex" @: enumFieldType,
                        no_cr $ poolId @: __termina_pool_t @. resourceLockTypeField @: __termina_resource_lock_type_t @. "mutex" @: __enum_termina_resource_lock_type__mutex_params_t @. "mutex_id" @: __termina_id_t
                            @= mutexId @: __termina_id_t
                        ]
                Nothing -> throwError $ InternalError $ "Pool " ++ show poolId ++ " not found in resource locking map"

genInitalEventFunction :: TerminaProgArch a -> TPEmitter a -> CGenerator [CFileItem]
genInitalEventFunction progArchitecture (TPSystemInitEmitter systemInit _)= do
    (targetEntity, targetPort) <- case M.lookup systemInit (emitterTargets progArchitecture) of
        Just (entity, port, _) -> return (entity, port)
        -- | If the interrupt emitter is not connected, throw an error
        Nothing -> throwError $ InternalError $ "System init emitter not connected: " ++ show systemInit
    -- |  Now we have to check if the target entity is a task or a handler
    let event = pre_cr $ var "event" __termina_event_t
    eventFunctionBody <-
        case M.lookup targetEntity (handlers progArchitecture) of
            Just (TPHandler identifier classId _ _ _ _ _ _) -> genHandlerEventFunction identifier classId targetPort
            Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                Just (TPTask identifier classId _ _ _ _ _ _ _) -> genTaskEventFunction identifier classId targetPort
                Nothing -> throwError $ InternalError $ "Invalid connection for system init: " ++ show targetEntity
    return [pre_cr $ static_function (namefy "termina_app" <::> "initial_event") [] @-> void $
            trail_cr . block $ event : eventFunctionBody ]

    where

        

        genHandlerEventFunction :: Identifier -> Identifier -> Identifier -> CGenerator [CCompoundBlockItem]
        genHandlerEventFunction identifier classId targetPort = do
            let cls = handlerClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort
                classIdType = typeDef classId
            handlerId <- genDefineHandlerIdLabel identifier
            emitterId <- genDefineEmitterIdLabel systemInit
            return [
                    no_cr $ "event" @: __termina_event_t @. "emitter_id" @: __termina_id_t @= emitterId @: __termina_id_t,
                    no_cr $ "event" @: __termina_event_t @. "owner" @: __termina_active_entity_t @. "type" @: enumFieldType @= "__termina_active_entity__handler" @: enumFieldType,
                    no_cr $ "event" @: __termina_event_t @. "owner" @: __termina_active_entity_t @. "handler" @: __enum_termina_active_entity__handler_params_t
                        @. "handler_id" @: __termina_id_t @= handlerId @: __termina_id_t,
                    no_cr $ "event" @: __termina_event_t @. "port_id" @: __termina_id_t @= dec 0 @: __termina_id_t,
                    pre_cr $ var "current" _TimeVal,
                    no_cr $ _SystemEntry__clock_get_uptime @@ [
                        addrOf ("event" @: __termina_event_t),
                        addrOf ("current" @: _TimeVal)],
                    -- classId * self = &identifier;
                    pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                    -- __status_int32_t status;
                    pre_cr $ var "status" __status_int32_t,
                    -- status.__variant = Success;
                    no_cr $ ("status" @: __status_int32_t) @. variant @: enumFieldType @= "Success" @: enumFieldType,
                    -- status = classFunctionName(self, current);
                    pre_cr $ "status" @: __status_int32_t @=
                        timer_handler classId targetAction @@
                            [
                                addrOf ("event" @: __termina_event_t),
                                "self" @: ptr classIdType,
                                "current" @: _TimeVal
                            ],
                    -- if (status.__variant != Success)
                    pre_cr $ _if (
                            (("status" @: __status_int32_t) @. variant) @: enumFieldType @!= "Success" @: enumFieldType)
                        $ block [
                            -- __termina_exec__reboot();
                            no_cr $ __termina_exec__reboot @@ []
                        ],
                    pre_cr $ _return Nothing
                ]

        genTaskEventFunction :: Identifier -> Identifier -> Identifier -> CGenerator [CCompoundBlockItem]
        genTaskEventFunction identifier classId targetPort = do
            let cls = taskClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort
                classIdType = typeDef classId
            taskId <- genDefineTaskIdLabel identifier
            portId <- genVariantForPort classId targetPort
            emitterId <- genDefineEmitterIdLabel systemInit
            return [
                    no_cr $ "event" @: __termina_event_t @. "emitter_id" @: __termina_id_t @= emitterId @: __termina_id_t,
                    no_cr $ "event" @: __termina_event_t @. "owner" @: __termina_active_entity_t @. "type" @: enumFieldType @= "__termina_active_entity__task" @: enumFieldType,
                    no_cr $ "event" @: __termina_event_t @. "owner" @: __termina_active_entity_t @. "task" @: __enum_termina_active_entity__task_params_t
                        @. "task_id" @: __termina_id_t @= taskId @: __termina_id_t,
                    no_cr $ "event" @: __termina_event_t @. "port_id" @: __termina_id_t @= portId @: __termina_id_t,
                    pre_cr $ var "current" _TimeVal,
                    no_cr $ _SystemEntry__clock_get_uptime @@ [
                        addrOf ("event" @: __termina_event_t), 
                        addrOf ("current" @: _TimeVal)],
                    -- classId * self = &identifier;
                    pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                    -- __status_int32_t status;
                    pre_cr $ var "status" __status_int32_t,
                    -- status.__variant = Success;
                    no_cr $ ("status" @: __status_int32_t) @. variant @: enumFieldType @= "Success" @: enumFieldType,
                    -- status = classFunctionName(self, current);
                    pre_cr $ "status" @: __status_int32_t @=
                        timer_handler classId targetAction @@
                            [
                                "self" @: ptr classIdType,
                                deref ("current" @: (_const . ptr $ _TimeVal))
                            ],
                    -- if (status.__variant != Success)
                    pre_cr $ _if (
                            (("status" @: __status_int32_t) @. variant) @: enumFieldType @!= "Success" @: enumFieldType)
                        $ block [
                            -- __termina_exec__reboot();
                            no_cr $ __termina_exec__reboot @@ []
                        ],
                    pre_cr $ _return Nothing
                ]
genInitalEventFunction _ _ = throwError $ InternalError "Invalid event emitter"

genAppInit :: TerminaProgArch a -> CGenerator CFileItem
genAppInit progArchitecture = do
    return $ pre_cr $ function (namefy "termina_app" <::> "init") [
            "status" @: (_const . ptr $ int32_t)
        ] @-> void $
        trail_cr . block $
            [
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t),
                -- | External call to __termina_app__init_globals().
                -- This function cannot fail, so we do not check the status.
                pre_cr $ __termina_app__init_globals @@ [],
                pre_cr $ __termina_app__init_msg_queues @@ ["status" @: (_const . ptr $ int32_t)]
            ] ++
            [
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__init_channel_connections @@ ["status" @: (_const . ptr $ int32_t)]
                        ]
            ] ++
            [
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__init_pools @@ ["status" @: (_const . ptr $ int32_t)]
                        ]
            ] ++ ([pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__initial_event @@ ["status" @: (_const . ptr $ int32_t)]
                        ] | any (\case { TPSystemInitEmitter {} -> True; _ -> False }) (emitters progArchitecture)]) ++
            [
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__init_mutexes @@ ["status" @: (_const . ptr $ int32_t)]
                        ]
            ] ++
            [
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__enable_protection @@ []
                        ],
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__init_emitters @@ ["status" @: (_const . ptr $ int32_t)]
                        ],
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__init_handlers @@ ["status" @: (_const . ptr $ int32_t)]
                        ],
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                        $ trail_cr . block $ [
                            pre_cr $ __termina_app__init_tasks @@ ["status" @: (_const . ptr $ int32_t)]
                        ]
            ]

genMainFile :: QualifiedName
    -> TerminaProgArch SemanticAnn
    -> CGenerator CFile
genMainFile mName progArchitecture = do
    let includeTermina = CPPDirective (CPPInclude True "termina.h") (internalAnn (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl (CEDFunction void (namefy $ "termina_app" <::> "init_globals") []) (internalAnn (CDeclarationAnn True))
    sinkPortMessageQueues <- getSinkPortMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture
    taskMessageQueues <- getTasksMessageQueues progArchitecture (sinkPortMessageQueues ++ channelMessageQueues)

    let mutexes = 
            M.filter (\case{ResourceLockMutex {} -> True; _ -> False}) (genResourceLockings progArchitecture)

    cPoolMemoryAreas <- genPoolMemoryAreas (M.elems $ pools progArchitecture)

    initTasks <- genInitTasks progArchitecture
    initHandlers <- genInitHandlers progArchitecture
    initEmitters <- genInitEmitters progArchitecture
    initPools <- genInitPools (M.elems $ pools progArchitecture)
    initMutexes <- genInitMutexes mutexes
    initMessageQueues <- genInitMessageQueues (taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues)

    channelConnections <- genChannelConnections progArchitecture
    enableProtection <- genEnableProtection progArchitecture

    initialEventFunction <- (case find (\case { TPSystemInitEmitter {} -> True; _ -> False }) (emitters progArchitecture) of
        Just systemInitEmitter@(TPSystemInitEmitter {}) ->
            genInitalEventFunction progArchitecture systemInitEmitter
        _ -> return [])

    appInit <- genAppInit progArchitecture

    return $ CSourceFile mName $ [
            -- #include <termina.h>
            includeTermina
        ] ++ includes
        ++ [
            externInitGlobals
        ]
        ++ cPoolMemoryAreas
        ++ [initTasks, initHandlers, initEmitters, initMutexes, initPools, initMessageQueues,
            enableProtection, channelConnections] ++ initialEventFunction
        ++ [appInit]

    where
        -- | List of modules that must be included
        incs = getGlobDeclModules progArchitecture
        -- | List of include directives
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs

runGenMainFile ::
    TerminaConfig
    -> M.Map Identifier Integer
    -> QualifiedName
    -> TerminaProgArch SemanticAnn
    -> Either CGeneratorError CFile
runGenMainFile config irqMap mainFilePath progArchitecture =
    case runState (runExceptT (genMainFile mainFilePath progArchitecture))
        (CGeneratorEnv mainFilePath S.empty emptyMonadicTypes config irqMap) of
    (Left err, _) -> Left err
    (Right cFile, _) -> Right cFile