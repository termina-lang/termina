{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Glue where

import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.CodeGen.Common
import Modules.Modules (QualifiedName)
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
                    return [
                            pre_cr $ var "connection" __termina_periodic_timer_connection_t,
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "type" @: enumFieldType
                                @= "__TerminaEmitterConnectionType__Handler" @: enumFieldType,
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "handler" @: __termina_periodic_timer_handler_connection_t
                                @. "handler_object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "handler" @: __termina_periodic_timer_handler_connection_t
                                @. "handler_action" @: __termina_periodic_timer_action_t @= classId <::> targetAction @: __termina_periodic_timer_action_t,
                            pre_cr $ __termina_periodic_timer__init @@ [
                                timerId @: __termina_id_t,
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
                                    @= "__TerminaEmitterConnectionType__Task" @: enumFieldType,
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
                                    addrOf ("connection" @: __termina_periodic_timer_connection_t),
                                    addrOf (timer @: __termina_periodic_timer_t @. "period" @: _TimeVal),
                                    "status" @: (_const . ptr $ int32_t)
                                ]
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for timer: " ++ show targetEntity
        genEmitterConnection (TPInterruptEmittter irq _) = do
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
                    return [
                            pre_cr $ var "connection" __termina_interrupt_connection_t,
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "type" @: enumFieldType
                                @= "__TerminaEmitterConnectionType__Handler" @: enumFieldType,
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "handler" @: __termina_interrupt_handler_connection_t
                                @. "handler_object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "handler" @: __termina_interrupt_handler_connection_t
                                @. "handler_action" @: __termina_interrupt_action_t @= classId <::> targetAction @: __termina_interrupt_action_t,
                            pre_cr $ __termina_interrupt__init @@ [
                                dec irqVector @: __termina_id_t,
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
                                    @= "__TerminaEmitterConnectionType__Task" @: enumFieldType,
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
genInitMutexes :: TerminaProgArch SemanticAnn -> M.Map Identifier OSALResourceLock -> CGenerator CFileItem
genInitMutexes progArchitecture mutexes = do
    let mutexesList = M.toList mutexes
    initMutexes <- mapM genOSALMutexInit mutexesList
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_mutexes") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initMutexes

    where
        genOSALMutexInit :: (Identifier, OSALResourceLock) -> CGenerator CCompoundBlockItem
        genOSALMutexInit (identifier, OSALResourceLockMutex ceilingPriority) = do
            mutexId <- genDefineMutexIdLabel identifier
            resourceCls <- case M.lookup identifier (resources progArchitecture) of
                Just (TPResource _ cls _ _ _) -> return cls
                Nothing -> case M.lookup identifier (pools progArchitecture) of
                    Just _ -> return pool
                    _ -> throwError $ InternalError $ "Invalid resource: " ++ show identifier
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ identifier @: typeDef resourceCls @. namefy "mutex_id" @: __termina_id_t
                            @= mutexId @: __termina_id_t,
                        pre_cr $ __termina_mutex__init @@ [
                            mutexId @: __termina_id_t,
                            "__TerminaMutexPolicy__Ceiling" @: enumFieldType,
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
            taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetName
            channelMsgQueueId <- genDefineChannelMsgQueueIdLabel channelName
            portVariant <- genVariantForPort classId targetPort
            return [
                    pre_cr $ channelName @: __termina_msg_queue_t @. "task_msg_queue_id" @: __termina_id_t
                        @= taskMsgQueueId @: __termina_id_t,
                    no_cr $ channelName @: __termina_msg_queue_t @. "channel_msg_queue_id" @: __termina_id_t
                        @= channelMsgQueueId @: __termina_id_t,
                    no_cr $ channelName @: __termina_msg_queue_t @. "port_id" @: __termina_id_t
                        @= portVariant @: __termina_id_t
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
    initMsgQueues <- mapM genOSALMsgQueueInit queues
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_msg_queues") ["status" @: (_const . ptr $ int32_t)] @-> void $
            trail_cr . block $
                pre_cr (deref ("status" @: (_const . ptr $ int32_t)) @= dec 0 @: int32_t)
                : initMsgQueues

    where
        genOSALMsgQueueInit :: OSALMsgQueue -> CGenerator CCompoundBlockItem
        genOSALMsgQueueInit mq@(OSALTaskMsgQueue _ _ size) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genExpression size
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            _sizeOfType __termina_id_t,
                            cSize,
                            "status" @: (_const . ptr $ int32_t)
                        ]
                ]
        genOSALMsgQueueInit mq@(OSALChannelMsgQueue _ ty size _ _) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genExpression size
            cTs <- genType noqual ty
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            _sizeOfType cTs,
                            cSize,
                            "status" @: (_const . ptr $ int32_t)
                        ]
                ]
        genOSALMsgQueueInit mq@(OSALSinkPortMsgQueue _ _ _ _ size) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genExpression size
            return $
                pre_cr $ _if (dec 0 @: int32_t @== deref ("status" @: (_const . ptr $ int32_t)))
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            _sizeOfType __termina_id_t,
                            cSize,
                            "status" @: (_const . ptr $ int32_t)
                        ]
                ]

genEnableProtection :: TerminaProgArch SemanticAnn -> M.Map Identifier OSALResourceLock -> CGenerator CFileItem
genEnableProtection progArchitecture resLockingMap = do
    -- Get the list of tasks and the list of handlers
    let progTasks = M.elems $ tasks progArchitecture
        progHandlers = M.elems $ handlers progArchitecture
    taskProtections <- concat <$> forM progTasks genEnableProtectionTask
    handlerProtections <- concat <$> forM progHandlers genEnableProtectionHandler
    return $ pre_cr $ static_function (namefy "termina_app" <::> "enable_protection") [] @-> void $
            trail_cr . block $
                taskProtections ++ handlerProtections

    where

        genEnableResourceMutex ::
            Identifier -- Task Identifier
            -> Identifier -- Class Identifier
            -> Identifier -- Port Identifier
            -> SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genEnableResourceMutex taskId classId portId ann = do
            let apConnectionAnn = unboxConnectionAnn ann
            case apConnectionAnn of
                APPoolConnTy {} -> do
                    let allocFunctionType = CTFunction void [void_ptr, ptr __option_box_t]
                        freeFunctionType = CTFunction void [void_ptr, __termina_box_t]
                    allocLock <- procedureMutexLock (namefy "termina_pool" <::> "alloc")
                    freeLock <- procedureMutexLock (namefy "termina_pool" <::> "free")
                    return [
                            pre_cr $ taskId @: typeDef classId @. portId @: __termina_allocator_t @. "alloc" @: ptr allocFunctionType
                                    @= allocLock @: allocFunctionType,
                            no_cr $ taskId @: typeDef classId @. portId @: __termina_allocator_t @. "free" @: ptr freeFunctionType
                                    @= freeLock @: freeFunctionType
                        ]
                APConnTy (TInterface _ iface)
                        (TGlobal ResourceClass resource) pss -> do
                    case [p | p <- pss, not (isUnprotected p) ] of
                        [] -> return []
                        -- Generate first procedure with a preceding new line
                        [ProcedureSeman pr prtys _modifiers] -> do
                            clsFunctionName <- genClassFunctionName resource pr
                            procFunctionType <- genFunctionType TUnit prtys
                            clsFunctionNameLock <- procedureMutexLock clsFunctionName
                            return [
                                pre_cr (taskId @: typeDef classId @. portId @: typeDef iface @. pr @: procFunctionType
                                    @= clsFunctionNameLock @: procFunctionType)
                                ]
                        ((ProcedureSeman pr prtys _modifiers) : pxs) -> do
                            -- Generate last procedures without new lines
                            procs <- forM pxs (\(ProcedureSeman procedureId ptys _modifiers) -> do
                                clsFunctionName <- genClassFunctionName resource procedureId
                                procFunctionType <- genFunctionType TUnit ptys
                                clsFunctionNameLock <- procedureMutexLock clsFunctionName
                                return $
                                    no_cr $ taskId @: typeDef classId @. portId @: typeDef iface @. procedureId @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType
                                )
                            -- Generate first procedure with a preceding new line
                            clsFunctionName <- genClassFunctionName resource pr
                            procFunctionType <- genFunctionType TUnit prtys
                            clsFunctionNameLock <- procedureMutexLock clsFunctionName
                            return $ pre_cr (taskId @: typeDef classId @. portId @: typeDef iface @. pr @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType) : procs
                _ -> return []

        genEnableResourceTaskLock ::
            Identifier -- Task Identifier
            -> Identifier -- Class Identifier
            -> Identifier -- Port Identifier
            -> SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genEnableResourceTaskLock taskId classId portId ann = do
            let apConnectionAnn = unboxConnectionAnn ann
            case apConnectionAnn of
                APPoolConnTy {} -> do
                    let allocFunctionType = CTFunction void [void_ptr, ptr __option_box_t]
                        freeFunctionType = CTFunction void [void_ptr, __termina_box_t]
                    allocLock <- procedureTaskLock (namefy "termina_pool" <::> "alloc")
                    freeLock <- procedureTaskLock (namefy "termina_pool" <::> "free")
                    return [
                            pre_cr $ taskId @: typeDef classId @. portId @: __termina_allocator_t @. "alloc" @: ptr allocFunctionType
                                    @= allocLock @: allocFunctionType,
                            no_cr $ taskId @: typeDef classId @. portId @: __termina_allocator_t @. "free" @: ptr freeFunctionType
                                    @= freeLock @: freeFunctionType
                        ]
                APConnTy (TInterface _ iface)
                        (TGlobal ResourceClass resource) pss -> do
                    case [p | p <- pss, not (isUnprotected p) ] of
                        [] -> return []
                        -- Generate first procedure with a preceding new line
                        [ProcedureSeman pr prtys _modifiers] -> do
                            clsFunctionName <- genClassFunctionName resource pr
                            procFunctionType <- genFunctionType TUnit prtys
                            clsFunctionNameLock <- procedureTaskLock clsFunctionName
                            return [ pre_cr (taskId @: typeDef classId @. portId @: typeDef iface @. pr @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType)
                                ]
                        ((ProcedureSeman pr prtys _modifiers) : pxs) -> do
                            -- Generate last procedures without new lines
                            procs <- forM pxs (\(ProcedureSeman procedureId ptys _modifiers) -> do
                                clsFunctionName <- genClassFunctionName resource procedureId
                                procFunctionType <- genFunctionType TUnit ptys
                                clsFunctionNameLock <- procedureTaskLock clsFunctionName
                                return $
                                    no_cr $ taskId @: typeDef classId @. portId @: typeDef iface @. procedureId @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType
                                )
                            -- Generate first procedure with a preceding new line
                            clsFunctionName <- genClassFunctionName resource pr
                            procFunctionType <- genFunctionType TUnit prtys
                            clsFunctionNameLock <- procedureTaskLock clsFunctionName
                            return $ pre_cr (taskId @: typeDef classId @. portId @: typeDef iface @. pr @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType) : procs
                _ -> return []

        genEnableResourceEventLock ::
            Identifier -- Handler Identifier
            -> Identifier -- Class Identifier
            -> Identifier -- Port Identifier
            -> SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genEnableResourceEventLock taskId classId portId ann = do
            let apConnectionAnn = unboxConnectionAnn ann
            case apConnectionAnn of
                APPoolConnTy {} -> do
                    let allocFunctionType = CTFunction void [void_ptr, ptr __option_box_t]
                        freeFunctionType = CTFunction void [void_ptr, __termina_box_t]
                    allocLock <- procedureEventLock (namefy "termina_pool" <::> "alloc")
                    freeLock <- procedureEventLock (namefy "termina_pool" <::> "free")
                    return [
                            pre_cr $ taskId @: typeDef classId @. portId @: __termina_allocator_t @. "alloc" @: ptr allocFunctionType
                                    @= allocLock @: allocFunctionType,
                            no_cr $ taskId @: typeDef classId @. portId @: __termina_allocator_t @. "free" @: ptr freeFunctionType
                                    @= freeLock @: freeFunctionType
                        ]
                APConnTy (TInterface _ iface)
                        (TGlobal ResourceClass resource) pss -> do
                    case [p | p <- pss, not (isUnprotected p) ] of
                        [] -> return []
                        [ProcedureSeman pr prtys _modifiers] -> do
                            clsFunctionName <- genClassFunctionName resource pr
                            procFunctionType <- genFunctionType TUnit prtys
                            clsFunctionNameLock <- procedureEventLock clsFunctionName
                            return [ pre_cr (taskId @: typeDef classId @. portId @: typeDef iface @. pr @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType)
                                ]
                        ((ProcedureSeman pr prtys _modifiers) : pxs) -> do
                            -- Generate last procedures without new lines
                            procs <- forM pxs (\(ProcedureSeman procedureId ptys _modifiers) -> do
                                clsFunctionName <- genClassFunctionName resource procedureId
                                procFunctionType <- genFunctionType TUnit ptys
                                clsFunctionNameLock <- procedureEventLock clsFunctionName
                                return $
                                    no_cr $ taskId @: typeDef classId @. portId @: typeDef iface @. procedureId @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType
                                )
                            -- Generate first procedure with a preceding new line
                            clsFunctionName <- genClassFunctionName resource pr
                            procFunctionType <- genFunctionType TUnit prtys
                            clsFunctionNameLock <- procedureEventLock clsFunctionName
                            return $ pre_cr (taskId @: typeDef classId @. portId @: typeDef iface @. pr @: procFunctionType
                                        @= clsFunctionNameLock @: procFunctionType) : procs
                            
                _ -> return []

        genEnableProtectionHandler :: TPHandler SemanticAnn -> CGenerator [CCompoundBlockItem]
        genEnableProtectionHandler hdlr = do
            let taskId = handlerName hdlr
                classId = handlerClass hdlr
                apConnections = handlerAPConnections hdlr
            concat <$> forM (M.toList apConnections) (\(portName, (targetResource, ann)) ->
                case resLockingMap M.! targetResource of
                    OSALResourceLockNone -> return []
                    OSALResourceLockIrq -> genEnableResourceEventLock taskId classId portName ann
                    OSALResourceLockMutex _ -> throwError $ InternalError $ "Handler " ++ show taskId
                            ++ " cannot connect to resource " ++ show targetResource ++ " with mutex locking. This is not allowed in Termina.")

        genEnableProtectionTask :: TPTask SemanticAnn -> CGenerator [CCompoundBlockItem]
        genEnableProtectionTask tsk = do
            let taskId = taskName tsk
                classId = taskClass tsk
                apConnections = taskAPConnections tsk
            concat <$> forM (M.toList apConnections) (\(portName, (targetResource, ann)) ->
                case resLockingMap M.! targetResource of
                    OSALResourceLockNone -> return []
                    OSALResourceLockIrq -> genEnableResourceTaskLock taskId classId portName ann
                    OSALResourceLockMutex _ ->
                        genEnableResourceMutex taskId classId portName ann)

genInitalEventFunction :: TerminaProgArch a -> TPEmitter a -> CGenerator [CFileItem]
genInitalEventFunction progArchitecture (TPSystemInitEmitter systemInit _)= do
    (targetEntity, targetPort) <- case M.lookup systemInit (emitterTargets progArchitecture) of
        Just (entity, port, _) -> return (entity, port)
        -- | If the interrupt emitter is not connected, throw an error
        Nothing -> throwError $ InternalError $ "System init emitter not connected: " ++ show systemInit
    -- |  Now we have to check if the target entity is a task or a handler
    eventFunctionBody <-
        case M.lookup targetEntity (handlers progArchitecture) of
            Just (TPHandler identifier classId _ _ _ _ _ _) -> genHandlerEventFunction identifier classId targetPort
            Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                Just (TPTask identifier classId _ _ _ _ _ _ _) -> genTaskEventFunction identifier classId targetPort
                Nothing -> throwError $ InternalError $ "Invalid connection for system init: " ++ show targetEntity
    return [pre_cr $ static_function (namefy "termina_app" <::> "initial_event") [] @-> void $
            trail_cr . block $ eventFunctionBody ]

    where

        genHandlerEventFunction :: Identifier -> Identifier -> Identifier -> CGenerator [CCompoundBlockItem]
        genHandlerEventFunction identifier classId targetPort = do
            let cls = handlerClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort
                classIdType = typeDef classId
            return [
                    pre_cr $ var "current" _TimeVal,
                    no_cr $ _SystemEntry__clock_get_uptime @@ [addrOf ("current" @: _TimeVal)],
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
                                "current" @: _TimeVal
                            ],
                    -- if (status.__variant != Success)
                    pre_cr $ _if (
                            (("status" @: __status_int32_t) @. variant) @: enumFieldType @!= "Success" @: enumFieldType)
                        $ block [
                            -- __termina_exec__shutdown();
                            no_cr $ __termina_exec__shutdown @@ []
                        ],
                    pre_cr $ _return Nothing
                ]

        genTaskEventFunction :: Identifier -> Identifier -> Identifier -> CGenerator [CCompoundBlockItem]
        genTaskEventFunction identifier classId targetPort = do
            let cls = taskClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort
                classIdType = typeDef classId
            return [
                    pre_cr $ var "current" _TimeVal,
                    no_cr $ _SystemEntry__clock_get_uptime @@ [addrOf ("current" @: _TimeVal)],
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
                            -- __termina_exec__shutdown();
                            no_cr $ __termina_exec__shutdown @@ []
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
                            pre_cr $ __termina_app__init_tasks @@ ["status" @: (_const . ptr $ int32_t)]
                        ]
            ]

genMainFile :: QualifiedName
    -> TerminaProgArch SemanticAnn
    -> CGenerator CFile
genMainFile mName progArchitecture = do
    let includeTermina = CPPDirective (CPPInclude True "termina.h") (internalAnn (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl (CEDFunction void (namefy $ "termina_app" <::> "init_globals") []) (internalAnn (CDeclarationAnn True))
    taskMessageQueues <- getTasksMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture

    resLockingMap <- genResLockingMap progArchitecture dependenciesMap
    let mutexes = M.filter (\case{ OSALResourceLockMutex {} -> True; _ -> False }) resLockingMap

    cPoolMemoryAreas <- genPoolMemoryAreas (M.elems $ pools progArchitecture)
    cAtomicDeclarations <- genAtomicDeclarations (M.elems $ atomics progArchitecture)
    cAtomicArrayDeclarations <- genAtomicArrayDeclarations (M.elems $ atomicArrays progArchitecture)

    initTasks <- genInitTasks progArchitecture
    initEmitters <- genInitEmitters progArchitecture
    initPools <- genInitPools (M.elems $ pools progArchitecture)
    initMutexes <- genInitMutexes progArchitecture mutexes
    initMessageQueues <- genInitMessageQueues (taskMessageQueues ++ channelMessageQueues)

    channelConnections <- genChannelConnections progArchitecture
    enableProtection <- genEnableProtection progArchitecture resLockingMap

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
        ++ cAtomicDeclarations ++ cAtomicArrayDeclarations
        ++ cPoolMemoryAreas
        ++ [initTasks, initEmitters, initMutexes, initPools, initMessageQueues,
            enableProtection, channelConnections] ++ initialEventFunction
        ++ [appInit]

    where
        -- | List of modules that must be included
        incs = getGlobDeclModules progArchitecture
        -- | List of include directives
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs

        dependenciesMap = getResDependencies progArchitecture

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