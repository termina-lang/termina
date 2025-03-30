{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Glue where

import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.CodeGen.Common
import Control.Monad.Reader
import Modules.Modules (QualifiedName)
import Semantic.Types
import ControlFlow.Architecture.Types
import Configuration.Configuration
import Generator.LanguageC.Embedded
import ControlFlow.Architecture.Utils
import System.FilePath
import Generator.CodeGen.SystemCall
import Control.Monad.Except
import Generator.CodeGen.Application.Utils
import Generator.CodeGen.Types
import Semantic.AST
import Semantic.Utils
import Control.Monad (forM)


genInitTasks :: TerminaProgArch a -> CGenerator CFileItem
genInitTasks progArchitecture = do
    let progTasks = M.elems $ tasks progArchitecture
    initTasks <- mapM genOSALTaskInit progTasks
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_tasks") ["status" @: (_const . ptr $ _Status)] @-> void $
            trail_cr . block $
                pre_cr ((("status" @: _Status) @. variant @: enumFieldType) @= "Status__Success" @: enumFieldType)
                : initTasks

    where
        genOSALTaskInit :: TPTask a -> CGenerator CCompoundBlockItem
        genOSALTaskInit tsk = do
            taskId <- genDefineTaskIdLabel (taskName tsk)
            let tskName = taskName tsk
                classId = taskClass tsk
                taskPrio = getCInteger . getPriority $ tsk
                taskStackSize = getCInteger . getStackSize $ tsk
            cTaskFunctionName <- taskFunctionName classId
            return $
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ [
                        pre_cr $ __termina_task__init @@ [
                            taskId @: __termina_id_t,
                            taskPrio @: __termina_task_prio_t,
                            taskStackSize @: size_t,
                            cTaskFunctionName @: __termina_task_entry_t,
                            addrOf (tskName @: typeDef classId),
                            "status" @: (_const . ptr $ _Status)
                        ]
                ]

-- | Function __termina_app__install_emitters. This function is called from the
-- Init task.  The function installs the ISRs and the periodic timers. The
-- function is called AFTER the initialization of the tasks and handlers.
genInitEmitters :: TerminaProgArch a -> CGenerator CFileItem
genInitEmitters progArchitecture = do
    let progEmitters = M.elems $ emitters progArchitecture
    initEmitter <- mapM genOSALEmitterInit $ filter (\case { TPSystemInitEmitter {} -> False; _ -> True }) progEmitters
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_emitters") ["status" @: (_const . ptr $ _Status)] @-> void $
            trail_cr . block $
                pre_cr ((("status" @: _Status) @. variant @: enumFieldType) @= "Status__Success" @: enumFieldType)
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
                                @. "object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "handler" @: __termina_periodic_timer_handler_connection_t
                                @. "handler_action" @: __termina_periodic_timer_action_t @= classId <::> targetAction @: __termina_periodic_timer_action_t,
                            pre_cr $ __termina_periodic_timer__init @@ [
                                timerId @: __termina_id_t,
                                addrOf ("connection" @: __termina_periodic_timer_connection_t),
                                addrOf (timer @: __termina_periodic_timer_t @. "period" @: _TimeVal),
                                "status" @: (_const . ptr $ _Status)
                            ]
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        return [
                                pre_cr $ var "connection" __termina_periodic_timer_connection_t,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "type" @: enumFieldType
                                    @= "__TerminaEmitterConnectionType__Task" @: enumFieldType,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "task_msgq_id" @: __termina_id_t @= namefy targetEntity <::> "msg_queue_id" @: __termina_id_t,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_msgq_id" @: __termina_id_t @= namefy targetEntity <::> targetPort  @:  __termina_id_t,
                                no_cr $ "connection" @: __termina_periodic_timer_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_port_id" @: __termina_id_t @= variantForPort @:  __termina_id_t,
                                pre_cr $ __termina_periodic_timer__init @@ [
                                    timerId @: __termina_id_t,
                                    addrOf ("connection" @: __termina_periodic_timer_connection_t),
                                    addrOf (timer @: __termina_periodic_timer_t @. "period" @: _TimeVal),
                                    "status" @: (_const . ptr $ _Status)
                                ]
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for timer: " ++ show targetEntity
        genEmitterConnection (TPInterruptEmittter irq _) = do
            irqMap <- asks interruptsMap
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
                                @. "object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "connection" @: __termina_interrupt_connection_t @. "handler" @: __termina_interrupt_handler_connection_t
                                @. "handler_action" @: __termina_interrupt_action_t @= classId <::> targetAction @: __termina_interrupt_action_t,
                            pre_cr $ __termina_interrupt__init @@ [
                                dec irqVector @: __termina_id_t,
                                addrOf ("connection" @: __termina_interrupt_connection_t),
                                "status" @: (_const . ptr $ _Status)
                            ]
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        return [
                                pre_cr $ var "connection" __termina_interrupt_connection_t,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "type" @: enumFieldType
                                    @= "__TerminaEmitterConnectionType__Task" @: enumFieldType,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "task_msgq_id" @: __termina_id_t @= namefy targetEntity <::> "msg_queue_id" @: __termina_id_t,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_msgq_id" @: __termina_id_t @= namefy targetEntity <::> targetPort  @:  __termina_id_t,
                                no_cr $ "connection" @: __termina_interrupt_connection_t @. "task" @: __termina_emitter_task_connection_t
                                    @. "sink_port_id" @: __termina_id_t @= variantForPort @:  __termina_id_t,
                                pre_cr $ __termina_interrupt__init @@ [
                                    dec irqVector @: __termina_id_t,
                                    addrOf ("connection" @: __termina_interrupt_connection_t),
                                    "status" @: (_const . ptr $ _Status)
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
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ periodicTimerConnection
        genOSALEmitterInit irq@(TPInterruptEmittter {}) = do
            interruptConnection <- genEmitterConnection irq
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            return $
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ interruptConnection
        genOSALEmitterInit _ = throwError $ InternalError "Invalid event emitter"

-- | Function __termina_app__init_mutexes. This function is called from the
-- Init task.  The function initializes the mutexes. The function is called AFTER
-- the execution of the init handler (if any) and before the initialization of the
-- resource locking mechanism.
genInitMutexes :: M.Map Identifier OSALResourceLock -> CGenerator CFileItem
genInitMutexes mutexes = do
    let mutexesList = M.toList mutexes
    initMutexes <- mapM genOSALMutexInit mutexesList
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_mutexes") ["status" @: (_const . ptr $ _Status)] @-> void $
            trail_cr . block $
                pre_cr ((("status" @: _Status) @. variant @: enumFieldType) @= "Status__Success" @: enumFieldType)
                : initMutexes

    where
        genOSALMutexInit :: (Identifier, OSALResourceLock) -> CGenerator CCompoundBlockItem
        genOSALMutexInit (identifier, OSALResourceLockMutex ceilingPriority) = do
            mutexId <- genDefineMutexIdLabel identifier
            return $
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ [
                        pre_cr $ __termina_mutex__init @@ [
                            mutexId @: __termina_id_t,
                            "__TerminaMutexPolicy__Ceiling" @: enumFieldType,
                            getCInteger ceilingPriority @: __termina_task_prio_t,
                            "status" @: (_const . ptr $ _Status)
                        ]
                ]
        genOSALMutexInit _ = throwError $ InternalError "Invalid resource lock"

genInitMessageQueues :: [OSALMsgQueue] -> CGenerator CFileItem
genInitMessageQueues queues = do
    initMsgQueues <- mapM genOSALMsgQueueInit queues
    return $ pre_cr $ static_function (namefy "termina_app" <::> "init_msg_queues") ["status" @: (_const . ptr $ _Status)] @-> void $
            trail_cr . block $
                pre_cr ((("status" @: _Status) @. variant @: enumFieldType) @= "Status__Success" @: enumFieldType)
                : initMsgQueues

    where
        genOSALMsgQueueInit :: OSALMsgQueue -> CGenerator CCompoundBlockItem
        genOSALMsgQueueInit mq@(OSALTaskMsgQueue _ _ size) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genArraySize size
            return $
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            cSize,
                            _sizeOfType __termina_id_t,
                            "status" @: (_const . ptr $ _Status)
                        ]
                ]
        genOSALMsgQueueInit mq@(OSALChannelMsgQueue _ ty size _ _) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genArraySize size
            cTs <- genType noqual ty
            return $
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            cSize,
                            _sizeOfType cTs,
                            "status" @: (_const . ptr $ _Status)
                        ]
                ]
        genOSALMsgQueueInit mq@(OSALSinkPortMsgQueue _ _ _ _ size) = do
            msgQueueId <- genDefineMsgQueueIdLabel mq
            cSize <- genArraySize size
            return $
                pre_cr $ _if ("Status__Success" @: enumFieldType @== ("status" @: _Status) @. variant @: enumFieldType)
                    $ trail_cr . block $ [
                        pre_cr $ __termina_msg_queue__init @@ [
                            msgQueueId @: __termina_id_t,
                            cSize,
                            _sizeOfType __termina_id_t,
                            "status" @: (_const . ptr $ _Status)
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

        genConnectResourceMutex ::
            Identifier -- Task Identifier
            -> Identifier -- Class Identifier
            -> Identifier -- Port Identifier
            -> SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genConnectResourceMutex taskId classId portId ann = do
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
                APConnTy (TInterface RegularInterface iface)
                        (TGlobal ResourceClass resource) ((ProcedureSeman pr prtys) : pxs) -> do
                    -- Generate last procedures without new lines
                    procs <- forM pxs (\(ProcedureSeman procedureId ptys) -> do
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

        genConnectResourceTaskLock :: 
            Identifier -- Task Identifier
            -> Identifier -- Class Identifier
            -> Identifier -- Port Identifier
            -> SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genConnectResourceTaskLock taskId classId portId ann = do
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
                APConnTy (TInterface RegularInterface iface)
                        (TGlobal ResourceClass resource) ((ProcedureSeman pr prtys) : pxs) -> do
                    -- Generate last procedures without new lines
                    procs <- forM pxs (\(ProcedureSeman procedureId ptys) -> do
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
        
        genConnectResourceEventLock :: 
            Identifier -- Handler Identifier
            -> Identifier -- Class Identifier
            -> Identifier -- Port Identifier
            -> SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genConnectResourceEventLock taskId classId portId ann = do
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
                APConnTy (TInterface RegularInterface iface)
                        (TGlobal ResourceClass resource) ((ProcedureSeman pr prtys) : pxs) -> do
                    -- Generate last procedures without new lines
                    procs <- forM pxs (\(ProcedureSeman procedureId ptys) -> do
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
                    OSALResourceLockIrq -> genConnectResourceEventLock taskId classId portName ann
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
                    OSALResourceLockIrq -> genConnectResourceTaskLock taskId classId portName ann
                    OSALResourceLockMutex _ ->
                        genConnectResourceMutex taskId classId portName ann)

genMainFile :: QualifiedName
    -> TerminaProgArch SemanticAnn
    -> CGenerator CFile
genMainFile mName progArchitecture = do
    let includeTermina = CPPDirective (CPPInclude True "termina.h") (internalAnn (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl (CEDFunction void (namefy $ "termina_app" <::> "init_globals") []) (internalAnn (CDeclarationAnn True))
        periodicTimers = M.filter (\case { TPPeriodicTimerEmitter {} -> True; _ -> False }) (emitters progArchitecture)
    taskMessageQueues <- getTasksMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture

    resLockingMap <- genResLockingMap progArchitecture dependenciesMap
    let mutexes = M.filter (\case{ OSALResourceLockMutex {} -> True; _ -> False }) resLockingMap

    cVariantsForTaskPorts <- concat <$> mapM genVariantsForTaskPorts (M.elems taskClss)
    cMutexDefines <- genDefineMutexId (M.keys mutexes)
    cTaskDefines <- genDefineTaskId (M.keys $ tasks progArchitecture)
    cMsgQueueDefines <- genDefineMsgQueueId (taskMessageQueues ++ channelMessageQueues)
    cTimerDefines <- genDefineTimerId (M.keys periodicTimers)
    cPoolMemoryAreas <- genPoolMemoryAreas (M.elems $ pools progArchitecture)
    cAtomicDeclarations <- genAtomicDeclarations (M.elems $ atomics progArchitecture)
    cAtomicArrayDeclarations <- genAtomicArrayDeclarations (M.elems $ atomicArrays progArchitecture)

    initTasks <- genInitTasks progArchitecture
    initEmitters <- genInitEmitters progArchitecture
    initMutexes <- genInitMutexes mutexes
    initMessageQueues <- genInitMessageQueues (taskMessageQueues ++ channelMessageQueues)

    enableProtection <- genEnableProtection progArchitecture resLockingMap

{--
    appConfig <- genAppConfig progArchitecture (taskMessageQueues ++ channelMessageQueues) mutexes
    --}
    return $ CSourceFile mName $ [
            -- #include <termina.h>
            includeTermina
        ] ++ includes
        ++ [
            externInitGlobals
        ] ++ cVariantsForTaskPorts ++ cMutexDefines ++ cTaskDefines ++ cMsgQueueDefines ++ cTimerDefines
        ++ cAtomicDeclarations ++ cAtomicArrayDeclarations
        ++ cPoolMemoryAreas
        ++ [initTasks, initEmitters, initMutexes, initMessageQueues, enableProtection]
        -- ++ appConfig

    where
        -- | List of modules that must be included
        incs = getGlobDeclModules progArchitecture
        -- | List of include directives
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs
        -- List of used task classes
        taskClss = taskClasses progArchitecture

        dependenciesMap = getResDependencies progArchitecture

runGenMainFile ::
    TerminaConfig
    -> M.Map Identifier Integer
    -> QualifiedName
    -> TerminaProgArch SemanticAnn
    -> Either CGeneratorError CFile
runGenMainFile config irqMap mainFilePath progArchitecture =
    runReader (runExceptT (genMainFile mainFilePath progArchitecture))
        (CGeneratorEnv M.empty config syscallFunctionsMap irqMap)
