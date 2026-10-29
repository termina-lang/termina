{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Glue where

import Generator.LanguageC.AST
import qualified Data.Map.Strict as M
import Generator.CodeGen.Common
import Semantic.Types
import ControlFlow.Architecture.Types
import Configuration.Configuration
import Configuration.Platform (Platform, getPlatformInterruptMap, defaultStackSize)
import Generator.LanguageC.Embedded
import ControlFlow.Architecture.Utils
import System.FilePath
import Control.Monad.Except
import Generator.CodeGen.Application.Utils
import Generator.CodeGen.Types
import Semantic.AST
import Control.Monad (forM)
import ControlFlow.Architecture.Errors (TestLevel(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Generator.CodeGen.Expression
import Generator.Monadic
import Control.Monad.State
import qualified Data.Set as S
import Utils.Annotations
import Core.Utils (harnessTaskPriority)
import Core.TestCases (NumberedCase(..))
import Data.Word (Word32)

-- | Body of an initialization function that can fail, given the declarations of
-- the local variables that its steps share and the steps. The status is an
-- output: the function sets it to zero, performs the first step and performs
-- each of the rest only while the status is still zero.
genStatusSteps :: [CCompoundBlockItem] -> [[CCompoundBlockItem]] -> [CCompoundBlockItem]
genStatusSteps declarations steps =
    declarations ++
    pre_cr (deref ("status" @: (_const . ptr $ termina__error_code_t)) @= termina__error__none) :
        case steps of
            [] -> []
            (firstStep : restSteps) -> firstStep ++ map statusCheck restSteps

    where

        statusCheck :: [CCompoundBlockItem] -> CCompoundBlockItem
        statusCheck items =
            pre_cr $ _if (termina__error__none @== deref ("status" @: (_const . ptr $ termina__error_code_t)))
                $ trail_cr . block $ items

genInitHandlers :: TerminaProgArch a -> CGenerator [CFileItem]
genInitHandlers progArchitecture = do
    let progHandlers = M.elems $ handlers progArchitecture
    initHandlers <- mapM genOSALHandlerInit progHandlers
    return [pre_cr $ static_function (terminafy $ "app" <::> "init_handlers") [] @-> void $
            trail_cr . block $ initHandlers | not (null initHandlers)]

    where

        genOSALHandlerInit :: TPHandler a -> CGenerator CCompoundBlockItem
        genOSALHandlerInit hndlr = do
            handlerId <- genDefineHandlerIdLabel (handlerName hndlr)
            return $ pre_cr $
                handlerName hndlr @: typeDef (handlerClass hndlr) @. handlerIDField @: termina__id_t @= handlerId @: termina__id_t

genInitTasks :: TerminaProgArch a -> CGenerator [CFileItem]
genInitTasks progArchitecture = do
    let progTasks = M.elems $ tasks progArchitecture
    initTasks <- mapM genOSALTaskInit progTasks
    testBuild <- gets (isTestBuild . configParams)
    initHarness <- if testBuild then (: []) <$> genHarnessInit else return []
    let steps = initTasks ++ initHarness
    return [pre_cr $ static_function (terminafy $ "app" <::> "init_tasks") ["status" @: (_const . ptr $ termina__error_code_t)] @-> void $
            trail_cr . block $ genStatusSteps [] steps | not (null steps)]

    where

        -- | The task of the harness, at the priority reserved for it and without a queue
        genHarnessInit :: CGenerator [CCompoundBlockItem]
        genHarnessInit = do
            harnessId <- genDefineTaskIdLabel testHarness
            defaultSize <- gets (defaultStackSize . targetPlatform)
            stackSize <- gets (fromMaybe defaultSize . harnessStackSize . configParams)
            return [
                    pre_cr $ termina__task__init @@ [
                        harnessId @: termina__id_t,
                        dec harnessTaskPriority @: termina__task_prio_t,
                        dec stackSize @: size_t,
                        addrOf (testHarnessFunction @: termina__task_entry_t),
                        "NULL" @: ptr void,
                        "status" @: (_const . ptr $ termina__error_code_t)
                    ]
                ]

        genOSALTaskInit :: TPTask a -> CGenerator [CCompoundBlockItem]
        genOSALTaskInit tsk = do
            taskId <- genDefineTaskIdLabel (taskName tsk)
            taskMsgQueueId <- genDefineTaskMsgQueueIdLabel (taskName tsk)
            defaultSize <- gets (defaultStackSize . targetPlatform)
            let tskName = taskName tsk
                classId = taskClass tsk
                taskPrio = getCInteger . getPriority $ tsk
                taskStackSize = getCInteger . getStackSize defaultSize $ tsk
            cTaskFunctionName <- taskFunctionName classId
            return [
                    pre_cr $ tskName @: typeDef classId @. taskIDField @: termina__id_t
                        @= taskId @: termina__id_t,
                    pre_cr $ tskName @: typeDef classId @. taskMsgQueueIDField @: termina__id_t
                        @= taskMsgQueueId @: termina__id_t,
                    pre_cr $ termina__task__init @@ [
                        taskId @: termina__id_t,
                        taskPrio @: termina__task_prio_t,
                        taskStackSize @: size_t,
                        addrOf (cTaskFunctionName @: termina__task_entry_t),
                        addrOf (tskName @: typeDef classId),
                        "status" @: (_const . ptr $ termina__error_code_t)
                    ]
                ]

-- | Function termina__app__install_emitters. This function is called from the
-- Init task.  The function installs the ISRs and the periodic timers. The
-- function is called AFTER the initialization of the tasks and handlers.
genInitEmitters :: TerminaProgArch SemanticAnn -> CGenerator [CFileItem]
genInitEmitters progArchitecture = do
    testBuild <- gets (isTestBuild . configParams)
    let progEmitters = M.elems $ emitters progArchitecture
    let installedEmitters = filter (\case { TPSystemInitEmitter {} -> False; _ -> True }) progEmitters
        -- | A test without a handler of its author on system_except gets the hook of the runtime
        authorExcept = any (\case { TPSystemExceptEmitter {} -> True; _ -> False }) installedEmitters
        hookExcept = testBuild && not authorExcept
        -- | The OSAL copies the connection, so all the timers share one
        -- connection variable and all the interrupts share another one
        declarations =
            [pre_cr $ var "timer_connection" termina__periodic_timer_connection_t
                | any (\case { TPPeriodicTimerEmitter {} -> True; _ -> False }) installedEmitters]
            ++ [pre_cr $ var "interrupt_connection" termina__interrupt_connection_t
                | any (\case { TPInterruptEmitter {} -> True; _ -> False }) installedEmitters]
            ++ [pre_cr $ var "except_connection" termina__system_except_connection_t
                | authorExcept || hookExcept]
    initEmitter <- mapM genEmitterConnection installedEmitters
    let progTestEmitters = M.elems $ testEmitters progArchitecture
    initTestEmitters <- mapM genTestEmitterConnection progTestEmitters
    fireFunctions <- concat <$> mapM genTestFireFunction progTestEmitters
    initHook <- if hookExcept then (: []) <$> genExceptHookConnection else return []
    let steps = initEmitter ++ initTestEmitters ++ initHook
    return $ fireFunctions ++ [pre_cr $ static_function (terminafy $ "app" <::> "init_emitters") ["status" @: (_const . ptr $ termina__error_code_t)] @-> void $
            trail_cr . block $ genStatusSteps declarations steps | not (null steps)]

    where

        -- | The hook of the runtime attends system_except
        genExceptHookConnection :: CGenerator [CCompoundBlockItem]
        genExceptHookConnection = do
            handlerId <- genDefineHandlerIdLabel "system_except"
            genExceptConnection ("NULL" @: ptr void) handlerId termina__test__except_hook

        -- | The handler of system_except: its object, its identifier and its action
        genExceptConnection :: CExpression -> Identifier -> CObject -> CGenerator [CCompoundBlockItem]
        genExceptConnection handlerObject handlerId handlerAction = do
            emitterId <- genDefineEmitterIdLabel "system_except"
            let connection = "except_connection" @: termina__system_except_connection_t
            return [
                    pre_cr $ connection @. "handler_object" @: ptr void @= handlerObject,
                    no_cr $ connection @. "handler_id" @: termina__id_t @= handlerId @: termina__id_t,
                    no_cr $ connection @. "handler_action" @: termina__system_except_action_t @= addrOf handlerAction,
                    pre_cr $ termina__except__init_emitter @@ [
                        emitterId @: termina__id_t,
                        addrOf connection
                    ]
                ]

        -- | The target of an emitter and its port
        emitterTarget :: Identifier -> CGenerator (Identifier, Identifier)
        emitterTarget emitter = case M.lookup emitter (emitterTargets progArchitecture) of
            Just (entity, port, _) -> return (entity, port)
            Nothing -> throwError $ InternalError $ "Emitter not connected: " ++ show emitter

        -- | The connection of a TestEmitter goes in its own object
        genTestEmitterConnection :: TPTestEmitter SemanticAnn -> CGenerator [CCompoundBlockItem]
        genTestEmitterConnection (TPTestEmitter emitter _ _ _) = do
            emitterId <- genDefineEmitterIdLabel emitter
            (targetEntity, targetPort) <- emitterTarget emitter
            let obj = emitter @: typeDef testEmitter
                idAssignment = pre_cr $ obj @. "emitter_id" @: termina__id_t @= emitterId @: termina__id_t
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier _ _ _ _ _ _ _) -> do
                    handlerId <- genDefineHandlerIdLabel identifier
                    return [
                            idAssignment,
                            no_cr $ obj @. "type" @: enumFieldType
                                @= "termina__emitter_connection_type__handler" @: enumFieldType,
                            no_cr $ obj @. "handler" @: termina__test__emitter_handler_connection_t
                                @. "handler_id" @: termina__id_t @= handlerId @: termina__id_t,
                            no_cr $ obj @. "handler" @: termina__test__emitter_handler_connection_t
                                @. "handler_action" @: termina__test__emitter_handler_action_t
                                @= addrOf (testFireFunctionName emitter @: termina__test__emitter_handler_action_t)
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        taskId <- genDefineTaskIdLabel targetEntity
                        taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetEntity
                        sinkMsgQueueId <- genDefineSinkMsgQueueIdLabel targetEntity targetPort
                        let taskConnection = obj @. "task" @: termina__emitter_task_connection_t
                        return [
                                idAssignment,
                                no_cr $ obj @. "type" @: enumFieldType
                                    @= "termina__emitter_connection_type__task" @: enumFieldType,
                                no_cr $ taskConnection @. "task_id" @: termina__id_t @= taskId @: termina__id_t,
                                no_cr $ taskConnection @. "task_msg_queue_id" @: termina__id_t @= taskMsgQueueId @: termina__id_t,
                                no_cr $ taskConnection @. "sink_msgq_id" @: termina__id_t @= sinkMsgQueueId @: termina__id_t,
                                no_cr $ taskConnection @. "sink_port_id" @: termina__id_t @= variantForPort @: termina__id_t,
                                pre_cr $ targetEntity @: typeDef tskCls @. targetPort @: termina__id_t
                                    @= sinkMsgQueueId @: termina__id_t
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for test emitter: " ++ show targetEntity

        -- | The function through which fire calls the action of a handler with the value
        genTestFireFunction :: TPTestEmitter SemanticAnn -> CGenerator [CFileItem]
        genTestFireFunction (TPTestEmitter emitter ety _ _) = do
            (targetEntity, targetPort) <- emitterTarget emitter
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier classId _ _ _ _ _ _) -> do
                    let cls = handlerClasses progArchitecture M.! classId
                        (_, targetAction) = sinkPorts cls M.! targetPort
                    cEventType <- genType noqual ety
                    let cValue = "value" @: (_const . ptr $ _const void)
                        cArg = deref (cast (ptr (_const cEventType)) cValue)
                        cAction = classId <::> targetAction @: CTFunction void []
                        cEvent = eventParam @: (_const . ptr $ _const termina__event_t)
                    return [pre_cr $ static_function (testFireFunctionName emitter) [
                                eventParam @: (_const . ptr $ _const termina__event_t),
                                "value" @: (_const . ptr $ _const void)
                            ] @-> void $
                            trail_cr . block $ [
                                pre_cr $ cAction @@ [cEvent, addrOf (identifier @: typeDef classId), cArg]
                            ]]
                Nothing -> return []

        testFireFunctionName :: Identifier -> Identifier
        testFireFunctionName emitter = terminafy ("test_fire" <::> emitter)

        genEmitterConnection :: TPEmitter SemanticAnn -> CGenerator [CCompoundBlockItem]
        genEmitterConnection (TPPeriodicTimerEmitter timer _ _ _) = do
            timerId <- genDefineTimerIdLabel timer
            emitterId <- genDefineEmitterIdLabel timer
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            (targetEntity, targetPort) <- case M.lookup timer (emitterTargets progArchitecture) of
                Just (entity, port, _) -> return (entity, port)
                -- | If the interrupt emitter is not connected, throw an error
                Nothing -> throwError $ InternalError $ "Periodic timer emitter not connected: " ++ show timer
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier classId _ _ _ _ _ _) -> do
                    let cls = handlerClasses progArchitecture M.! classId
                        (_, targetAction) = sinkPorts cls M.! targetPort
                    handlerId <- genDefineHandlerIdLabel identifier
                    return [
                            pre_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "type" @: enumFieldType
                                @= "termina__emitter_connection_type__handler" @: enumFieldType,
                            no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "handler" @: termina__periodic_timer_handler_connection_t
                                @. "handler_object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "handler" @: termina__periodic_timer_handler_connection_t
                                @. "handler_id" @: termina__periodic_timer_action_t @= handlerId @: size_t,
                            no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "handler" @: termina__periodic_timer_handler_connection_t
                                @. "handler_action" @: termina__periodic_timer_action_t @= addrOf (classId <::> targetAction @: termina__periodic_timer_action_t),
                            pre_cr $ termina__periodic_timer__init @@ [
                                timerId @: termina__id_t,
                                emitterId @: termina__id_t,
                                addrOf ("timer_connection" @: termina__periodic_timer_connection_t),
                                addrOf (timer @: termina__periodic_timer_t @. "period" @: _TimeVal),
                                "status" @: (_const . ptr $ termina__error_code_t)
                            ]
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        taskId <- genDefineTaskIdLabel targetEntity
                        taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetEntity
                        sinkMsgQueueId <- genDefineSinkMsgQueueIdLabel targetEntity targetPort
                        return [
                                pre_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "type" @: enumFieldType
                                    @= "termina__emitter_connection_type__task" @: enumFieldType,
                                no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "task_id" @: termina__id_t @= taskId @: termina__id_t,
                                no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "task_msg_queue_id" @: termina__id_t @= taskMsgQueueId @: termina__id_t,
                                no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "sink_msgq_id" @: termina__id_t @= sinkMsgQueueId @:  termina__id_t,
                                no_cr $ "timer_connection" @: termina__periodic_timer_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "sink_port_id" @: termina__id_t @= variantForPort @:  termina__id_t,
                                pre_cr $ targetEntity @: typeDef tskCls @. targetPort @: termina__id_t
                                    @= sinkMsgQueueId @: termina__id_t,
                                pre_cr $ termina__periodic_timer__init @@ [
                                    timerId @: termina__id_t,
                                    emitterId @: termina__id_t,
                                    addrOf ("timer_connection" @: termina__periodic_timer_connection_t),
                                    addrOf (timer @: termina__periodic_timer_t @. "period" @: _TimeVal),
                                    "status" @: (_const . ptr $ termina__error_code_t)
                                ]
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for timer: " ++ show targetEntity
        genEmitterConnection (TPInterruptEmitter irq _) = do
            emitterId <- genDefineEmitterIdLabel irq
            irqMap <- gets (getPlatformInterruptMap . targetPlatform)
            irqVector <- case M.lookup irq irqMap of
                Just v -> return v
                Nothing -> throwError $ InternalError $ "Invalid interrupt emitter: " ++ show irq
            -- | Obtain the identifier of the target entity and the port to which the
            -- interrupt emitter is connected
            (targetEntity, targetPort) <- case M.lookup irq (emitterTargets progArchitecture) of
                Just (entity, port, _) -> return (entity, port)
                -- | If the interrupt emitter is not connected, throw an error
                Nothing -> throwError $ InternalError $ "Interrupt emitter not connected: " ++ show irq
            -- | Now we have to check if the target entity is a task or a handler
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier classId _ _ _ _ _ _) -> do
                    let cls = handlerClasses progArchitecture M.! classId
                        (_, targetAction) = sinkPorts cls M.! targetPort
                    handlerId <- genDefineHandlerIdLabel identifier
                    return [
                            pre_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "type" @: enumFieldType
                                @= "termina__emitter_connection_type__handler" @: enumFieldType,
                            no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "handler" @: termina__interrupt_handler_connection_t
                                @. "handler_object" @: ptr void @= cast (ptr void) (addrOf (identifier @: typeDef classId)),
                            no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "handler" @: termina__interrupt_handler_connection_t
                                @. "handler_id" @: termina__interrupt_action_t @= handlerId @: size_t,
                            no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "handler" @: termina__interrupt_handler_connection_t
                                @. "handler_action" @: termina__interrupt_action_t @= addrOf (classId <::> targetAction @: termina__interrupt_action_t),
                            pre_cr $ termina__interrupt__init @@ [
                                dec irqVector @: termina__id_t,
                                emitterId @: termina__id_t,
                                addrOf ("interrupt_connection" @: termina__interrupt_connection_t),
                                "status" @: (_const . ptr $ termina__error_code_t)
                            ]
                        ]
                Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
                    Just (TPTask _ tskCls _ _ _ _ _ _ _) -> do
                        variantForPort <- genVariantForPort tskCls targetPort
                        taskId <- genDefineTaskIdLabel targetEntity
                        taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetEntity
                        sinkMsgQueueId <- genDefineSinkMsgQueueIdLabel targetEntity targetPort
                        return [
                                pre_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "type" @: enumFieldType
                                    @= "termina__emitter_connection_type__task" @: enumFieldType,
                                no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "task_id" @: termina__id_t @= taskId @: termina__id_t,
                                no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "task_msg_queue_id" @: termina__id_t @= taskMsgQueueId @: termina__id_t,
                                no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "sink_msgq_id" @: termina__id_t @= sinkMsgQueueId @:  termina__id_t,
                                no_cr $ "interrupt_connection" @: termina__interrupt_connection_t @. "task" @: termina__emitter_task_connection_t
                                    @. "sink_port_id" @: termina__id_t @= variantForPort @:  termina__id_t,
                                pre_cr $ targetEntity @: typeDef tskCls @. targetPort @: termina__id_t
                                    @= sinkMsgQueueId @: termina__id_t,
                                pre_cr $ termina__interrupt__init @@ [
                                    dec irqVector @: termina__id_t,
                                    emitterId @: termina__id_t,
                                    addrOf ("interrupt_connection" @: termina__interrupt_connection_t),
                                    "status" @: (_const . ptr $ termina__error_code_t)
                                ]
                            ]
                    Nothing -> throwError $ InternalError $ "Invalid connection for interrupt: " ++ show targetEntity
        -- | Only a handler can attend system_except (SE-214)
        genEmitterConnection (TPSystemExceptEmitter systemExcept _) = do
            (targetEntity, targetPort) <- emitterTarget systemExcept
            case M.lookup targetEntity (handlers progArchitecture) of
                Just (TPHandler identifier classId _ _ _ _ _ _) -> do
                    let cls = handlerClasses progArchitecture M.! classId
                        (_, targetAction) = sinkPorts cls M.! targetPort
                    handlerId <- genDefineHandlerIdLabel identifier
                    genExceptConnection (cast (ptr void) (addrOf (identifier @: typeDef classId))) handlerId
                        (classId <::> targetAction @: termina__system_except_action_t)
                Nothing -> throwError $ InternalError $ "Invalid connection for system except: " ++ show targetEntity
        genEmitterConnection _ = throwError $ InternalError "Invalid event emitter"

-- | Function termina__app__init_mutexes. This function is called from the
-- Init task.  The function initializes the mutexes. The function is called AFTER
-- the execution of the init handler (if any) and before the initialization of the
-- resource locking mechanism.
genInitMutexes :: ResourceLockingMap -> CGenerator [CFileItem]
genInitMutexes mutexes = do
    let mutexesList = M.toList mutexes
    initMutexes <- mapM genOSALMutexInit mutexesList
    return [pre_cr $ static_function (terminafy $ "app" <::> "init_mutexes") ["status" @: (_const . ptr $ termina__error_code_t)] @-> void $
            trail_cr . block $ genStatusSteps [pre_cr $ var "protocol" _MutexProtocol] initMutexes | not (null initMutexes)]

    where
        genOSALMutexInit :: (Identifier, ResourceLock) -> CGenerator [CCompoundBlockItem]
        genOSALMutexInit (identifier, ResourceLockMutex ceilingPriority) = do
            mutexId <- genDefineMutexIdLabel identifier
            return [
                    -- protocol._variant = MutexProtocol__Ceiling;
                    pre_cr $ "protocol" @: _MutexProtocol @. variant @: enumFieldType @= "MutexProtocol__Ceiling" @: enumFieldType,
                    -- protocol.Ceiling._0 = ceiling_priority;
                    no_cr $ "protocol" @: _MutexProtocol @. "Ceiling" @: enumFieldType @. variantParamField 0 @: termina__task_prio_t @= getCInteger ceilingPriority @: termina__task_prio_t,
                    -- termina__mutex__init(mutex_id, protocol, status);
                    pre_cr $ termina__mutex__init @@ [
                        mutexId @: termina__id_t,
                        "protocol" @: _MutexProtocol,
                        "status" @: (_const . ptr $ termina__error_code_t)
                    ]
                ]
        genOSALMutexInit _ = throwError $ InternalError "Invalid resource lock"

genChannelConnections :: TerminaProgArch a -> CGenerator [CFileItem]
genChannelConnections progArchitecture = do
    let targets = M.toList $ channelTargets progArchitecture
    channelConnections <- concat <$> traverse genChannelConnection targets
    testChannelConnections <- concat <$> traverse genTestChannelConnection (M.keys $ testChannels progArchitecture)
    let connections = channelConnections ++ testChannelConnections
    return [pre_cr $ static_function (terminafy $ "app" <::> "init_channel_connections") [] @-> void $
            trail_cr . block $ connections | not (null connections)]

    where

        -- | A TestChannel delivers to the harness, through a queue of events of its own
        genTestChannelConnection :: Identifier -> CGenerator [CCompoundBlockItem]
        genTestChannelConnection channelName = do
            harnessId <- genDefineTaskIdLabel testHarness
            eventMsgQueueId <- genDefineTestEventMsgQueueIdLabel channelName
            channelMsgQueueId <- genDefineChannelMsgQueueIdLabel channelName
            return [
                    pre_cr $ channelName @: termina__msg_queue_t @. "task_id" @: termina__id_t
                        @= harnessId @: termina__id_t,
                    no_cr $ channelName @: termina__msg_queue_t @. "task_msg_queue_id" @: termina__id_t
                        @= eventMsgQueueId @: termina__id_t,
                    no_cr $ channelName @: termina__msg_queue_t @. "channel_msg_queue_id" @: termina__id_t
                        @= channelMsgQueueId @: termina__id_t,
                    no_cr $ channelName @: termina__msg_queue_t @. "port_id" @: termina__id_t
                        @= dec 0 @: size_t
                ]

        genChannelConnection :: (Identifier, (Identifier, Identifier, a)) -> CGenerator [CCompoundBlockItem]
        genChannelConnection (channelName, (targetName, targetPort, _)) = do
            let classId = taskClass $ tasks progArchitecture M.! targetName
            let (TPMsgQueue _ dty _ _ _) = channels progArchitecture M.! channelName
            taskId <- genDefineTaskIdLabel targetName
            taskMsgQueueId <- genDefineTaskMsgQueueIdLabel targetName
            channelMsgQueueId <- genDefineChannelMsgQueueIdLabel channelName
            portVariant <- genVariantForPort classId targetPort
            return $ pre_cr (channelName @: termina__msg_queue_t @. "task_id" @: termina__id_t
                        @= taskId @: termina__id_t) :
                    no_cr (channelName @: termina__msg_queue_t @. "task_msg_queue_id" @: termina__id_t
                        @= taskMsgQueueId @: termina__id_t) :
                    (case dty of
                        TUnit -> [
                            no_cr $ channelName @: termina__msg_queue_t @. "channel_msg_queue_id" @: termina__id_t
                                @= "TERMINA__ID__INVALID" @: termina__id_t
                            ]
                        _ -> [
                            no_cr $ channelName @: termina__msg_queue_t @. "channel_msg_queue_id" @: termina__id_t
                                @= channelMsgQueueId @: termina__id_t
                            ]) ++ [
                    no_cr $ channelName @: termina__msg_queue_t @. "port_id" @: termina__id_t
                        @= portVariant @: termina__id_t,
                    pre_cr $ targetName @: typeDef classId @. targetPort @: termina__id_t
                        @= channelMsgQueueId @: termina__id_t
                ]

genInitPools :: [TPPool SemanticAnn] -> [TPPool SemanticAnn] -> CGenerator [CFileItem]
genInitPools pls testPls = do
    initPools <- mapM (\p@(TPPool identifier _ _ _ _) ->
        let poolObj = identifier @: termina__pool_t in genPoolInit termina__pool__init poolObj poolObj p) pls
    -- | The pool of a TestPool is its first field
    initTestPools <- mapM (\p@(TPPool identifier _ _ _ _) ->
        let testPoolObj = identifier @: typeDef testPool in
        genPoolInit termina__test__pool__init (testPoolObj @. "pool" @: termina__pool_t) testPoolObj p) testPls
    let steps = initPools ++ initTestPools
    return [pre_cr $ static_function (terminafy $ "app" <::> "init_pools") ["status" @: (_const . ptr $ termina__error_code_t)] @-> void $
            trail_cr . block $ genStatusSteps [] steps | not (null steps)]

    where

        -- | The identifier goes in the pool, and the function initializes the object around it
        genPoolInit :: CExpression -> CObject -> CObject -> TPPool SemanticAnn -> CGenerator [CCompoundBlockItem]
        genPoolInit initFunction poolObj obj (TPPool identifier ts _ _ _) = do
            cTs <- genType noqual ts
            poolId <- genDefinePoolIdLabel identifier
            return [
                    pre_cr $ poolObj @. "pool_id" @: termina__id_t
                        @= poolId @: termina__id_t,
                    pre_cr $ initFunction @@
                            [
                                addrOf obj,
                                cast (ptr void) (poolMemoryArea identifier @: ptr uint8_t),
                                _sizeOfExpr (poolMemoryArea identifier @: ptr uint8_t),
                                _sizeOfType cTs,
                                "status" @: (_const . ptr $ termina__error_code_t)
                            ]
                ]

genInitMessageQueues :: [OSALMsgQueue] -> CGenerator [CFileItem]
genInitMessageQueues queues = do
    initMsgQueues <- concat <$> traverse genOSALMsgQueueInit queues
    return [pre_cr $ static_function (terminafy $ "app" <::> "init_msg_queues") ["status" @: (_const . ptr $ termina__error_code_t)] @-> void $
            trail_cr . block $ genStatusSteps [] initMsgQueues | not (null initMsgQueues)]

    where
        -- | Steps that initialize a message queue: one step, or none if the
        -- queue does not need a definition
        genOSALMsgQueueInit :: OSALMsgQueue -> CGenerator [[CCompoundBlockItem]]
        -- | The runtime creates the queue of the harness and opens the scenario as case 1
        genOSALMsgQueueInit OSALTestHarnessMsgQueue = do
            msgQueueId <- genDefineMsgQueueIdLabel OSALTestHarnessMsgQueue
            return [[
                    pre_cr $ termina__test__scenario__init @@ [
                        msgQueueId @: termina__id_t,
                        "status" @: (_const . ptr $ termina__error_code_t)
                    ]
                ]]
        genOSALMsgQueueInit mq = do
            elements <- msgQueueElements mq
            case elements of
                -- | Message queues with unit type do not need a definition
                Nothing -> return []
                Just (size, cTs) -> do
                    msgQueueId <- genDefineMsgQueueIdLabel mq
                    cSize <- genConstExpression size
                    return [[
                            pre_cr $ termina__msg_queue__init @@ [
                                msgQueueId @: termina__id_t,
                                _sizeOfType cTs,
                                cSize,
                                "status" @: (_const . ptr $ termina__error_code_t)
                            ]
                        ]]

genEnableProtection :: TerminaProgArch SemanticAnn -> CGenerator [CFileItem]
genEnableProtection progArchitecture = do
    resourceProtections <- concat <$> forM (M.elems $ resources progArchitecture) genEnableProtectionResource
    poolProtections <- concat <$> forM (M.elems $ pools progArchitecture)
        (\(TPPool poolId _ _ _ _) -> genEnableProtectionPool poolId (poolId @: termina__pool_t))
    -- | The lock of a TestPool is the one of the pool inside it
    testPoolProtections <- concat <$> forM (M.elems $ testPools progArchitecture)
        (\(TPPool poolId _ _ _ _) -> genEnableProtectionPool poolId ((poolId @: typeDef testPool) @. "pool" @: termina__pool_t))
    let protections = resourceProtections ++ poolProtections ++ testPoolProtections
    return [pre_cr $ static_function (terminafy $ "app" <::> "enable_protection") [] @-> void $
            trail_cr . block $ protections | not (null protections)]

    where

        resourceLockingMap = genResourceLockings progArchitecture

        genEnableProtectionResource :: TPResource SemanticAnn -> CGenerator [CCompoundBlockItem]
        genEnableProtectionResource res = do
            let resourceId = resourceName res
                classId = resourceClass res
            case M.lookup resourceId resourceLockingMap of
                Just ResourceLockNone -> return []
                Just ResourceLockIrq -> return [
                        pre_cr $ resourceId @: typeDef classId @. resourceLockTypeField @: termina__resource_lock_type_t @. "type" @: enumFieldType @= 
                            "termina__resource_lock_type__irq" @: enumFieldType
                    ]
                Just (ResourceLockMutex _) -> do
                    mutexId <- genDefineMutexIdLabel resourceId
                    return [
                        pre_cr $ resourceId @: typeDef classId @. resourceLockTypeField @: termina__resource_lock_type_t @. "type" @: enumFieldType @= 
                            "termina__resource_lock_type__mutex" @: enumFieldType,
                        no_cr $ resourceId @: typeDef classId @. resourceLockTypeField @: termina__resource_lock_type_t @. "mutex" @: termina__enum__resource_lock_type__mutex_params_t @. "mutex_id" @: termina__id_t
                            @= mutexId @: termina__id_t
                        ]
                Nothing -> throwError $ InternalError $ "Resource " ++ show resourceId
                    ++ " not found in resource locking map" 
        
        genEnableProtectionPool :: Identifier -> CObject -> CGenerator [CCompoundBlockItem]
        genEnableProtectionPool poolId poolObj = do
            case M.lookup poolId resourceLockingMap of
                Just ResourceLockNone -> return []
                Just ResourceLockIrq -> return [
                        pre_cr $ poolObj @. resourceLockTypeField @: termina__resource_lock_type_t @. "type" @: enumFieldType @=
                            "termina__resource_lock_type__irq" @: enumFieldType
                    ]
                Just (ResourceLockMutex _) -> do
                    mutexId <- genDefineMutexIdLabel poolId
                    return [
                        pre_cr $ poolObj @. resourceLockTypeField @: termina__resource_lock_type_t @. "type" @: enumFieldType @=
                            "termina__resource_lock_type__mutex" @: enumFieldType,
                        no_cr $ poolObj @. resourceLockTypeField @: termina__resource_lock_type_t @. "mutex" @: termina__enum__resource_lock_type__mutex_params_t @. "mutex_id" @: termina__id_t
                            @= mutexId @: termina__id_t
                        ]
                Nothing -> throwError $ InternalError $ "Pool " ++ show poolId ++ " not found in resource locking map"

genInitalEventFunction :: TerminaProgArch a -> TPEmitter a -> CGenerator [CFileItem]
genInitalEventFunction progArchitecture (TPSystemInitEmitter systemInit _)= do
    (targetEntity, targetPort) <- case M.lookup systemInit (emitterTargets progArchitecture) of
        Just (entity, port, _) -> return (entity, port)
        -- | If the system init emitter is not connected, throw an error
        Nothing -> throwError $ InternalError $ "System init emitter not connected: " ++ show systemInit
    (identifier, classId) <- case M.lookup targetEntity (handlers progArchitecture) of
        Just (TPHandler ident cls _ _ _ _ _ _) -> return (ident, cls)
        Nothing -> throwError $ InternalError $ "Invalid connection for system init: " ++ show targetEntity
    let cls = handlerClasses progArchitecture M.! classId
        (_, targetAction) = sinkPorts cls M.! targetPort
        classIdType = typeDef classId
        connection = "connection" @: termina__system_init_connection_t
    handlerId <- genDefineHandlerIdLabel identifier
    emitterId <- genDefineEmitterIdLabel systemInit
    return [pre_cr $ static_function (terminafy $ "app" <::> "initial_event") [] @-> void $
            trail_cr . block $ [
                -- termina__system_init_connection_t connection;
                pre_cr $ var "connection" termina__system_init_connection_t,
                -- connection.handler_object = (void *)&identifier;
                pre_cr $ connection @. "handler_object" @: ptr void
                    @= cast (ptr void) (addrOf (identifier @: classIdType)),
                -- connection.handler_id = handlerId;
                no_cr $ connection @. "handler_id" @: termina__id_t
                    @= handlerId @: termina__id_t,
                -- connection.handler_action = &classId__targetAction;
                no_cr $ connection @. "handler_action" @: termina__system_init_action_t
                    @= addrOf (classId <::> targetAction @: termina__system_init_action_t),
                -- termina__system_init__dispatch(emitterId, &connection);
                pre_cr $ termina__system_init__dispatch @@ [
                    emitterId @: termina__id_t,
                    addrOf connection
                ],
                pre_cr $ _return Nothing
            ]]
genInitalEventFunction _ _ = throwError $ InternalError "Invalid event emitter"

-- | The record of a test, with the stamp of its descriptor
genTestRecord :: Word32 -> CGenerator [CFileItem]
genTestRecord stamp = do
    capacity <- gets (recordCapacity . configParams)
    let cCapacity = dec capacity @: size_t
    return [
            pre_cr $ global (var "termina__test__stamp" (_const uint32_t) @:= (dec (toInteger stamp) @: uint32_t)),
            pre_cr $ global (var "termina__test__capacity" (_const uint32_t) @:= (dec capacity @: uint32_t)),
            pre_cr $ global (var "termina__test__header" (typeDef "termina__test__header_t")),
            pre_cr $ global (var "termina__test__area" (CTArray uint8_t cCapacity))
        ]

-- | The name of the function of the task of the harness
testHarnessFunction :: Identifier
testHarnessFunction = terminafy ("app" <::> "test_harness")

-- | The reset before each case: the steps of the initialization that only write memory
genTestReset :: TerminaProgArch SemanticAnn -> Bool -> Bool -> CGenerator [CFileItem]
genTestReset progArchitecture hasPools hasProtection = do
    let status = "status" @: termina__error_code_t
        flushes = [ pre_cr $ termina__test__channel__flush @@ [addrOf (channel @: termina__msg_queue_t)]
                  | channel <- M.keys (testChannels progArchitecture) ]
    return [pre_cr $ static_function (terminafy ("app" <::> "test_reset")) [] @-> void $
            trail_cr . block $
                [pre_cr $ var "status" termina__error_code_t @:= termina__error__none | hasPools]
                ++ [pre_cr $ termina__app__init_globals @@ []]
                ++ [pre_cr $ termina__app__init_pools @@ [addrOf status] | hasPools]
                ++ [pre_cr $ termina__app__enable_protection @@ [] | hasProtection]
                ++ flushes
                ++ [pre_cr $ termina__test__clock__reset @@ []]
                ++ [pre_cr $ _if (termina__error__none @!= ("status" @: termina__error_code_t)) $ trail_cr . block $ [
                        pre_cr $ termina__except__runtime_failure @@ [
                            dec 0 @: size_t,
                            termina__runtime_operation__test_reset,
                            "status" @: termina__error_code_t
                        ]
                    ] | hasPools]
        ]

-- | The task of the harness of a concurrent test, which the runtime ends after finish()
genScenarioHarness :: CGenerator [CFileItem]
genScenarioHarness =
    return [pre_cr $ static_function testHarnessFunction ["arg" @: (_const . ptr $ void)] @-> void $
            trail_cr . block $
                genDiscardedParameters [("arg", _const . ptr $ void)]
                ++ [pre_cr $ termina__test__scenario__harness @@ []]
        ]

-- | The task of the harness, which runs each selected case after the reset
genTestHarness :: TerminaProgArch SemanticAnn -> CGenerator [CFileItem]
genTestHarness progArchitecture = do
    harnessTaskId <- genDefineTaskIdLabel testHarness
    harnessEmitterId <- genDefineEmitterIdLabel testHarness
    let cEventType = termina__event_t
        cEvent = [
                "emitter_id" @.= (harnessEmitterId @: termina__id_t),
                "owner.type" @.= ("termina__active_entity__task" @: enumFieldType),
                "owner.task.task_id" @.= (harnessTaskId @: termina__id_t),
                "port_id" @.= (dec 0 @: size_t)
            ] @.: cEventType
        cEventObj = eventParam @: cEventType
    cCases <- case architectureCases progArchitecture of
        Left (_, cls) -> throwError $ InternalError $ "Unknown test class: " ++ show cls
        Right cases -> return cases
    return [pre_cr $ static_function testHarnessFunction ["arg" @: (_const . ptr $ void)] @-> void $
            trail_cr . block $
                genDiscardedParameters [("arg", _const . ptr $ void)]
                ++ [pre_cr $ var eventParam (_const cEventType) @:= cEvent]
                ++ map (genCaseBlock cEventObj) cCases
                ++ [pre_cr $ termina__test__end @@ []]
        ]

    where

        genCaseBlock :: CObject -> NumberedCase -> CCompoundBlockItem
        genCaseBlock cEventObj (NumberedCase n instanceId classId caseId _) =
            let cCase = dec n @: uint16_t in
            pre_cr $ _if (termina__test__selected @@ [cCase]) $ trail_cr . block $ [
                    pre_cr $ termina__test__case_begin @@ [cCase],
                    no_cr $ termina__app__test_reset @@ [],
                    no_cr $ ((classId <::> caseId) @: CTFunction void []) @@ [addrOf cEventObj, addrOf (instanceId @: typeDef classId)],
                    no_cr $ termina__test__case_end @@ []
                ]

-- | Step of the application initialization: the call to the function that
-- performs it, whether that function can fail (and then receives the status)
-- and the definition of the function. A step whose definition is empty has
-- nothing to initialize and is neither defined nor called.
data InitStep = InitStep CExpression Bool [CFileItem]

genAppInit :: [InitStep] -> CGenerator CFileItem
genAppInit initSteps = do
    return $ pre_cr $ function (terminafy $ "app" <::> "init") [
            "status" @: (_const . ptr $ termina__error_code_t)
        ] @-> void $
        trail_cr . block $
            [
                pre_cr (deref ("status" @: (_const . ptr $ termina__error_code_t)) @= termina__error__none),
                -- | External call to termina__app__init_globals().
                -- This function cannot fail, so we do not check the status.
                pre_cr $ termina__app__init_globals @@ []
            ] ++ unguardedCalls (filter (\(InitStep _ _ definition) -> not (null definition)) initSteps)

    where

        stepCall :: InitStep -> CCompoundBlockItem
        stepCall (InitStep call _ _) = pre_cr call

        canFail :: InitStep -> Bool
        canFail (InitStep _ fails _) = fails

        -- | The steps up to the first one that can fail are called without
        -- checking the status, which is still zero.
        unguardedCalls :: [InitStep] -> [CCompoundBlockItem]
        unguardedCalls steps =
            case break canFail steps of
                (before, []) -> map stepCall before
                (before, step : after) -> map stepCall (before ++ [step]) ++ guardedCalls after

        -- | Each of the remaining steps is called only while the status is zero.
        -- The steps that cannot fail do not change the status, so they share
        -- the check with the next step that can fail.
        guardedCalls :: [InitStep] -> [CCompoundBlockItem]
        guardedCalls [] = []
        guardedCalls steps =
            case break canFail steps of
                (before, []) -> [statusCheck before]
                (before, step : after) -> statusCheck (before ++ [step]) : guardedCalls after

        statusCheck :: [InitStep] -> CCompoundBlockItem
        statusCheck steps =
            pre_cr $ _if (termina__error__none @== deref ("status" @: (_const . ptr $ termina__error_code_t)))
                $ trail_cr . block $ map stepCall steps

genMainFile :: QualifiedName
    -> Word32
    -> TerminaProgArch SemanticAnn
    -> CGenerator CFile
genMainFile mName stamp progArchitecture = do
    let includeTermina = CPPDirective (CPPInclude True "termina.h") (internalAnn (CPPDirectiveAnn True))
    sinkPortMessageQueues <- getSinkPortMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture
    taskMessageQueues <- getTasksMessageQueues progArchitecture (sinkPortMessageQueues ++ channelMessageQueues)

    let mutexes = 
            M.filter (\case{ResourceLockMutex {} -> True; _ -> False}) (genResourceLockings progArchitecture)

    cPoolMemoryAreas <- genPoolMemoryAreas (M.elems (pools progArchitecture) ++ M.elems (testPools progArchitecture))

    initTasks <- genInitTasks progArchitecture
    initHandlers <- genInitHandlers progArchitecture
    initEmitters <- genInitEmitters progArchitecture
    initPools <- genInitPools (M.elems $ pools progArchitecture) (M.elems $ testPools progArchitecture)
    initMutexes <- genInitMutexes mutexes
    testBuild <- gets (isTestBuild . configParams)
    let level = testLevelOf testBuild progArchitecture

    initMessageQueues <- genInitMessageQueues (taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues
        ++ getTestChannelsMessageQueues progArchitecture ++ getTestHarnessMessageQueues level)

    channelConnections <- genChannelConnections progArchitecture
    enableProtection <- genEnableProtection progArchitecture

    initialEventFunction <- (case find (\case { TPSystemInitEmitter {} -> True; _ -> False }) (emitters progArchitecture) of
        Just systemInitEmitter@(TPSystemInitEmitter {}) ->
            genInitalEventFunction progArchitecture systemInitEmitter
        _ -> return [])

    -- | A test adds its record and its harness, and a unit test the reset
    testRecord <- if testBuild then genTestRecord stamp else return []
    testReset <- if level == Just UnitTest then genTestReset progArchitecture (not (null initPools)) (not (null enableProtection)) else return []
    testHarnessTask <- case level of
        Just UnitTest -> genTestHarness progArchitecture
        Just ConcurrentTest -> genScenarioHarness
        Nothing -> return []
    let harnessDeclaration = [
                pre_cr $ CExtDecl (CEDFunction (Just CStatic) [] void testHarnessFunction
                    [CDecl (CTypeSpec (_const . ptr $ void)) (Just "arg") Nothing])
                    (internalAnn (CDeclarationAnn True))
            | testBuild ]

    -- | The steps are listed in the order in which they are called
    appInit <- genAppInit [
            InitStep (termina__app__init_msg_queues @@ ["status" @: (_const . ptr $ termina__error_code_t)]) True initMessageQueues,
            InitStep (termina__app__init_channel_connections @@ []) False channelConnections,
            InitStep (termina__app__init_pools @@ ["status" @: (_const . ptr $ termina__error_code_t)]) True initPools,
            InitStep (termina__app__initial_event @@ []) False initialEventFunction,
            InitStep (termina__app__init_mutexes @@ ["status" @: (_const . ptr $ termina__error_code_t)]) True initMutexes,
            InitStep (termina__app__enable_protection @@ []) False enableProtection,
            InitStep (termina__app__init_emitters @@ ["status" @: (_const . ptr $ termina__error_code_t)]) True initEmitters,
            InitStep (termina__app__init_handlers @@ []) False initHandlers,
            InitStep (termina__app__init_tasks @@ ["status" @: (_const . ptr $ termina__error_code_t)]) True initTasks
        ]

    return $ CSourceFile mName $ [
            -- #include <termina.h>
            includeTermina,
            -- | The identifiers of the deployment. The OSAL API does not
            -- include config.h, so that the modules do not depend on it.
            CPPDirective (CPPInclude False "config.h") (internalAnn (CPPDirectiveAnn False))
        ] ++ includes
        ++ testRecord
        ++ cPoolMemoryAreas
        ++ harnessDeclaration
        ++ initTasks ++ initHandlers ++ initEmitters ++ initMutexes ++ initPools ++ initMessageQueues
        ++ enableProtection ++ channelConnections ++ initialEventFunction
        ++ testReset ++ testHarnessTask
        ++ [appInit]

    where
        -- | List of modules that must be included
        incs = getGlobDeclModules progArchitecture
        -- | List of include directives
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs

runGenMainFile ::
    TerminaConfig
    -> Platform
    -> QualifiedName
    -> Word32 -- ^ the stamp of the descriptor of a unit test
    -> TerminaProgArch SemanticAnn
    -> Either CGeneratorError CFile
runGenMainFile config plt mainFilePath stamp progArchitecture =
    case runState (runExceptT (genMainFile mainFilePath stamp progArchitecture))
        (CGeneratorEnv mainFilePath S.empty emptyMonadicTypes config plt False) of
    (Left err, _) -> Left err
    (Right cFile, _) -> Right cFile