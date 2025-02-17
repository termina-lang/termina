{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Glue where

import Generator.LanguageC.AST
import ControlFlow.BasicBlocks.AST
import qualified Data.Map as M
import Data.List (find)
import Generator.LanguageC.Embedded
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Types
import Generator.CodeGen.Application.Types
import Generator.CodeGen.Common
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Reader (runReader)
import Data.Text (unpack)
import Generator.LanguageC.Printer
import Semantic.Types
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils (getConnectedEmitters, getClassMembers)
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Utils
import Generator.CodeGen.Application.OS.RTEMS.Utils
import Generator.Utils

genInterruptEmitterDeclaration :: Bool -> TPEmitter SemanticAnn -> CGenerator CFileItem
genInterruptEmitterDeclaration before (TPInterruptEmittter identifier _ ) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType noqual (TStruct (namefy ("rtems" <::> "interrupt_emitter_t")))
    return $ CExtDecl (CEDVariable (Just CStatic) (CDecl (CTypeSpec cType) (Just identifier) Nothing)) declStmt
genInterruptEmitterDeclaration _ obj = throwError $ InternalError ("Invalid object (not an interrupt emitter): " ++ show obj)

genInterruptEmitterDeclarations :: [TPEmitter SemanticAnn] -> CGenerator [CFileItem]
genInterruptEmitterDeclarations [] = return []
genInterruptEmitterDeclarations (obj : objs) = do
    decl <- genInterruptEmitterDeclaration True obj
    rest <- mapM (genInterruptEmitterDeclaration False) objs
    return $ decl : rest

genTaskClassCode :: TPClass SemanticAnn -> CGenerator CFileItem
genTaskClassCode tskCls = do
    cBody <- genBody
    return $ pre_cr $ static_function (namefy "rtems_task" <::> classId) [
            "arg" @: rtems_task_argument
        ] @-> rtems_task $ trail_cr . block $
            cBody ++ [pre_cr (_return Nothing)]

    where

        classId = classIdentifier tskCls
        members = getClassMembers . classTypeDef $ tskCls

        actions :: [(Identifier, TerminaType, Identifier)]
        actions = foldl (\acc member ->
            case member of
                ClassField (FieldDefinition identifier (TSinkPort dts action)) _ -> (identifier, dts, action) : acc
                ClassField (FieldDefinition identifier (TInPort dts action)) _ -> (identifier, dts, action) : acc
                _ -> acc
            ) [] members

        getMsgDataVariable :: Bool -> Identifier -> TerminaType -> CGenerator CCompoundBlockItem
        getMsgDataVariable before action dts = do
            cDataType <- genType noqual dts
            if before then
                return $ pre_cr $ var (action <::> "msg_data") cDataType
            else
                return $ no_cr $ var (action <::> "msg_data") cDataType

        getMsgDataVariables :: [(Identifier, TerminaType, Identifier)] -> CGenerator [CCompoundBlockItem]
        getMsgDataVariables [] = return []
        getMsgDataVariables ((_identifier, dts, action) : xs) = do
            decl <- getMsgDataVariable True action dts
            rest <- mapM (uncurry (getMsgDataVariable False) . (\(_, dts', action') -> (action', dts'))) xs
            return $ decl : rest

        genCase :: (Identifier, TerminaType, Identifier) -> CGenerator [CCompoundBlockItem]
        genCase (port, dts, action) = do
            this_variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            classStructType <- genType noqual (TStruct classId)
            cDataType <- genType noqual dts
            let classFunctionType = CTFunction _Result
                    [_const . ptr $ classStructType, cDataType]
            return
                [
                    -- case variant:
                    pre_cr $ _case (this_variant @: enumFieldType) $
                    -- status = rtems_message_queue_receive(self->port, &action_msg_data, 
                    --                                      &size, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
                    indent . pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_receive @@
                            [
                                ("self" @: ptr classStructType) @. port @: rtems_id,
                                cast void_ptr (addrOf ((action <::> "msg_data") @: cDataType)),
                                addrOf ("size" @: size_t),
                                "RTEMS_NO_WAIT" @: rtems_option,
                                "RTEMS_NO_TIMEOUT" @: rtems_interval
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    indent . pre_cr $ _if (
                            "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ],
                    -- result = classFunctionName(self, action_msg_data);
                    indent . pre_cr $ "result" @: CTTypeDef "Result" noqual @=
                        (classFunctionName @: classFunctionType) @@ ["self" @: classStructType, (action <::> "msg_data") @: cDataType],
                    -- if (result.__variant != Result__Ok)
                    indent . pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ],
                    indent . pre_cr $ _break
                ]

        genLoop :: CGenerator CStatement
        genLoop = do
            classStructType <- genType noqual (TStruct classId)
            cases <- concat <$> mapM genCase actions
            return $ trail_cr . block $ [
                    -- | status = rtems_message_queue_receive(
                    -- |                self->__task.msgq_id, &next_msg, &size, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
                    pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_receive @@
                            [
                                (("self" @: ptr classStructType) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                                addrOf ("next_msg" @: uint32_t),
                                addrOf ("size" @: size_t),
                                "RTEMS_WAIT" @: rtems_option,
                                "RTEMS_NO_TIMEOUT" @: rtems_interval
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if
                            ("RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- break;
                            no_cr _break
                        ],
                    pre_cr $ _switch ("next_msg" @: uint32_t) $
                        trail_cr . block $ (cases ++
                            [
                                -- default:
                                pre_cr $ _default $
                                    -- rtems_shutdown_executive(1);
                                    indent . pre_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t],
                                    -- break;
                                    indent . pre_cr $ _break
                            ])
                ]

        genBody :: CGenerator [CCompoundBlockItem]
        genBody = do
            msgDataVars <- getMsgDataVariables actions
            loop <- genLoop
            return $ [
                    -- ClassIdentifier * self = (ClassIdentifier *)&arg;
                    pre_cr $ var "self" (ptr classId) @:= cast (ptr classId) ("arg" @: rtems_task_argument),
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    no_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code,
                    -- uint32_t next_msg = 0U;
                    no_cr $ var "next_msg" uint32_t @:= dec 0 @: uint32_t,
                    -- size_t size = 0U;
                    no_cr $ var "size" size_t @:= dec 0 @: size_t,
                    -- Result result;
                    pre_cr $ var "result" (typeDef "Result"),
                    -- result.__variant = Result__Ok;
                    no_cr $ ("result" @: typeDef "Result") @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
                ] ++ msgDataVars ++
                [
                    pre_cr $ _for Nothing Nothing Nothing loop,
                    pre_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                ]

emitterToArrayMap :: M.Map Identifier Integer
emitterToArrayMap = M.fromList [("irq_0", 0), ("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4)]

genArmTimer :: CObject -> Identifier -> CGenerator [CCompoundBlockItem]
genArmTimer cObj identifier = do
    return [
            -- __termina__add_timeval(&timer.__timer.current, timer.period);
            pre_cr $ __termina__add_timeval @@
                [
                    addrOf ((cObj @. "__timer" @: __termina_timer_t) @. "current" @: _TimeVal),
                    addrOf (cObj @. "period" @: _TimeVal)
                ],
            -- status = __rtems__timer_delay_at(timer.__timer.timer_id, 
            --                                  &timer.__timer.current,
            --                                  __rtems_periodic_timer__identifier);
            pre_cr $ "status" @: rtems_status_code @=  __rtems__timer_delay_at @@
                [
                    (cObj @. "__timer" @: __termina_timer_t) @. "timer_id" @: rtems_id,
                    addrOf ((cObj @. "__timer" @: __termina_timer_t) @. "current" @: _TimeVal),
                    (namefy "rtems_periodic_timer" <::> identifier) @: typeDef "rtems_timer_service_routine_entry"
                ]
        ]

-- | Function __rtems_app__enable_protection. This function is called from the Init task.
-- It enables the protection of the shared resources when needed. In case the resource uses a mutex,
-- it also initializes the mutex. The function is called AFTER the initialization of the tasks and handlers.
genEnableProtection :: TerminaProgArch a -> M.Map Identifier RTEMSResourceLock -> CGenerator CFileItem
genEnableProtection progArchitecture resLockingMap = do
    initResourcesProt <- concat <$> mapM genInitProt (M.toList resLockingMap)
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "enable_protection") [] @-> void $
        trail_cr . block $ [
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
            ] ++ initResourcesProt
    where

        genInitProt :: (Identifier, RTEMSResourceLock) -> CGenerator [CCompoundBlockItem]
        genInitProt (identifier, lock) = do
            case M.lookup identifier (resources progArchitecture) of
                Just glb -> genInitResourceProt (glb, lock)
                Nothing -> case M.lookup identifier (pools progArchitecture) of
                    Just glb -> genInitPoolProt (glb, lock)
                    -- | If the resource is not a regular resource nor a pool, there is no
                    -- need to initialize it
                    Nothing -> return [] 
        
        genInitResourceProt (TPResource identifier classId _ _ _, RTEMSResourceLockNone) = 
            return [
                pre_cr $ ((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "None" @: __rtems_runtime_resource_lock_t
                ]
        genInitResourceProt (TPResource identifier classId _ _ _, RTEMSResourceLockIrq) = do
            return [
                pre_cr $ ((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "Irq" @: __rtems_runtime_resource_lock_t
                ]
        genInitResourceProt (TPResource identifier classId _ _ _, RTEMSResourceLockMutex ceilPrio) = do
            let cCeilPrio = genInteger ceilPrio
            return [
                    -- | identifier.__resource.lock = RTEMSResourceLock__Mutex;
                    pre_cr $ ((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                        @= namefy "RTEMSResourceLock" <::> "Mutex" @: __rtems_runtime_resource_lock_t,
                    no_cr $
                        (((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "policy" @: __rtems_runtime_mutex_policy
                        @= namefy "RTEMSMutexPolicy" <::> "Ceiling" @: __rtems_runtime_mutex_policy,
                    no_cr $
                        (((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "prio_ceiling" @: rtems_task_priority
                        @= cCeilPrio @: rtems_task_priority,
                    pre_cr $ "result" @: _Result @= __termina_resource__init @@
                        [
                            addrOf ((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t)
                        ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ]

        genInitPoolProt (TPPool identifier _ _ _ _, RTEMSResourceLockNone) = do
            return [
                pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "None" @: __rtems_runtime_resource_lock_t
                ]
        genInitPoolProt (TPPool identifier _ _ _ _, RTEMSResourceLockIrq) = do
            return [
                pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "Irq" @: __rtems_runtime_resource_lock_t
                ]
        genInitPoolProt (TPPool identifier _ _ _ _, RTEMSResourceLockMutex ceilPrio) = do
            let cCeilPrio = genInteger ceilPrio
            return [
                    -- | identifier.__resource.lock = RTEMSResourceLock__Mutex;
                    pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                        @= namefy "RTEMSResourceLock" <::> "Mutex" @: __rtems_runtime_resource_lock_t,
                    no_cr $
                        (((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "policy" @: __rtems_runtime_mutex_policy
                        @= namefy "RTEMSMutexPolicy" <::> "Ceiling" @: __rtems_runtime_mutex_policy,
                    no_cr $
                        (((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "prio_ceiling" @: rtems_task_priority
                        @= cCeilPrio @: rtems_task_priority,
                    pre_cr $ "result" @: _Result @= __termina_resource__init @@
                        [
                            addrOf ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t)
                        ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ]

getInterruptEmittersToTasks :: TerminaProgArch a -> [TPEmitter a]
getInterruptEmittersToTasks progArchitecture = foldl (\acc emitter ->
    case emitter of
        TPInterruptEmittter identifier _ -> 
            case M.lookup identifier (emitterTargets progArchitecture) of
                Just (entity, _port, _) -> 
                    case M.lookup entity (tasks progArchitecture) of
                        Just _ -> emitter : acc
                        Nothing -> acc
                Nothing -> acc
        _ -> acc
    ) [] (getConnectedEmitters progArchitecture)

getPeriodicTimersToTasks :: TerminaProgArch a -> [TPEmitter a]
getPeriodicTimersToTasks progArchitecture = foldl (\acc emitter ->
    case emitter of
        TPPeriodicTimerEmitter identifier _ _ -> 
            case M.lookup identifier (emitterTargets progArchitecture) of
                Just (entity, _port, _) -> 
                    case M.lookup entity (tasks progArchitecture) of
                        Just _ -> emitter : acc
                        Nothing -> acc
                Nothing -> acc
        _ -> acc
    ) [] (getConnectedEmitters progArchitecture)

-- | Function __rtems_app__init_globals. This function is called from the Init task.
-- The function is called BEFORE the initialization of the tasks and handlers. The function disables
-- the protection of the global resources, since it is not needed when running in the Init task. 
genInitGlobals :: TerminaProgArch SemanticAnn
    -> CGenerator CFileItem
genInitGlobals progArchitecture  = do
    let interruptEmittersToTasks = getInterruptEmittersToTasks progArchitecture
        timersToTasks = getPeriodicTimersToTasks progArchitecture
        timers = filter (\case {TPPeriodicTimerEmitter {} -> True; _ -> False}) (getConnectedEmitters progArchitecture)

    tasksMessageQueues <- getTasksMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture
    initResources <- concat <$> mapM genInitResource (resources progArchitecture)
    initPools <- concat <$> mapM genInitPool (pools progArchitecture)
    cTaskMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue tasksMessageQueues
    cChannelMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue channelMessageQueues
    cInterruptEmittersToTasks <- concat <$> mapM genInitInterruptEmitterToTask interruptEmittersToTasks
    cTimersToTasks <- concat <$> mapM genInitTimerToTask timersToTasks
    cTaskInitialization <- concat <$> mapM genTaskInitialization (tasks progArchitecture)
    cCreateTimers <- concat <$> mapM genRTEMSCreateTimer timers
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "init_globals") [] @-> void $
            trail_cr . block $ [
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
            ] ++ initResources ++ initPools
            ++ [
                    pre_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code
                        | not (null tasksMessageQueues) || not (null channelMessageQueues) || not (null timers)]
                ++ cTaskMessageQueues ++ cChannelMessageQueues
                ++ cInterruptEmittersToTasks ++ cTimersToTasks ++ cTaskInitialization ++ cCreateTimers

    where

        genInitResource :: TPResource SemanticAnn -> CGenerator [CCompoundBlockItem]
        genInitResource (TPResource identifier classId _ _ _) = do
            -- | resource.__resource.lock = RTEMSResourceLock__None;
            return [pre_cr $ ((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "None" @: __rtems_runtime_resource_lock_t]

        genInitPool :: TPPool SemanticAnn -> CGenerator [CCompoundBlockItem]
        genInitPool (TPPool identifier ts _ _ _) = do
            cTs <- genType noqual ts
            return
                [
                    -- identifier.__resource.lock = __RTEMSResourceLock__None;
                    pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                        @= (namefy "RTEMSResourceLock" <::> "None") @: __rtems_runtime_resource_lock_t,
                    -- result = __termina_pool__init(&identifier, (void *)memory_area_identifier)
                    pre_cr $ "result" @: _Result @= __termina_pool__init @@
                                [
                                    addrOf (identifier @: ptr __termina_pool_t),
                                    cast (ptr void) (poolMemoryArea identifier @: ptr uint8_t),
                                    _sizeOfExpr (poolMemoryArea identifier @: ptr uint8_t),
                                    _sizeOfType cTs
                                ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ]

        -- | Prints the code to initialize a message queue. The function is called to generate the code for the
        -- message queues corresponding to the channels declared by the user plus the ones that belong to each
        -- of the tasks that is used to notify the inclusion of a given message on a specific queue.
        genRTEMSCreateMsgQueue :: RTEMSMsgQueue -> CGenerator [CCompoundBlockItem]
        genRTEMSCreateMsgQueue (RTEMSChannelMsgQueue identifier ts size taskId portId) = do
            cSize <- genArraySize size
            cTs <- genType noqual ts
            classId <- case M.lookup taskId (tasks progArchitecture) of
                Just task -> return (taskClass task)
                Nothing -> throwError $ InternalError $ "Invalid task id: " ++ show taskId
            variantForPort <- genVariantForPort classId portId
            let classIdType = typeDef classId
            return
                [
                    pre_cr $ (identifier @: __termina_msg_queue_t) @. "task_msgq_id" @: rtems_id @=
                        ((taskId @: classIdType) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                    no_cr $ (identifier @: __termina_msg_queue_t) @. "task_port" @: uint32_t @=
                        variantForPort @: uint32_t,
                    no_cr $ (identifier @: __termina_msg_queue_t) @. "message_size" @: size_t @=
                            _sizeOfType cTs,
                    -- status = __rtems__create_msg_queue(count, msg_size, &msg_queue_id)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_msg_queue @@
                        [
                            cSize,
                            _sizeOfType cTs,
                            addrOf ((identifier @: __termina_msg_queue_t) @. "msgq_id" @: rtems_id)
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
        genRTEMSCreateMsgQueue (RTEMSTaskMsgQueue taskId classId size) = do
            let classIdType = typeDef classId
            cSize <- genArraySize size
            return
                [
                    -- status = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_msg_queue @@
                        [
                            cSize,
                            _sizeOfType uint32_t,
                            addrOf (((taskId @: classIdType) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id)
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
        genRTEMSCreateMsgQueue (RTEMSSinkPortMsgQueue taskId classId portId ts size) = do
            cSize <- genArraySize size
            cTs <- genType noqual ts
            return
                [
                    -- status = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_msg_queue @@
                        [
                            cSize,
                            _sizeOfType cTs,
                            addrOf ((taskId @: typeDef classId) @. portId @: rtems_id)
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]

        genInitInterruptEmitterToTask :: TPEmitter SemanticAnn -> CGenerator [CCompoundBlockItem]
        genInitInterruptEmitterToTask (TPInterruptEmittter identifier _) = do
            let emitterClassId = "Interrupt"
            (targetEntity, targetPort) <- case M.lookup identifier (emitterTargets progArchitecture) of
                Just (entity, port, _) -> return (entity, port)
                -- | If the interrupt emitter is not connected, throw an error
                Nothing -> throwError $ InternalError $ "Interrupt emitter not connected: " ++ show identifier
            -- | Now we have to check if the target entity is a task
            case M.lookup targetEntity (tasks progArchitecture) of
                Just (TPTask taskId classId _ _ _ _ _ _ _) -> do
                    variantForPort <- genVariantForPort classId targetPort
                    return
                        [
                            pre_cr $ (identifier @: typeDef emitterClassId) @. "task_msgq_id" @: rtems_id @=
                                    ((taskId @: typeDef classId) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                            no_cr $ (identifier @: typeDef emitterClassId) @. "sink_msgq_id" @: rtems_id @=
                                    (taskId @: typeDef classId) @. targetPort @: rtems_id,
                            no_cr $ (identifier @: typeDef emitterClassId) @. "task_port" @: uint32_t @=
                                    variantForPort @: uint32_t
                        ]
                Nothing -> return []
        genInitInterruptEmitterToTask obj = throwError $ InternalError $ "Invalid global object (not an interrupt emitter): " ++ show obj

        genInitTimerToTask :: TPEmitter SemanticAnn -> CGenerator [CCompoundBlockItem]
        genInitTimerToTask (TPPeriodicTimerEmitter identifier _ _) = do
            (targetEntity, targetPort) <- case M.lookup identifier (emitterTargets progArchitecture) of
                Just (entity, port, _) -> return (entity, port)
                -- | If the interrupt emitter is not connected, throw an error
                Nothing -> throwError $ InternalError $ "Interrupt emitter not connected: " ++ show identifier
            -- | Now we have to check if the target entity is a task
            case M.lookup targetEntity (tasks progArchitecture) of 
                Just (TPTask taskId classId _ _ _ _ _ _ _) -> do
                    variantForPort <- genVariantForPort classId targetPort
                    return
                        [
                            pre_cr $ ((identifier @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "task_msgq_id" @: rtems_id @=
                                ((taskId @: typeDef classId) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                            no_cr $ ((identifier @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "sink_msgq_id" @: rtems_id @=
                                (taskId @: typeDef classId) @. targetPort @: rtems_id,
                            no_cr $ ((identifier @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "task_port" @: uint32_t @=
                                variantForPort @: uint32_t
                        ]
                Nothing -> return []

        genInitTimerToTask obj = throwError $ InternalError $ "Invalid global object (not a timer connected to a task): " ++ show obj

        genTaskInitialization :: TPTask SemanticAnn -> CGenerator [CCompoundBlockItem]
        genTaskInitialization (TPTask identifier classId inputPtConns _ _ _ _ _ _) = do
            mapM genInputPortInitialization $ M.toList inputPtConns

            where

                genInputPortInitialization :: (Identifier, (Identifier, SemanticAnn)) -> CGenerator CCompoundBlockItem
                genInputPortInitialization (portId, (channelId, _)) = do
                    return $ pre_cr $
                        identifier @: typeDef classId @. portId @: rtems_id @=
                            (channelId @: __termina_msg_queue_t) @. "msgq_id" @: rtems_id

        genRTEMSCreateTimer :: TPEmitter SemanticAnn  -> CGenerator [CCompoundBlockItem]
        genRTEMSCreateTimer (TPPeriodicTimerEmitter identifier _ _) = do
            return
                [
                    -- status = __rtems__create_timer(&timer_id)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_timer @@
                            [
                                addrOf (((identifier @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "timer_id" @: rtems_id)
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
        genRTEMSCreateTimer obj = throwError $ InternalError $ "Invalid global object (not a timer): " ++ show obj

genCreateTasks :: [TPTask a] -> CGenerator CFileItem
genCreateTasks progTasks = do
    createTasks <- concat <$> mapM genRTEMSCreateTask progTasks
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "create_tasks") [] @-> void $ 
        trail_cr . block $ 
            -- rtems_status_code status = RTEMS_SUCCESSFUL;
            pre_cr (var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code) : createTasks
    where

        genRTEMSCreateTask :: TPTask a -> CGenerator [CCompoundBlockItem]
        genRTEMSCreateTask (TPTask identifier classId _ _ _ _ modifiers _ _) = do
            let cPriority = genInteger . getPriority $ modifiers
                cStackSize = genInteger . getStackSize $ modifiers
            return [
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_task @@
                            [
                                cPriority @: rtems_task_priority,
                                cStackSize @: size_t,
                                namefy ("rtems_task" <::> classId) @: rtems_task_entry,
                                cast rtems_task_argument (addrOf (identifier @: typeDef classId)),
                                addrOf (((identifier @: typeDef classId) @. taskClassIDField @: __termina_task_t) @. "task_id" @: rtems_id)
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]

genInitTask :: [TPEmitter a] -> CGenerator CFileItem
genInitTask progEmitters = do
    return $ pre_cr $ function "Init" [
            "_ignored" @: rtems_task_argument
        ] @-> rtems_task $
        trail_cr . block $ 
            [
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                pre_cr $ var "current" _TimeVal,
                pre_cr $ __termina__clock_get_uptime @@ [addrOf ("current" @: _TimeVal)],
                pre_cr $ __termina_app__init_globals @@ [],
                pre_cr $ __rtems_app__init_globals @@ []
             ] ++
                (case find (\case { TPSystemInitEmitter {} -> True; _ -> False }) progEmitters of
                    Just (TPSystemInitEmitter {}) -> [
                            pre_cr $ __rtems_app__initial_event @@ [addrOf ("current" @: _TimeVal)]
                        ]
                    _ -> []) ++
                [
                    pre_cr $ __rtems_app__enable_protection @@ [],
                    pre_cr $ __rtems_app__install_emitters @@ [addrOf ("current" @: _TimeVal)],
                    pre_cr $ __rtems_app__create_tasks @@ [],
                    pre_cr $ rtems_task_delete @@ ["RTEMS_SELF" @: rtems_id]
                ]

genAppConfig ::
    TerminaProgArch SemanticAnn
    -> [RTEMSMsgQueue]
    -> [RTEMSResourceLock]
    -> CGenerator [CFileItem]
genAppConfig progArchitecture msgQueues mutexes = do
    let progTasks = M.elems $ tasks progArchitecture
        progTimers = M.elems . M.filter (\case { TPPeriodicTimerEmitter {} -> True; _ -> False }) $ emitters progArchitecture
    messageBufferMemory <- genMessageBufferMemory msgQueues
    return $ [
            -- #define CONFIGURE_MAXIMUM_TASKS
            pre_cr $ _define "CONFIGURE_MAXIMUM_TASKS" (Just [show (length progTasks + 1)]),
            -- #define CONFIGURE_MAXIMUM_MESSAGE_QUEUES
            _define "CONFIGURE_MAXIMUM_MESSAGE_QUEUES" (Just [show (length msgQueues)]),
            -- #define CONFIGURE_MAXIMUM_TIMERS
            _define "CONFIGURE_MAXIMUM_TIMERS" (Just [show (length progTimers)]),
            -- #define CONFIGURE_MAXIMUM_SEMAPHORES
            _define "CONFIGURE_MAXIMUM_SEMAPHORES" (Just [show (length mutexes)])
        ] ++ messageBufferMemory ++
        [
            pre_cr $ _define "CONFIGURE_APPLICATION_DOES_NOT_NEED_CONSOLE_DRIVER" Nothing,
            _define "CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER" Nothing,
            _define "CONFIGURE_MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)]),
            pre_cr $ _define "CONFIGURE_RTEMS_INIT_TASKS_TABLE" Nothing,
            pre_cr $ _define "CONFIGURE_INIT" Nothing,
            pre_cr $ _include True "rtems/confdefs.h"
        ]

    where

        genMessagesForQueue :: RTEMSMsgQueue -> CGenerator [String]
        genMessagesForQueue (RTEMSTaskMsgQueue _ _ size) = do
            cSize <- genArraySize size
            let cSizeOf = _sizeOfType uint32_t
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSChannelMsgQueue _ ts size _ _) = do
            cSize <- genArraySize size
            cTs <- genType noqual ts
            let cSizeOf = _sizeOfType cTs
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSSinkPortMsgQueue _ _ _ ts size) = do
            cSize <- genArraySize size
            cTs <- genType noqual ts
            let cSizeOf = _sizeOfType cTs
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]

        genMessagesForQueues :: [RTEMSMsgQueue] -> CGenerator [String]
        genMessagesForQueues [msgq] = genMessagesForQueue msgq
        genMessagesForQueues (msgq : xs) = do
            msgsForQueue <- genMessagesForQueue msgq
            msgsForQueues <- genMessagesForQueues xs
            return $ msgsForQueue ++ ["+ "] ++ msgsForQueues
        genMessagesForQueues [] = throwError $ InternalError "Invalid message queue list: empty list"

        genMessageBufferMemory :: [RTEMSMsgQueue] -> CGenerator [CFileItem]
        genMessageBufferMemory [] = return []
        genMessageBufferMemory msgq = do
            messagesForQueue <- genMessagesForQueues msgq
            return [
                    CPPDirective (CPPDefine "CONFIGURE_MESSAGE_BUFFER_MEMORY"
                        (Just $
                            "( " : messagesForQueue ++ [")"]
                        )) (internalAnn (CPPDirectiveAnn True))
                ]
