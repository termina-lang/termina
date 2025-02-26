{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Platform.RTEMS5LEON3TSIM.Glue where

import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.CodeGen.Common
import Control.Monad.Reader (runReader)
import Modules.Modules (QualifiedName)
import Semantic.Types
import ControlFlow.Architecture.Types
import Generator.CodeGen.Application.Types
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Types
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Glue
import Control.Monad.Except (runExceptT, MonadError(throwError))
import Configuration.Configuration
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Utils
import Generator.CodeGen.Application.Platform.RTEMS5NoelSpike.Types
import Generator.LanguageC.Embedded
import ControlFlow.Architecture.Utils
import System.FilePath
import Generator.CodeGen.SystemCall

-- | Function __rtems_app__install_emitters. This function is called from the Init task.
-- The function installs the ISRs and the periodic timers. The function is called AFTER the initialization
-- of the tasks and handlers.
genInstallEmitters :: [TPEmitter a] -> CGenerator CFileItem
genInstallEmitters progEmitters = do
    installEmitters <- mapM genRTEMSInstallEmitter $ filter (\case { TPSystemInitEmitter {} -> False; _ -> True }) progEmitters
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "install_emitters")
            [
                "current" @: (_const . ptr $ _TimeVal)
            ] @-> void $
            trail_cr . block $ 
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                pre_cr (var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code) : installEmitters

    where

        genRTEMSInstallEmitter :: TPEmitter a -> CGenerator CCompoundBlockItem
        genRTEMSInstallEmitter (TPInterruptEmittter interrupt _) = do
            return $
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                    $ trail_cr . block $ [
                        pre_cr $ "status" @: rtems_status_code @= __rtems__install_isr @@
                            [
                                dec (emitterToArrayMap M.! interrupt) @: uint32_t,
                                namefy ("rtems_isr" <::> interrupt) @: rtems_interrupt_handler
                            ]
                    ]
        genRTEMSInstallEmitter (TPPeriodicTimerEmitter timer _ _) = do
            let cExpr = CVar timer (CTTypeDef "PeriodicTimer" noqual)
            armTimer <- genArmTimer cExpr timer
            return $
                pre_cr $ _if ("RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                    $ block $ 
                        pre_cr (((timer @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "current" @: _TimeVal @=
                            deref ("current" @: (_const . ptr $ _TimeVal))) : armTimer
        genRTEMSInstallEmitter (TPSystemInitEmitter {}) = throwError $ InternalError "Initial event does not have to be installed"

genEmitter :: TerminaProgArch a -> TPEmitter SemanticAnn -> CGenerator CFileItem
genEmitter progArchitecture (TPInterruptEmittter interrupt _) = do
    let irqArray = emitterToArrayMap M.! interrupt
    -- | Obtain the identifier of the target entity and the port to which the
    -- interrupt emitter is connected
    (targetEntity, targetPort) <- case M.lookup interrupt (emitterTargets progArchitecture) of
        Just (entity, port, _) -> return (entity, port)
        -- | If the interrupt emitter is not connected, throw an error
        Nothing -> throwError $ InternalError $ "Interrupt emitter not connected: " ++ show interrupt
    -- | Now we have to check if the target entity is a task or a handler
    case M.lookup targetEntity (handlers progArchitecture) of
        Just (TPHandler identifier classId _ _ _ _ _ _) -> do
            let cls = handlerClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort
                classIdType = typeDef classId
            return $ pre_cr $ function (namefy "rtems_isr" <::> interrupt) ["_ignored" @: uint32_t] @-> void $
                    trail_cr . block $ [
                        -- classId * self = &identifier;
                        pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                        -- Result result; 
                        pre_cr $ var "result" _Result,
                        -- result.__variant = Result__Ok;
                        no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                        -- result = classFunctionName(self, interrupt);
                        pre_cr $ "result" @: _Result @=
                            irq_handler classId targetAction @@
                                [
                                    "self" @: ptr classIdType,
                                    dec irqArray @: uint32_t
                                ],
                        -- if (result.__variant != Result__Ok)
                        pre_cr $ _if (
                                (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                            $ block [
                                -- rtems_shutdown_executive(1);
                                no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                            ],
                        pre_cr $ _return Nothing
                    ]
        Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
            Just (TPTask {}) -> do
                return $ pre_cr $ function (namefy "rtems_isr" <::> interrupt) ["_ignored" @: uint32_t] @-> void $
                    block [
                        -- rtems_status_code status = RTEMS_SUCCESSFUL;
                        pre_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code,
                        -- uint32_t vector = interrupt;
                        pre_cr $ var "vector" uint32_t @:= dec irqArray @: uint32_t,
                        -- status = rtems_message_queue_send(interrupt.sink_msgq_id, &interrupt.task_port, sizeof(uint32_t));
                        pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                            [
                                (interrupt @: __rtems_interrupt_emitter_t) @. "sink_msgq_id" @: rtems_id,
                                addrOf ((interrupt @: __rtems_interrupt_emitter_t) @. "vector" @: uint32_t),
                                _sizeOfType uint32_t
                            ],
                        -- if (RTEMS_SUCCESSFUL == status)
                        pre_cr $ _if (
                                "RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                            $ block [
                                -- status = rtems_message_queue_send(interrupt.task_msgq_id, &vector, sizeof(uint32_t));
                                pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                                    [
                                        (interrupt @: __rtems_interrupt_emitter_t) @. "task_msgq_id" @: rtems_id,
                                        (interrupt @: __rtems_interrupt_emitter_t) @. "task_port" @: uint32_t,
                                        _sizeOfType uint32_t
                                    ],
                                -- if (RTEMS_SUCCESSFUL != status)
                                pre_cr $ _if (
                                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                                    $ block [
                                        -- rtems_shutdown_executive(1);
                                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                                    ]
                            ],
                        -- return;
                        pre_cr $ _return Nothing
                    ]
            Nothing -> throwError $ InternalError $ "Invalid connection for interrupt: " ++ show targetEntity
genEmitter progArchitecture (TPPeriodicTimerEmitter timer _ _) = do
    armTimer <- genArmTimer (CVar timer (CTTypeDef "PeriodicTimer" noqual)) timer
    (targetEntity, targetPort) <- case M.lookup timer (emitterTargets progArchitecture) of
        Just (entity, port, _) -> return (entity, port)
        -- | If the interrupt emitter is not connected, throw an error
        Nothing -> throwError $ InternalError $ "Periodic timer emitter not connected: " ++ show timer
    -- | Now we have to check if the target entity is a task or a handler
    case M.lookup targetEntity (handlers progArchitecture) of 
        Just (TPHandler identifier classId _ _ _ _ _ _) -> do
            let cls = handlerClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort 
            return $ pre_cr $ function (namefy "rtems_periodic_timer" <::> timer)
                [
                    "_timer_id" @: rtems_id,
                    "_ignored" @: void_ptr
                ] @-> void $
                trail_cr . block $ [
                    -- classId * self = &identifier;
                    pre_cr $ var "self" (ptr classId) @:= addrOf (identifier @: typeDef classId),
                    -- Result result;
                    pre_cr $ var "result" _Result,
                    -- result.__variant = Result__Ok;
                    no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                    -- result = classFunctionName(self, timer.current);
                    pre_cr $ "result" @: _Result @= irq_handler classId targetAction @@
                        [
                            "self" @: ptr classId,
                            ((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "current" @: _TimeVal
                        ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ] ++ armTimer ++ [
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                            "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ],
                    pre_cr $ _return Nothing
                ]
        Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
            Just (TPTask {}) -> return $ pre_cr $ function (namefy "rtems_periodic_timer" <::> timer)
                [
                    "_timer_id" @: rtems_id,
                    "_ignored" @: void_ptr
                ] @-> void $
                trail_cr . block $ [
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    pre_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code,
                    -- status = rtems_message_queue_send(timer.timer.sink_msg_id, &timer.current, sizeof(TimeVal));
                    pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                        [
                            ((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "sink_msgq_id" @: rtems_id,
                            addrOf (((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "current" @: _TimeVal),
                            _sizeOfType _TimeVal
                        ],
                    -- if (RTEMS_SUCCESSFUL == status)
                    pre_cr $ _if (
                            "RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                        $ block [
                            -- status = rtems_message_queue_send(timer.__timer.task_msgq_id, &timer.__timer.task_port, sizeof(uint32_t));
                            pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                                [
                                    ((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "task_msgq_id" @: rtems_id,
                                    addrOf (((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "task_port" @: uint32_t),
                                    _sizeOfType uint32_t
                                ]
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                            "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ] ++ armTimer ++ [
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                            "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ],
                    pre_cr $ _return Nothing
                ]
            Nothing -> throwError $ InternalError $ "Invalid connection for timer: " ++ show targetEntity
genEmitter progArchitecture (TPSystemInitEmitter systemInit _) = do
    (targetEntity, targetPort) <- case M.lookup systemInit (emitterTargets progArchitecture) of
        Just (entity, port, _) -> return (entity, port)
        -- | If the interrupt emitter is not connected, throw an error
        Nothing -> throwError $ InternalError $ "System init emitter not connected: " ++ show systemInit
    -- | Now we have to check if the target entity is a task or a handler
    case M.lookup targetEntity (handlers progArchitecture) of     
        Just (TPHandler identifier classId _ _ _ _ _ _) -> do
            let cls = handlerClasses progArchitecture M.! classId
                (_, targetAction) = sinkPorts cls M.! targetPort
                classIdType = typeDef classId
            return $ pre_cr $ function (namefy "rtems_app" <::> "initial_event") [
                    "current" @: (_const . ptr $ _TimeVal)
                ] @-> void $
                    trail_cr . block $ [
                        -- classId * self = &identifier;
                        pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                        -- Result result;
                        pre_cr $ var "result" _Result,
                        -- result.__variant = Result__Ok;
                        no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                        -- result = classFunctionName(self, current);
                        pre_cr $ "result" @: _Result @=
                            timer_handler classId targetAction @@
                                [
                                    "self" @: ptr classIdType,
                                    deref ("current" @: (_const . ptr $ _TimeVal))
                                ],
                        -- if (result.__variant != Result__Ok)
                        pre_cr $ _if (
                                (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                            $ block [
                                -- rtems_shutdown_executive(1);
                                no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                            ],
                        pre_cr $ _return Nothing
                    ]
        Nothing -> case M.lookup targetEntity (tasks progArchitecture) of
            Just (TPTask identifier classId _ _ _ _ _ _ _) -> do
                let cls = taskClasses progArchitecture M.! classId
                    (_, targetAction) = sinkPorts cls M.! targetPort
                    classIdType = typeDef classId
                return $ pre_cr $ function (namefy "rtems_app" <::> "inital_event") [
                        "current" @: (_const . ptr $ _TimeVal)
                    ] @-> void $
                        block [
                            -- classId * self = &identifier;
                            pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                            -- Result result;
                            pre_cr $ var "result" _Result,
                            -- result.__variant = Result__Ok;
                            no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                            -- result = classFunctionName(self, current);
                            pre_cr $ "result" @: _Result @=
                                timer_handler classId targetAction @@
                                    [
                                        "self" @: ptr classIdType,
                                        deref ("current" @: (_const . ptr $ _TimeVal))
                                    ],
                            -- if (result.__variant != Result__Ok)
                            pre_cr $ _if (
                                    (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                                $ block [
                                    -- rtems_shutdown_executive(1);
                                    no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                                ],
                            pre_cr $ _return Nothing
                        ]
            Nothing -> throwError $ InternalError $ "Invalid connection for timer: " ++ show targetEntity


genMainFile :: QualifiedName 
    -> TerminaProgArch SemanticAnn 
    -> CGenerator CFile
genMainFile mName progArchitecture = do
    let includeRTEMS = CPPDirective (CPPInclude True "rtems.h") (internalAnn (CPPDirectiveAnn True))
        includeTermina = CPPDirective (CPPInclude True "termina.h") (internalAnn (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl (CEDFunction void (namefy $ "termina_app" <::> "init_globals") []) (internalAnn (CDeclarationAnn True))
        interruptEmittersToTasks = getInterruptEmittersToTasks progArchitecture
        connectedEmitters = getConnectedEmitters progArchitecture
        progTasks = M.elems $ tasks progArchitecture

    taskMessageQueues <- getTasksMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture

    resLockingMap <- genResLockingMap progArchitecture dependenciesMap
    let mutexes = [m | m <- M.elems resLockingMap, (\case{ RTEMSResourceLockMutex {} -> True; _ -> False }) m]

    cVariantsForTaskPorts <- concat <$> mapM genVariantsForTaskPorts (M.elems taskClss)
    cPoolMemoryAreas <- genPoolMemoryAreas (M.elems $ pools progArchitecture)
    cAtomicDeclarations <- genAtomicDeclarations (M.elems $ atomics progArchitecture)
    cAtomicArrayDeclarations <- genAtomicArrayDeclarations (M.elems $ atomicArrays progArchitecture)
    cInterruptEmitterDeclarations <- genInterruptEmitterDeclarations interruptEmittersToTasks
    cTaskClassesCode <- mapM genTaskClassCode (M.elems (taskClasses progArchitecture))
    cEmitters <- mapM (genEmitter progArchitecture) connectedEmitters
    enableProtection <- genEnableProtection progArchitecture resLockingMap 
    initGlobals <- genInitGlobals progArchitecture
    installEmitters <- genInstallEmitters connectedEmitters
    createTasks <- genCreateTasks progTasks
    initTask <- genInitTask connectedEmitters
    appConfig <- genAppConfig progArchitecture (taskMessageQueues ++ channelMessageQueues) mutexes
    return $ CSourceFile mName $ [
            -- #include <rtems.h>
            includeRTEMS,
            -- #include <termina.h>
            includeTermina
        ] ++ includes
        ++ [
            externInitGlobals
        ] ++ cVariantsForTaskPorts ++ cAtomicDeclarations ++ cAtomicArrayDeclarations 
        ++ cPoolMemoryAreas ++ cInterruptEmitterDeclarations
        ++ cTaskClassesCode ++ cEmitters ++ [enableProtection, initGlobals, installEmitters, createTasks, initTask]
        ++ appConfig

    where
        -- | List of modules that must be included
        incs = getGlobDeclModules progArchitecture
        -- | List of include directives
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs

        -- List of used task classes
        taskClss = taskClasses progArchitecture

        dependenciesMap = getResDependencies progArchitecture

runGenMainFile :: TerminaConfig -> QualifiedName -> TerminaProgArch SemanticAnn -> Either CGeneratorError CFile
runGenMainFile config mainFilePath progArchitecture = runReader (runExceptT (genMainFile mainFilePath progArchitecture)) (CGeneratorEnv M.empty config syscallFunctionsMap)
