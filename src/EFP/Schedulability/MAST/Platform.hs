module EFP.Schedulability.MAST.Platform where

import EFP.Schedulability.MAST.AST
import qualified Data.Map.Strict as M
import Configuration.Platform
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import EFP.Schedulability.MAST.Monad
import Control.Monad.Except
import qualified Data.Text as T
import Configuration.Configuration
import Utils.Annotations
import EFP.Schedulability.MAST.Errors
import EFP.Schedulability.MAST.Platform.RTEMS5LEON3QEMU
import Control.Monad.State
import EFP.Schedulability.MAST.Utils

getPlatform :: MASTGenMonad Platform
getPlatform = do
    config <- gets configParams
    let pltName = T.unpack $ platform config
    maybe (throwError . annotateError Internal $ EUnknownPlatform pltName) return $ checkPlatform pltName

getProcessingResources :: MASTGenMonad (M.Map Identifier MASTProcessingResource)
getProcessingResources = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ M.fromList [
            ("cpu0", MASTRegularProcessor
                { prName = "cpu0"
                , prSpeedFactor = 1.0
                , prWorstISRSwitch = 0.002
                , prMaxInterruptPriority = 15
                , prMinInterruptPriority = 0
                })
            ]
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getSchedulers :: MASTGenMonad (M.Map Identifier MASTScheduler)
getSchedulers = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ M.fromList [
                ("RTEMS", MASTPrimaryScheduler
                    "RTEMS"
                    (
                        MASTFixedPriority
                            0.0005 -- Worst-case context switch time
                            (256 + 16) -- Maximum priority
                            1      -- Minimum priority
                    )
                    "cpu0"
                )
            ]
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getTaskPriority :: TPTask a -> MASTGenMonad Priority
getTaskPriority task = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU ->
            let TInteger prio _ = getPriority task in
            return $ 256 - fromInteger prio
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getEmitterPriority :: TPEmitter a -> MASTGenMonad Priority
getEmitterPriority (TPInterruptEmittter emitterId _) = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> getRTEMS5LEON3QEMUIrqPriority emitterId
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)
getEmitterPriority (TPPeriodicTimerEmitter {}) = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> getRTEMS5LEON3QEMUTimerIrqPriority
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)
getEmitterPriority (TPSystemInitEmitter {}) = throwError . annotateError Internal $ EUnsupportedSystemInitEmitter
getEmitterPriority (TPSystemExceptEmitter {}) = throwError . annotateError Internal $ EUnsupportedSystemExceptEmitter

genEmitterSchedulingServer :: TPEmitter a -> MASTGenMonad MASTSchedulingServer
genEmitterSchedulingServer emitter = do
    let emitterID = getEmitterIdentifier emitter
    priority <- getEmitterPriority emitter
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            return $ MASTRegularSchedulingServer
                { serverName = emitterID
                , serverSchedParams = MASTFixedPrioPolicy priority
                , severScheduler = "RTEMS"
                }
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genTaskSchedulingServer :: TPTask a -> MASTGenMonad MASTSchedulingServer
genTaskSchedulingServer task = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            priority <- getTaskPriority task
            return $ MASTRegularSchedulingServer
                { serverName = taskName task
                , serverSchedParams = MASTFixedPrioPolicy priority
                , severScheduler = "RTEMS"
                }
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getProcessingResource :: Identifier -> MASTGenMonad Identifier
getProcessingResource _ = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return "cpu0"
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genEmitterOperation :: TPEmitter a -> MASTGenMonad MASTOperation
genEmitterOperation emitter = do
    let emitterId = getEmitterIdentifier emitter
    arch <- gets progArch
    emitterTarget <- case M.lookup emitterId (emitterTargets arch) of
        Just (target, _, _) -> return target
        Nothing -> throwError . annotateError Internal $ EUnknownEmitter emitterId
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> if M.member emitterTarget (tasks arch) then
            return $ MASTSimpleOperation
                    "__rtems_timer_top_half"
                    0.001 -- WCET
                    []    -- Resources to lock
                    []    -- Resources to unlock
            else if M.member emitterTarget (handlers arch) then
                return $ MASTSimpleOperation
                    "__rtems_interrupt_top_half"
                    0.001 -- WCET
                    []    -- Resources to lock
                    []    -- Resources to unlock
            else
                throwError . annotateError Internal $ EUnknownComponent emitterTarget
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genEmitterEventHandler :: Identifier -> TPEmitter a -> MASTGenMonad MASTEventHandler
genEmitterEventHandler eventId emitter = do
    let emitterId = getEmitterIdentifier emitter
    arch <- gets progArch
    (emitterTarget, emitterTargetPort) <- case M.lookup emitterId (emitterTargets arch) of
        Just (target, targetPort, _) -> return (target, targetPort)
        Nothing -> throwError . annotateError Internal $ EUnknownEmitter emitterId
    targetAction <- getTargetAction emitterTarget emitterTargetPort
    let outputEventName = getInternalEventName emitterTarget targetAction
    plt <- getPlatform
    operationName <- case plt of
        RTEMS5LEON3QEMU -> if M.member emitterTarget (tasks arch) then
                return "__rtems_timer_top_half"
            else if M.member emitterTarget (handlers arch) then
                return "__rtems_interrupt_top_half"
            else
                throwError . annotateError Internal $ EUnknownComponent emitterTarget
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)
    return $ MASTActivityEventHandler
        eventId -- ^ Input event name
        outputEventName -- ^ Output event name
        operationName -- ^ Operation name
        emitterTarget




