module EFP.Schedulability.MAST.Platform where

import EFP.Schedulability.MAST.AST
import qualified Data.Map.Strict as M
import Configuration.Platform
import ControlFlow.Architecture.Types
import EFP.Schedulability.MAST.Monad
import Control.Monad.Except
import qualified Data.Text as T
import Configuration.Configuration
import Utils.Annotations
import EFP.Schedulability.MAST.Errors
import qualified EFP.Schedulability.MAST.Platform.RTEMS5LEON3QEMU as RTEMS5LEON3QEMU
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
        RTEMS5LEON3QEMU -> return RTEMS5LEON3QEMU.getProcessingResources
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getSchedulers :: MASTGenMonad (M.Map Identifier MASTScheduler)
getSchedulers = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return RTEMS5LEON3QEMU.getSchedulers
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getTaskPriority :: TPTask a -> MASTGenMonad Priority
getTaskPriority task = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ RTEMS5LEON3QEMU.getTaskPriority task
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getEmitterPriority :: TPEmitter a -> MASTGenMonad Priority
getEmitterPriority (TPInterruptEmitter emitterId _) = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> RTEMS5LEON3QEMU.getIrqPriority emitterId
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)
getEmitterPriority (TPPeriodicTimerEmitter {}) = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> RTEMS5LEON3QEMU.getTimerIrqPriority
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)
getEmitterPriority (TPSystemInitEmitter {}) = throwError . annotateError Internal $ EUnsupportedSystemInitEmitter
getEmitterPriority (TPSystemExceptEmitter {}) = throwError . annotateError Internal $ EUnsupportedSystemExceptEmitter

genTaskSchedulingServer :: TPTask a -> MASTGenMonad MASTSchedulingServer
genTaskSchedulingServer task = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ RTEMS5LEON3QEMU.genTaskSchedulingServer task
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

getProcessingResource :: Identifier -> MASTGenMonad Identifier
getProcessingResource componentId = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ RTEMS5LEON3QEMU.getProcessingResource componentId
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genIrqLockSharedResource :: Identifier -> MASTGenMonad ()
genIrqLockSharedResource resourceId = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            let irqLockOperation = RTEMS5LEON3QEMU.genIrqLockOperation resourceId
            insertOperation irqLockOperation
            let irqUnlockOperation = RTEMS5LEON3QEMU.genIrqUnlockOperation resourceId
            insertOperation irqUnlockOperation
            let sharedResource = RTEMS5LEON3QEMU.genIrqSharedResource resourceId
            insertSharedResource sharedResource
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genMutexLockSharedResource :: Identifier -> AnyPriority -> MASTGenMonad ()
genMutexLockSharedResource resourceId ceil = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            let mutexLockOperation = RTEMS5LEON3QEMU.genMutexLockOperation resourceId
            insertOperation mutexLockOperation
            let mutexUnlockOperation = RTEMS5LEON3QEMU.genMutexUnlockOperation resourceId
            insertOperation mutexUnlockOperation
            let sharedResource = RTEMS5LEON3QEMU.genMutexSharedResource resourceId ceil
            insertSharedResource sharedResource
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genUnprotectedPool :: Identifier -> MASTGenMonad ()
genUnprotectedPool poolId = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            insertOperation $ RTEMS5LEON3QEMU.genUnprotectedAllocBoxOperation poolId
            insertOperation $ RTEMS5LEON3QEMU.genUnprotectedFreeBoxOperation poolId
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genIrqLockSharedPool :: Identifier -> MASTGenMonad ()
genIrqLockSharedPool poolId = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            insertOperation $ RTEMS5LEON3QEMU.genIrqLockAllocBoxOperation poolId
            insertOperation $ RTEMS5LEON3QEMU.genIrqUnlockFreeBoxOperation poolId
            let sharedResource = RTEMS5LEON3QEMU.genIrqSharedResource poolId
            insertSharedResource sharedResource
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genMutexLockSharedPool :: Identifier -> AnyPriority -> MASTGenMonad ()
genMutexLockSharedPool poolId ceil = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> do
            insertOperation $ RTEMS5LEON3QEMU.genMutexLockAllocBoxOperation poolId
            insertOperation $ RTEMS5LEON3QEMU.genMutexUnlockFreeBoxOperation poolId
            let sharedResource = RTEMS5LEON3QEMU.genMutexSharedResource poolId ceil
            insertSharedResource sharedResource
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genTimerTopHalfMASTOperation :: MASTGenMonad MASTOperation
genTimerTopHalfMASTOperation = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return RTEMS5LEON3QEMU.timerTopHalfMASTOperation
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genTimerTopHalfSchedulingServer :: MASTGenMonad MASTSchedulingServer
genTimerTopHalfSchedulingServer = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return RTEMS5LEON3QEMU.timerTopHalfSchedulingServer
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genIrqTopHalfMASTOperation :: Identifier -> MASTGenMonad MASTOperation
genIrqTopHalfMASTOperation emitterId = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ RTEMS5LEON3QEMU.irqTopHalfMASTOperation emitterId
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genIrqHandlerSchedulingServer :: Identifier -> MASTGenMonad MASTSchedulingServer
genIrqHandlerSchedulingServer emitterId = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> RTEMS5LEON3QEMU.genIrqHandlerSchedulingServer emitterId
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)

genSystemCallMASTOperations :: MASTGenMonad (M.Map Identifier MASTOperation)
genSystemCallMASTOperations = do
    plt <- getPlatform
    case plt of
        RTEMS5LEON3QEMU -> return $ M.fromList RTEMS5LEON3QEMU.genSystemCallMASTOperations
        _ -> throwError . annotateError Internal $ EUnsupportedPlatform (show plt)