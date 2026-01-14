module EFP.Schedulability.MAST.Platform.RTEMS5LEON3QEMU where

import EFP.Schedulability.MAST.AST
import EFP.Schedulability.MAST.Monad
import Control.Monad.Except
import Utils.Annotations
import EFP.Schedulability.MAST.Errors
import EFP.Schedulability.MAST.Utils
import qualified Data.Map.Strict as M
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils

getProcessingResources :: M.Map Identifier MASTProcessingResource
getProcessingResources = M.fromList [
    ("cpu0", MASTRegularProcessor
        { prName = "cpu0"
        , prSpeedFactor = 1.0
        , prWorstISRSwitch = 1.0E-5
        , prMaxInterruptPriority = 256 + 16
        , prMinInterruptPriority = 1
        , prSytemTimer = MASTTicker
            1.0E-5 -- Worst overhead
            0.01  -- Tick interval
        })
    ]

getSchedulers :: M.Map Identifier MASTScheduler
getSchedulers = M.fromList [
        ("RTEMS", MASTPrimaryScheduler
            "RTEMS"
            (
                MASTFixedPriority
                    1.0E-5 -- Worst-case context switch time
                    (256 + 16) -- Maximum priority
                    1      -- Minimum priority
            )
            "cpu0"
        )
    ]

getIrqPriority :: Identifier -> MASTGenMonad Priority
getIrqPriority emitterId = case emitterId of
    "irq_1" -> return $ 256 + 16 - 1
    "irq_2" -> return $ 256 + 16 - 2
    "irq_3" -> return $ 256 + 16 - 3
    "irq_4" -> return $ 256 + 16 - 4
    "irq_5" -> return $ 256 + 16 - 5
    "irq_6" -> return $ 256 + 16 - 6
    "irq_7" -> return $ 256 + 16 - 7
    "irq_8" -> return $ 256 + 16 - 8
    "irq_9" -> return $ 256 + 16 - 9
    "irq_10" -> return $ 256 + 16 - 10
    "irq_11" -> return $ 256 + 16 - 11
    "irq_12" -> return $ 256 + 16 - 12
    "irq_13" -> return $ 256 + 16 - 13
    "irq_14" -> return $ 256 + 16 - 14
    "irq_15" -> return $ 256 + 16 - 15
    _ -> throwError . annotateError Internal $ EUnknownEmitter emitterId

getTaskPriority :: TPTask a -> Priority
getTaskPriority task =
    let TInteger prio _ = getPriority task in
    256 - fromInteger prio

genTaskSchedulingServer :: TPTask a -> MASTSchedulingServer
genTaskSchedulingServer task = do
    let priority = getTaskPriority task
    MASTRegularSchedulingServer
        { serverName = taskName task
        , serverSchedParams = MASTFixedPrioPolicy priority
        , severScheduler = "RTEMS"
        }

getProcessingResource :: Identifier -> Identifier
getProcessingResource _ = "cpu0"

getTimerIrqPriority :: MASTGenMonad Priority
getTimerIrqPriority = return $ 256 + 16 - 8

genIrqLockOperation :: Identifier -> MASTOperation
genIrqLockOperation resourceId = 
    let opId = getIrqLockMASTOperationId resourceId
    in 
        MASTSimpleOperation
            opId
            1.0E-5
            [resourceId] []

genIrqUnlockOperation :: Identifier -> MASTOperation
genIrqUnlockOperation resourceId = 
    let opId = getIrqUnlockMASTOperationId resourceId
    in 
        MASTSimpleOperation
            opId
            1.0E-5
            [] [resourceId]

genIrqSharedResource :: Identifier -> MASTSharedResource
genIrqSharedResource resourceId =
    MASTImmediateCeilingResource
        resourceId -- ^ Resource name
        (256 + 16)-- ^ Ceiling priority

genMutexLockOperation :: Identifier -> MASTOperation
genMutexLockOperation resourceId = 
    let opId = getMutexLockMASTOperationId resourceId
    in 
        MASTSimpleOperation
            opId
            2.0E-5
            [resourceId] []

genMutexUnlockOperation :: Identifier -> MASTOperation
genMutexUnlockOperation resourceId = 
    let opId = getMutexUnlockMASTOperationId resourceId
    in 
        MASTSimpleOperation
            opId
            2.0E-5
            [] [resourceId]

genMutexSharedResource :: Identifier -> AnyPriority -> MASTSharedResource
genMutexSharedResource resourceId ceil = 
    MASTImmediateCeilingResource
        resourceId -- ^ Resource name
        (256 - ceil) -- ^ Ceiling priority

genUnprotectedAllocBoxOperation :: Identifier -> MASTOperation
genUnprotectedAllocBoxOperation poolId = 
    let opId = getAllocBoxMASTOperationId poolId
    in 
        MASTSimpleOperation
            opId
            1.0E-4
            [] []

genUnprotectedFreeBoxOperation :: Identifier -> MASTOperation
genUnprotectedFreeBoxOperation poolId = 
    let opId = getFreeBoxMASTOperationId poolId
    in 
        MASTSimpleOperation
            opId
            1.0E-4
            [] []

genIrqLockAllocBoxOperation :: Identifier -> MASTOperation
genIrqLockAllocBoxOperation poolId = 
    let opId = getAllocBoxMASTOperationId poolId
    in 
        MASTSimpleOperation
            opId
            1.2E-4
            [poolId] [poolId]

genIrqUnlockFreeBoxOperation :: Identifier -> MASTOperation
genIrqUnlockFreeBoxOperation poolId = 
    let opId = getFreeBoxMASTOperationId poolId
    in 
        MASTSimpleOperation
            opId
            1.2E-4
            [poolId] [poolId]

genMutexLockAllocBoxOperation :: Identifier -> MASTOperation
genMutexLockAllocBoxOperation poolId = 
    let opId = getAllocBoxMASTOperationId poolId
    in 
        MASTSimpleOperation
            opId
            1.2E-4
            [poolId] [poolId]

genMutexUnlockFreeBoxOperation :: Identifier -> MASTOperation
genMutexUnlockFreeBoxOperation poolId = 
    let opId = getFreeBoxMASTOperationId poolId
    in 
        MASTSimpleOperation
            opId
            1.2E-4
            [poolId] [poolId]

timerTopHalfMASTOperation :: MASTOperation
timerTopHalfMASTOperation = 
    MASTSimpleOperation
        timerTopHalfMASTOperationId
        1.0E-5
        [] []

timerTopHalfSchedulingServer :: MASTSchedulingServer
timerTopHalfSchedulingServer = 
    MASTRegularSchedulingServer
        timerTopHalfSchedulingServerId
        (MASTFixedPrioPolicy  (256 + 16 - 8))
        "RTEMS"

irqTopHalfMASTOperation :: Identifier -> MASTOperation
irqTopHalfMASTOperation emitterId = 
    let opId = irqTopHalfMASTOperationId emitterId
    in 
        MASTSimpleOperation
            opId
            1.0E-5
            [] []

genIrqHandlerSchedulingServer :: Identifier -> MASTGenMonad MASTSchedulingServer
genIrqHandlerSchedulingServer emitterId = 
    getIrqPriority emitterId >>= \prio ->
        return $ MASTRegularSchedulingServer
            (irqHandlerSchedulerServerId emitterId)
            (MASTIrqFixedPrioPolicy prio)
            "RTEMS"

genSystemCallMASTOperations :: [(Identifier, MASTOperation)]
genSystemCallMASTOperations = 
    [ (getSystemCallMASTOperationId "clock_get_uptime", MASTSimpleOperation
        (getSystemCallMASTOperationId "clock_get_uptime")
        5.0E-5
        [] []
      )
    ]