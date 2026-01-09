module EFP.Schedulability.MAST.AST 
    (module EFP.Schedulability.Core.AST
    , module EFP.Schedulability.MAST.AST) where

import EFP.Schedulability.Core.AST
import qualified Data.Map.Strict as M

type NormalizedExecutionTime = Double
type Time = Double
type InterruptPriority = Integer
type Priority = Integer
type AnyPriority = Integer
type SpeedFactor = Double

data MASTSchedulingPolicy =
    -- | Fixed Priority Scheduling Policy
    MASTFixedPriority
        NormalizedExecutionTime -- ^ Worst-case context switch time
        Priority -- ^ Maximum priority
        Priority -- ^ Minimum priority
    deriving Show

data MASTScheduler = 
    MASTPrimaryScheduler
        Identifier -- ^ Scheduler name
        MASTSchedulingPolicy -- ^ Scheduling policy
        Identifier -- ^ Host processing resource
    deriving Show

data MASTProcessingResource =
    MASTRegularProcessor
    { prName :: Identifier
    , prSpeedFactor :: SpeedFactor
    , prWorstISRSwitch :: NormalizedExecutionTime
    , prMaxInterruptPriority :: InterruptPriority
    , prMinInterruptPriority :: InterruptPriority
    }
    deriving Show

data MASTSchedParameters =
    MASTFixedPrioPolicy Priority
    | MASTIrqFixedPrioPolicy InterruptPriority
    deriving Show

data MASTSchedulingServer =
    MASTRegularSchedulingServer
    { serverName :: Identifier 
      , serverSchedParams :: MASTSchedParameters
      , severScheduler :: Identifier
    }
    deriving Show

data MASTSharedResource =
    MASTImmediateCeilingResource
        Identifier -- ^ Resource name
        AnyPriority -- ^ Ceiling priority
    deriving Show

data MASTOperation =
    MASTSimpleOperation
        Identifier -- ^ Operation's name
        NormalizedExecutionTime -- ^ Operation's WCET
        [Identifier] -- ^ Shared resources to lock
        [Identifier] -- ^ Shared resources to unlock
    | MASTCompositeOperation
        Identifier -- ^ Operation's name
        [Identifier] -- ^ Sub-operations
    | MASTEnclosingOperation
        Identifier -- ^ Operation's name
        NormalizedExecutionTime -- ^ Operation's WCET
        [Identifier] -- ^ Enclosed operations
    deriving Show

data MASTExternalEvent =
    MASTPeriodicExternalEvent
        Identifier -- ^ Event's name
        Time -- ^ Period
    | MASTBurstyExternalEvent
        Identifier -- ^ Event's name
        Time -- ^ Bound interval 
        Integer -- ^ Maximum number of events in burst
    deriving Show

data MASTTimingRequirement =
    MASTGlobalDeadline
        Time -- ^ Deadline
        Identifier -- ^ Referenced event
    deriving Show

data MASTInternalEvent =
    MASTRegularInternalEvent
        Identifier -- ^ Event's name
        (Maybe MASTTimingRequirement) -- ^ Timing requirement
    deriving Show

data MASTEventHandler =
    MASTTimedActivityEventHandler
        Identifier -- ^ Input event
        Identifier -- ^ Output event
        Identifier -- ^ Operation
        Identifier -- ^ Scheduling server
    | MASTActivityEventHandler
        Identifier -- ^ Input event
        Identifier -- ^ Output event
        Identifier -- ^ Operation
        Identifier -- ^ Scheduling server
    | MASTMulticastEventHandler
        Identifier -- ^ Input event
        [Identifier] -- ^ Output events
    deriving Show

data MASTTransaction =
    MASTRegularTransaction
        Identifier -- ^ Transaction's name
        [MASTExternalEvent] -- ^ External events
        [MASTInternalEvent] -- ^ Internal events
        [MASTEventHandler] -- ^ Event handlers
    deriving Show

data MASTModel = MASTModel
    { mastModelName :: Identifier
    , mastProcessingResources :: M.Map Identifier MASTProcessingResource
    , mastSchedulers :: M.Map Identifier MASTScheduler
    , mastSchedServers :: M.Map Identifier MASTSchedulingServer
    , mastSharedResources :: M.Map Identifier MASTSharedResource
    , mastOperations :: M.Map Identifier MASTOperation
    , mastTransactions :: M.Map Identifier MASTTransaction
    }
    deriving Show