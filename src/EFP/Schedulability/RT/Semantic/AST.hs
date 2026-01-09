module EFP.Schedulability.RT.Semantic.AST 
    (module EFP.Schedulability.RT.Semantic.AST
    , module EFP.Schedulability.Core.AST) where

import EFP.Schedulability.Core.AST

import Utils.Annotations
import qualified Data.Map.Strict as M

type RTDeadlineMap a = M.Map Identifier Double

data RTTransStep a = 
    RTTransStepAction
        Identifier -- ^ Step name
        Identifier -- ^ Task or handler name
        Identifier -- ^ Action name
        Identifier -- ^ Path name
        (RTTransStep a) -- ^ Next step (if any)
        a -- ^ Annotation
    | RTTransStepMuticast 
        [RTTransStep a] -- ^ Steps to multicast to
        a -- ^ Annotation
    | RTTransStepConditional
        [(TInteger, RTTransStep a)] -- ^ Condition and corresponding step
        a -- ^ Annotation
    | RTTransStepEnd Identifier a -- ^ End step
    deriving Show

instance Annotated RTTransStep where

    getAnnotation (RTTransStepAction _ _ _ _ _ a) = a
    getAnnotation (RTTransStepMuticast _ a) = a
    getAnnotation (RTTransStepConditional _ a) = a
    getAnnotation (RTTransStepEnd _ a) = a

    updateAnnotation (RTTransStepAction name task action path next _) = RTTransStepAction name task action path next
    updateAnnotation (RTTransStepMuticast steps _) = RTTransStepMuticast steps
    updateAnnotation (RTTransStepConditional conds _) = RTTransStepConditional conds
    updateAnnotation (RTTransStepEnd name _) = RTTransStepEnd name

data RTEvent a =
    RTEventBursty 
        Identifier -- ^ Event identifier
        Identifier -- ^ Emitter identifier
        Identifier -- ^ Transaction identifier
        TInteger -- ^ Interval
        TInteger -- ^ Arrivals expression
        (RTDeadlineMap a) -- ^ Deadlines map
        a -- ^ Annotation
    | RTEventPeriodic
        Identifier -- ^ Event identifier
        Identifier -- ^ Emitter identifier
        Identifier -- ^ Transaction identifier
        (RTDeadlineMap a) -- ^ Deadlines map
        a -- ^ Annotation
    deriving Show

instance Annotated RTEvent where
    getAnnotation (RTEventBursty _ _ _ _ _ _ a) = a
    getAnnotation (RTEventPeriodic _ _ _ _ a) = a

    updateAnnotation (RTEventBursty eventId emitterId transId interval arrivals deadlines _) = 
        RTEventBursty eventId emitterId transId interval arrivals deadlines
    updateAnnotation (RTEventPeriodic eventId emitterId transId deadlines _) = 
        RTEventPeriodic eventId emitterId transId deadlines

type RTEventMap a = M.Map Identifier (RTEvent a)

data RTElement a =
    RTTransaction 
        Identifier -- ^ Transaction identifier
        (RTTransStep a) -- ^ First step
        a -- ^ Annotation
    | RTSituation 
        Identifier -- ^ Situation identifier
        (RTEventMap a) -- ^ Associated events
        a -- ^ Annotation
    deriving Show

instance Annotated RTElement where
    getAnnotation (RTTransaction _ _ a) = a
    getAnnotation (RTSituation _ _ a) = a

    updateAnnotation (RTTransaction ident step _) = RTTransaction ident step
    updateAnnotation (RTSituation ident expr _) = RTSituation ident expr