{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.RT.Types where

import Utils.Annotations
import qualified Data.Map.Strict as M
import EFP.Schedulability.RT.AST

type RTDeadlineMap = M.Map Identifier (ConstExpression RTSemAnn)

data RTEvent a =
    RTEventInterrupt 
        Identifier -- ^ Transaction identifier
        (ConstExpression a) -- ^ Interval expression
        (ConstExpression a) -- ^ Arrivals expression
        RTDeadlineMap -- ^ Deadlines map
        a -- ^ Annotation
    | RTEventPeriodic
        Identifier -- ^ Transaction identifier
        RTDeadlineMap -- ^ Deadlines map
        a -- ^ Annotation
    deriving Show

data RTSemAnn = 
    RTExprTy ConstExprType Location
    | RTTransTy Location
    | RTEventTy Location
    | RTSitTy (RTEventMap RTSemAnn) Location
    | RTStructFieldAssignTy Location
    | RTStructInitTy Location
    | RTStepTy Location
    deriving Show

type RTEventMap a = M.Map Identifier (RTEvent a)

instance Annotated RTEvent where
    getAnnotation (RTEventInterrupt _ _ _ _ a) = a
    getAnnotation (RTEventPeriodic _ _ a) = a

    updateAnnotation (RTEventInterrupt tid interval arrivals deadlines _) = RTEventInterrupt tid interval arrivals deadlines
    updateAnnotation (RTEventPeriodic tid deadlines _) = RTEventPeriodic tid deadlines

instance Located RTSemAnn where
    getLocation (RTExprTy _ loc) = loc
    getLocation (RTTransTy loc) = loc
    getLocation (RTEventTy loc) = loc
    getLocation (RTSitTy _ loc) = loc
    getLocation (RTStructFieldAssignTy loc) = loc
    getLocation (RTStructInitTy loc) = loc
    getLocation (RTStepTy loc) = loc

    updateLocation (RTExprTy t _) loc = RTExprTy t loc
    updateLocation (RTTransTy _) loc = RTTransTy loc
    updateLocation (RTEventTy _) loc = RTEventTy loc
    updateLocation (RTSitTy m _) loc = RTSitTy m loc
    updateLocation (RTStructFieldAssignTy _) loc = RTStructFieldAssignTy loc
    updateLocation (RTStructInitTy loc) _ = RTStructInitTy loc
    updateLocation (RTStepTy _) loc = RTStepTy loc

type RTTransactionMap a = M.Map Identifier (RTElement a)
type RTSituationMap a = M.Map Identifier (RTElement a)

-- | Valid continuation is a pair (Component identifier, Action identifier)
type Continuation = (Identifier, Identifier)

