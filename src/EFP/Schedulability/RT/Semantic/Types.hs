{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.RT.Semantic.Types where

import Utils.Annotations
import qualified Data.Map.Strict as M
import EFP.Schedulability.RT.Semantic.AST

data RTSemAnn = 
    RTExprTy ConstExprType Location
    | RTTransTy Location
    | RTEventTy Location
    | RTSitTy Location
    | RTStepTy Location
    deriving Show

instance Located RTSemAnn where
    getLocation (RTExprTy _ loc) = loc
    getLocation (RTTransTy loc) = loc
    getLocation (RTEventTy loc) = loc
    getLocation (RTSitTy loc) = loc
    getLocation (RTStepTy loc) = loc

    updateLocation (RTExprTy t _) loc = RTExprTy t loc
    updateLocation (RTTransTy _) loc = RTTransTy loc
    updateLocation (RTEventTy _) loc = RTEventTy loc
    updateLocation (RTSitTy _) loc = RTSitTy loc
    updateLocation (RTStepTy _) loc = RTStepTy loc

type RTTransactionMap a = M.Map Identifier (RTElement a)
type RTSituationMap a = M.Map Identifier (RTElement a)

-- | Valid continuation is a pair (Component identifier, Action identifier)
type Continuation = (Identifier, Identifier)

