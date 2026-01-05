{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.WCET.Types where

import Utils.Annotations
import qualified Data.Map.Strict as M
import EFP.Schedulability.WCET.AST

data WCETSemAnn = 
    WCETExprTy ConstExprType Location
    | WCETTy Location
    | WCETPltAssignmentTy Location
    deriving Show

instance Located WCETSemAnn where
    getLocation (WCETExprTy _ loc)= loc
    getLocation (WCETTy loc) = loc
    getLocation (WCETPltAssignmentTy loc) = loc

    updateLocation (WCETExprTy t _) loc = WCETExprTy t loc
    updateLocation (WCETTy _) loc = WCETTy loc
    updateLocation (WCETPltAssignmentTy _) loc = WCETPltAssignmentTy loc

type WCETimesMap a = M.Map Identifier (M.Map (Identifier, Identifier) (M.Map Identifier (TransactionalWCET a)))
