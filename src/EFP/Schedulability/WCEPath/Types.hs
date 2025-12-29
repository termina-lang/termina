{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.WCEPath.Types where

import Utils.Annotations
import qualified Data.Map as M
import EFP.Schedulability.WCEPath.AST

data TRPSemAnn = 
    TRExprTy ConstExprType Location
    | TRBlock Location
    | TRWCEPTy Location
    deriving Show

instance Located TRPSemAnn where
    getLocation (TRExprTy _ loc)= loc
    getLocation (TRBlock loc) = loc
    getLocation (TRWCEPTy loc) = loc

    updateLocation (TRExprTy t _) loc = TRExprTy t loc
    updateLocation (TRBlock _) loc = TRBlock loc
    updateLocation (TRWCEPTy _) loc = TRWCEPTy loc

type TransPathMap a = M.Map (Identifier, Identifier) (M.Map Identifier (WCEPath a))