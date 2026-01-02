{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.WCEPath.Types where

import Utils.Annotations
import qualified Data.Map as M
import EFP.Schedulability.WCEPath.AST

data WCEPSemAnn = 
    WCEPExprTy ConstExprType Location
    | WCEPBlock Location
    | WCEPPathTy Location
    deriving Show

instance Located WCEPSemAnn where
    getLocation (WCEPExprTy _ loc)= loc
    getLocation (WCEPBlock loc) = loc
    getLocation (WCEPPathTy loc) = loc

    updateLocation (WCEPExprTy t _) loc = WCEPExprTy t loc
    updateLocation (WCEPBlock _) loc = WCEPBlock loc
    updateLocation (WCEPPathTy _) loc = WCEPPathTy loc

type WCEPathMap a = M.Map (Identifier, Identifier) (M.Map Identifier (WCEPath a))