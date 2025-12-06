{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.WCET.Types where

import Utils.Annotations
import qualified Data.Map as M
import EFP.Schedulability.WCET.AST

-- | Type of the annotations used when parsing WCE path files.
type ParserAnn = Location

newtype SemanticAnn = SemanticAnn Location
    deriving Show

instance Located SemanticAnn where
    getLocation (SemanticAnn loc)= loc
    updateLocation _ = SemanticAnn

type WCETimesMap a = M.Map Identifier (M.Map (Identifier, Identifier) (M.Map Identifier (TransactionalWCET a)))
