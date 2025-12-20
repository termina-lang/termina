{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.RT.Types where

import Utils.Annotations
import qualified Data.Map as M
import EFP.Schedulability.RT.AST

-- | Type of the annotations used when parsing WCE path files.
type ParserAnn = Location

-- | Type of the annotations used when generating WCE path files.
data GeneratorAnn = Generated

newtype SemanticAnn = SemanticAnn Location
    deriving Show

instance Located SemanticAnn where
    getLocation (SemanticAnn loc)= loc
    updateLocation _ = SemanticAnn

type RTTransactionMap a = M.Map Identifier (RTElement a)
type RTSituationMap a = M.Map Identifier (RTElement a)

-- | Valid continuation is a pair (Component identifier, Action identifier)
type Continuation = (Identifier, Identifier)

