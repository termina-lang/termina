{-# LANGUAGE TypeSynonymInstances #-}
module EFP.Schedulability.TransPath.Types where

import Utils.Annotations

-- | Type of the annotations used when parsing WCE path files.
type ParserAnn = Location

-- | Type of the annotations used when generating WCE path files.
data GeneratorAnn = Generated

newtype SemanticAnn = SemanticAnn Location
    deriving Show

instance Located SemanticAnn where
    getLocation (SemanticAnn loc)= loc
    updateLocation _ = SemanticAnn
