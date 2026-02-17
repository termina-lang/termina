module EFP.Schedulability.Core.Types where

import Utils.Annotations

-- | Type of the annotations used when parsing WCE path files.
type ParserAnn = Location

-- | Type of the annotations used when generating WCE path files.
data GeneratorAnn = Generated
