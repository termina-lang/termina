module EFP.Schedulability.RT.Linear where
import EFP.Schedulability.RT.AST

data LRTAction a =
    LRTAction
        Identifier -- ^ Action name
        a -- ^ Annotation
    deriving Show