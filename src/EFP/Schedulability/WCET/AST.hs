module EFP.Schedulability.WCET.AST 
    (module EFP.Schedulability.WCET.AST
    , module EFP.Schedulability.Core.AST) where

import EFP.Schedulability.Core.AST

data TransactionalWCET a
    = TransactionalWCET
        Identifier -- ^ class name
        Identifier -- ^ action/procedure name
        Identifier -- ^ path name
        [Identifier] -- ^ constant parameters
        (ConstExpression a) -- ^ WCET expression
        a -- ^ annotation
    deriving Show

data WCETPlatformAssignment a
    = WCETPlatformAssignment
        Identifier -- ^ platform name
        [TransactionalWCET a] -- ^ WCETs assigned to this platform
        a -- ^ annotation
    deriving Show