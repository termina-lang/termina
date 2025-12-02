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
    deriving Show