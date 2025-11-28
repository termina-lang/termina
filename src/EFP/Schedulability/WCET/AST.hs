module EFP.Schedulability.WCET.AST 
    (module EFP.Schedulability.WCET.AST
    , module EFP.Schedulability.Core.AST) where

import EFP.Schedulability.Core.AST

data TransactionalWCET
    = TransactionalWCET
        Identifier -- ^ class name
        Identifier -- ^ action/procedure name
        Identifier -- ^ path name
        [Identifier] -- ^ constant parameters
        ConstExpression -- ^ WCET expression
    deriving Show