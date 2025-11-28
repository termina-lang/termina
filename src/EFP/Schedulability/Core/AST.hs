module EFP.Schedulability.Core.AST
    (module EFP.Schedulability.Core.AST
    , module Core.AST) where

import Core.AST

data ConstExpression
    = ConstInt TInteger
    | ConstObject Identifier
    | ConstBinOp Op ConstExpression ConstExpression
    deriving Show