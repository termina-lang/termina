-- | Util functions related to CoreAST

module Utils.CoreAST where

import           CoreAST

getObjectAnnotations :: Object' exprI exprE a -> a
getObjectAnnotations (Variable _ a)                = a
getObjectAnnotations (IdentifierExpression _ a)    = a
getObjectAnnotations (VectorIndexExpression _ _ a) = a
getObjectAnnotations (MemberAccess _ _ a)          = a
getObjectAnnotations (Dereference _ a)             = a
