-- | Util functions related to CoreAST

module Utils.CoreAST where

import           CoreAST

getObjectAnnotations :: Object expr a -> a
getObjectAnnotations (Variable _ a)                = a
getObjectAnnotations (VectorIndexExpression _ _ a) = a
getObjectAnnotations (MemberAccess _ _ a)          = a
