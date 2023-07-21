-- | Utilities for Semantic AST

module Utils.SemanAST where

import           SemanAST
import           Utils.CoreAST

getAnnotations :: Expression a -> a
getAnnotations (AccessObject (RHS obj))                 = getObjectAnnotations obj
getAnnotations (Constant _ a)                           = a
getAnnotations (BinOp _ _ _ a)                          = a
getAnnotations (ReferenceExpression _ a)                = a
getAnnotations (Casting _ _ a)                          = a
getAnnotations (FunctionExpression _ _ a)               = a
getAnnotations (FieldValuesAssignmentsExpression _ _ a) = a
getAnnotations (EnumVariantExpression _ _ _ a)          = a
getAnnotations (VectorInitExpression _ _ a)             = a
getAnnotations (Undyn _ a)             = a
