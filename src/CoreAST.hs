{-# LANGUAGE DeriveFunctor #-}
-- | Module defining core AST

module CoreAST where

import AST (Identifier
           , Const(..)
           , TypeSpecifier
           , ConstExpression
           ,FieldValueAssignment(..)
           , Op(..)
           , OptionVariant(..))

data Expression a
  = Variable Identifier a
  | Constant Const a
  | ParensExpression (Expression a) a
  | BinOp Op (Expression a) (Expression a) a
  | VectorIndexExpression (Expression a) (Expression a) a -- ^ Binary operation : array indexing
  | ReferenceExpression (Expression a) a
  | DereferenceExpression (Expression a) a
  | Casting (Expression a) TypeSpecifier a
  | FunctionExpression  [ Expression a ] a
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | VectorInitExpression (Expression a) ConstExpression a -- ^ Vector initializer
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment (Expression a)] a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantExpression Identifier Identifier [ Expression a ] a
  | OptionVariantExpression (OptionVariant a) a
  | Undyn Identifier a
  deriving (Show, Functor)
