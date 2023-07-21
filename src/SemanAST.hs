{-# LANGUAGE DeriveFunctor #-}

module SemanAST
  ( module SemanAST
  , module CoreAST
  ) where

import CoreAST

-- | Expression here is same as AST plus |Undyn|
data Expression a
  = AccessObject (RHSObject Expression a)
  | Constant Const a
  | ParensExpression (Expression a) a
  | BinOp Op (Expression a) (Expression a) a
  | ReferenceExpression (RHSObject Expression a) a
  | DereferenceExpression (Expression a) a
  | Casting (Expression a) TypeSpecifier a
  | FunctionExpression  Identifier [ Expression a ] a
  | VectorInitExpression (Expression a) ConstExpression a -- ^ Vector initializer
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment' Expression a] a
  | EnumVariantExpression Identifier Identifier [ Expression a ] a
  | OptionVariantExpression (OptionVariant (Expression a)) a
  -------
  -- Added new implicit (from the user pov) constructor.
  | Undyn (Expression a) a
  deriving (Show, Functor)

-- type OptionVariant a = OptionVariant' (Expression a)
type ReturnStmt = ReturnStmt' Expression
type BlockRet = BlockRet' Expression
type AnnASTElement = AnnASTElement' Expression
type FieldValueAssignment = FieldValueAssignment' Expression
type Global = Global' Expression

type TypeDef = TypeDef' Expression

type ClassMember = ClassMember' Expression

type MatchCase = MatchCase' Expression
type ElseIf = ElseIf' Expression
type Statement = Statement' Expression

type AnnotatedProgram a = [AnnASTElement' Expression a]
type Block a = Block' Expression a
