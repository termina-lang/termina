{-# LANGUAGE DeriveFunctor #-}

module SemanAST
  ( module SemanAST
  , module CoreAST
  ) where


import CoreAST

data Expression a
  = Variable Identifier a
  | Constant Const a
  | ParensExpression (Expression a) a
  | BinOp Op (Expression a) (Expression a) a
  | VectorIndexExpression (Expression a) (Expression a) a -- ^ Binary operation : array indexing
  | ReferenceExpression (Expression a) a
  | DereferenceExpression (Expression a) a
  | Casting (Expression a) TypeSpecifier a
  | FunctionExpression  Identifier [ Expression a ] a
  | VectorInitExpression (Expression a) ConstExpression a -- ^ Vector initializer
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment' Expression a] a
  | EnumVariantExpression Identifier Identifier [ Expression a ] a
  | OptionVariantExpression (OptionVariant (Expression a)) a
  -------
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
