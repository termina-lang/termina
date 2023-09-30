{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

module SemanAST
  ( module SemanAST
  , module CoreAST
  ) where

import           Annotations
import           CoreAST
----------------------------------------
-- | Assignable and /accessable/ values. LHS, referencable and accessable.
-- |Object| should not be invoked directly.
data Object a
  = Variable Identifier a
  -- ^ Plain identifier |v|
  | VectorIndexExpression (Object a) (Expression a) a
  -- ^ Array indexing | eI [ eIx ]|,
  -- value |eI :: exprI a| is an identifier expression, could be a name or a
  -- function call (depending on what |exprI| is)
  | MemberAccess (Object a) Identifier a
  -- ^ Data structure/Class access | eI.name |, same as before |ei :: exprI a| is an
  -- expression identifier.
  | Dereference (Object a) a
  -- ^ Dereference | *eI |, |eI| is an ~identifier~ expression.
  | VectorSliceExpression (Object a) ConstExpression ConstExpression a
  -- ^ Array slicing | eI [ cEx .. cEy ]|,
  -- value |eI :: exprI a| is an identifier expression
  -- |cEx| is an expression for the lower bound
  -- |cEx| is an expression for the upper bound
  | Undyn (Object a) a
  deriving (Show, Functor)

instance Annotated Object where
  getAnnotation (Variable _ a)                = a
  getAnnotation (VectorIndexExpression _ _ a) = a
  getAnnotation (MemberAccess _ _ a)          = a
  getAnnotation (Dereference _ a)             = a
  getAnnotation (VectorSliceExpression _ _ _ a) = a
  getAnnotation (Undyn _ a)                   = a
----------------------------------------

-- type OptionVariant a = OptionVariant' (Expression a)
type Expression = Expression' Object

type ReturnStmt = ReturnStmt' Expression
type BlockRet = BlockRet' Expression Object
type AnnASTElement = AnnASTElement' Expression Object
type FieldValueAssignment = FieldValueAssignment' Expression
type Global = Global' Expression

type TypeDef a = TypeDef' Expression Object a

type ClassMember = ClassMember' Expression Object

type MatchCase = MatchCase' Expression Object
type ElseIf = ElseIf' Expression Object
type Statement = Statement' Expression Object

type AnnotatedProgram a = [AnnASTElement' Expression Object a]
type Block a = Block' Expression Object a
