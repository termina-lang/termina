{-# LANGUAGE DeriveFunctor #-}
-- | Module defining AST after parsing.
-- The parsing module defines a function |SourceCode -> AnnotatedProgram
-- ParseAnnotations|.
-- In this module, we only define what expressions are after parsing.

module AST
  ( module AST
  , module CoreAST
  ) where

-- From |CoreAST| we get all basic blocks.
import           CoreAST

-- | First AST after parsing
data Expression a
  = AccessObject (RHSObject Expression a)
  | Constant Const a -- ^ | 24 : i8|
  | ParensExpression (Expression a) a
  | BinOp Op (Expression a) (Expression a) a
  | ReferenceExpression (RHSObject Expression a) a
  | DereferenceExpression (Expression a) a
  | Casting (Expression a) TypeSpecifier a
  | FunctionExpression Identifier [ Expression a ] a
  -- FunctionExpression (FuncName a) [ Expression a ] a
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | VectorInitExpression (Expression a) ConstExpression a -- ^ Vector initializer, | (13 : i8) + (2 : i8)|
  | FieldValuesAssignmentsExpression
    Identifier -- ^ Structure type identifier
    [FieldValueAssignment' Expression a] -- ^ Initial value of each field identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantExpression
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [ Expression a ] -- ^ list of expressions
    a
  | OptionVariantExpression (OptionVariant (Expression a)) a
  deriving (Show, Functor)

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
