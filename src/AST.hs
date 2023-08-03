{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

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

----------------------------------------
-- | Assignable and /accessable/ values. LHS, referencable and accessable.
-- |Object'| should not be invoked directly.
data Object'
    (exprI :: * -> *) -- ^ Types returning identifiers
    (a :: *)
  = Variable Identifier a
  -- ^ Plain identifier |v|
  | IdentifierExpression (exprI a) a
  -- ^ Complex identifier expressions: objects in runtime.
  -- Added to have something like `return (f().foo + 3)`
  | VectorIndexExpression (Object' exprI a) (Expression a) a
  -- ^ Array indexing | eI [ eIx ]|,
  -- value |eI :: exprI a| is an identifier expression, could be a name or a
  -- function call (depending on what |exprI| is)
  | MemberAccess (Object' exprI a) Identifier a
  -- ^ Data structure/Class access | eI.name |, same as before |ei :: exprI a| is an
  -- expression identifier.
  | MemberMethodAccess (Object' exprI a) Identifier [Expression a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  | Dereference (Object' exprI a) a
  -- ^ Dereference | *eI |, |eI| is an identifier expression.
  deriving (Show, Functor)

-- | |RHSObjects| do not make a difference between identifier expressions and
-- regular expressions.
newtype RHSObject a = RHS {unRHS :: Object' Expression a}
  deriving (Show, Functor)
-- | |LHSObjects| do not accept |IdentifierExpressions|, and thus, we use the
-- (polymorphic) empty type |Empty|
newtype LHSObject a = LHS {unLHS :: Object' Empty a}
  deriving (Show, Functor)
----------------------------------------


type Expression = Expression' RHSObject

type ReturnStmt = ReturnStmt' Expression
type BlockRet = BlockRet' Expression LHSObject
type AnnASTElement = AnnASTElement' Expression LHSObject
type FieldValueAssignment = FieldValueAssignment' Expression
type Global = Global' Expression

type TypeDef a = TypeDef' Expression LHSObject a

type ClassMember = ClassMember' Expression LHSObject

type MatchCase = MatchCase' Expression LHSObject
type ElseIf = ElseIf' Expression LHSObject
type Statement = Statement' Expression LHSObject

type AnnotatedProgram a = [AnnASTElement' Expression LHSObject a]
type Block a = Block' Expression LHSObject a
