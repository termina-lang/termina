{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

-- | Module defining AST after parsing.
-- The parsing module defines a function |SourceCode -> AnnotatedProgram
-- ParseAnnotations|.
-- In this module, we only define what expressions are after parsing.

module AST.Parser
  ( module AST.Parser
  , module AST.Core
  ) where

-- From |CoreAST| we get all basic blocks.
import Utils.Annotations
import AST.Core

----------------------------------------
-- | Assignable and /accessable/ values. LHS, referencable and accessable.
-- |Object| should not be invoked directly.
data Object a
  = Variable Identifier a
  -- ^ Plain identifier |v|
  | ArrayIndexExpression (Object a) (Expression' Object a) a
  -- ^ Array indexing | eI [ eIx ]|,
  -- value |eI :: exprI a| is an identifier expression, could be a name or a
  -- function call (depending on what |exprI| is)
  | MemberAccess (Object a) Identifier a
  -- ^ Data structure/Class access | eI.name |, same as before |ei :: exprI a| is an
  -- expression identifier.
  | Dereference (Object a) a
  -- ^ Dereference | *eI |, |eI| is an identifier expression.
  | DereferenceMemberAccess (Object a) Identifier a
  -- ^ Dereference member access | eI->name |, same as before |ei :: exprI a| is an
  | ArraySlice (Object a) (Expression' Object a) (Expression' Object a) a
  -- ^ Array slicing | eI [ cEx .. cEy ]|,
  -- value |eI :: exprI a| is an identifier expression
  -- |cEx| is an expression for the lower bound
  -- |cEx| is an expression for the upper bound
  deriving (Show, Functor)

  -- | First AST after parsing
data Expression'
    obj -- ^ objects type
    a -- ^ Annotations
  = AccessObject (obj a)
  | Constant Const a -- ^ | 24 : i8|
  | BinOp Op (Expression' obj a) (Expression' obj a) a
  | ReferenceExpression AccessKind (obj a) a
  | Casting (Expression' obj a) TypeSpecifier a
  -- Invocation expressions
  | FunctionCall Identifier [ Expression' obj a ] a
  | MemberFunctionCall (obj a) Identifier [Expression' obj a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  | DerefMemberFunctionCall (obj a) Identifier [Expression' obj a] a
  -- ^ Dereference class method/viewer access | self->name(x_{1}, ... , x_{n})|
  --
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | ArrayInitializer (Expression' obj a) Size a -- ^ Array initializer, | (13 : i8) + (2 : i8)|
  | ArrayExprListInitializer [Expression' obj a] a -- ^ Array expression list initializer, | { 13 : i8, 2 : i8 } |
  | StructInitializer
    [FieldAssignment' (Expression' obj) a] -- ^ Initial value of each field identifier
    (Maybe Identifier) -- ^ Structure type identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantInitializer
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [ Expression' obj a ] -- ^ list of expressions
    a
  | OptionVariantInitializer (OptionVariant (Expression' obj a)) a
  | IsEnumVariantExpression
    (obj a) -- ^ Enum object
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier a
    a
  | IsOptionVariantExpression
    (obj a) -- ^ Opion object
    OptionVariantLabel -- ^ Variant label
    a
  deriving (Show, Functor)


instance Annotated Object where
  getAnnotation (Variable _ a)                = a
  getAnnotation (ArrayIndexExpression _ _ a) = a
  getAnnotation (MemberAccess _ _ a)          = a
  getAnnotation (Dereference _ a)             = a
  getAnnotation (DereferenceMemberAccess _ _ a) = a
  getAnnotation (ArraySlice _ _ _ a) = a


instance (Annotated obj) => Annotated (Expression' obj) where
  getAnnotation (AccessObject obj)                = getAnnotation obj
  getAnnotation (Constant _ a)                    = a
  getAnnotation (BinOp _ _ _ a)                   = a
  getAnnotation (ReferenceExpression _ _ a)       = a
  getAnnotation (Casting _ _ a)                   = a
  getAnnotation (FunctionCall _ _ a)              = a
  getAnnotation (StructInitializer _ _ a)         = a
  getAnnotation (EnumVariantInitializer _ _ _ a)  = a
  getAnnotation (ArrayInitializer _ _ a)          = a
  getAnnotation (ArrayExprListInitializer _ a)    = a
  getAnnotation (OptionVariantInitializer _ a)    = a
  getAnnotation (MemberFunctionCall _ _ _ a)      = a
  getAnnotation (DerefMemberFunctionCall _ _ _ a) = a
  getAnnotation (IsEnumVariantExpression _ _ _ a) = a
  getAnnotation (IsOptionVariantExpression _ _ a) = a