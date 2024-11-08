{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

module Semantic.AST
  ( module Semantic.AST
  , module Core.AST
  ) where

import Utils.Annotations
import Core.AST

----------------------------------------
-- | Assignable and /accessable/ values. LHS, referencable and accessable.
-- |Object| should not be invoked directly.
data Object' ty a
  = Variable Identifier a
  -- ^ Plain identifier |v|
  | ArrayIndexExpression (Object' ty a) (Expression' ty (Object' ty) a) a
  -- ^ TArray indexing | eI [ eIx ]|,
  -- value |eI :: exprI a| is an identifier expression, could be a name or a
  -- function call (depending on what |exprI| is)
  | MemberAccess (Object' ty a) Identifier a
  -- ^ Data structure/Class access | eI.name |, same as before |ei :: exprI a| is an
  -- expression identifier.
  | Dereference (Object' ty a) a
  -- ^ Dereference | *eI |, |eI| is an identifier expression.
  | DereferenceMemberAccess (Object' ty a) Identifier a
  -- ^ Dereference member access | eI->name |, same as before |ei :: exprI a| is an
  | Unbox (Object' ty a) a
  deriving (Show, Functor)

-- | First AST after parsing
data Expression'
    ty -- ^ typing information
    obj -- ^ objects type
    a -- ^ Annotations
  = AccessObject (obj a)
  | Constant (Const' ty) a -- ^ | 24 : i8|
  | BinOp Op (Expression' ty obj a) (Expression' ty obj a) a
  | ReferenceExpression AccessKind (obj a) a
  | Casting (Expression' ty obj a) ty a
  -- Invocation expressions
  | FunctionCall Identifier [Expression' ty obj a] a
  | MemberFunctionCall (obj a) Identifier [Expression' ty obj a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  | DerefMemberFunctionCall (obj a) Identifier [Expression' ty obj a] a
  -- ^ Dereference class method/viewer access | self->name(x_{1}, ... , x_{n})|
  --
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | ArrayInitializer (Expression' ty obj a) Size a -- ^ TArray initializer, | (13 : i8) + (2 : i8)|
  | ArrayExprListInitializer [Expression' ty obj a] a -- ^ TArray expression list initializer, | { 13 : i8, 2 : i8 } |
  | StructInitializer
    [FieldAssignment' (Expression' ty obj) a] -- ^ Initial value of each field identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantInitializer
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [Expression' ty obj a] -- ^ list of expressions
    a
  | OptionVariantInitializer (OptionVariant (Expression' ty obj a)) a
  | IsEnumVariantExpression
    (obj a) -- ^ Enum object
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier a
    a
  | IsOptionVariantExpression
    (obj a) -- ^ Opion object
    OptionVariantLabel -- ^ Variant label
    a
  | ArraySliceExpression AccessKind (Object' ty a) (Expression' ty obj a) (Expression' ty obj a) a
  -- ^ TArray slice. This is a reference to an slisce of an array.
  deriving (Show, Functor)

instance Annotated (Object' ty) where
  getAnnotation (Variable _ a)                = a
  getAnnotation (ArrayIndexExpression _ _ a) = a
  getAnnotation (MemberAccess _ _ a)          = a
  getAnnotation (Dereference _ a)             = a
  getAnnotation (DereferenceMemberAccess _ _ a) = a
  getAnnotation (Unbox _ a)                   = a

  updateAnnotation (Variable n _) = Variable n
  updateAnnotation (ArrayIndexExpression obj e _) = ArrayIndexExpression obj e
  updateAnnotation (MemberAccess obj n _) = MemberAccess obj n
  updateAnnotation (Dereference obj _) = Dereference obj
  updateAnnotation (DereferenceMemberAccess obj n _) = DereferenceMemberAccess obj n
  updateAnnotation (Unbox obj _) = Unbox obj

instance (Annotated obj) => Annotated (Expression' ty obj) where
  getAnnotation (AccessObject obj)                = getAnnotation obj
  getAnnotation (Constant _ a)                    = a
  getAnnotation (BinOp _ _ _ a)                   = a
  getAnnotation (ReferenceExpression _ _ a)       = a
  getAnnotation (Casting _ _ a)                   = a
  getAnnotation (FunctionCall _ _ a)              = a
  getAnnotation (StructInitializer _ a)           = a
  getAnnotation (EnumVariantInitializer _ _ _ a)  = a
  getAnnotation (ArrayInitializer _ _ a)          = a
  getAnnotation (ArrayExprListInitializer _ a)    = a
  getAnnotation (OptionVariantInitializer _ a)    = a
  getAnnotation (MemberFunctionCall _ _ _ a)      = a
  getAnnotation (DerefMemberFunctionCall _ _ _ a) = a
  getAnnotation (IsEnumVariantExpression _ _ _ a) = a
  getAnnotation (IsOptionVariantExpression _ _ a) = a
  getAnnotation (ArraySliceExpression _ _ _ _ a)    = a

  updateAnnotation (AccessObject obj) = AccessObject . updateAnnotation obj
  updateAnnotation (Constant c _) = Constant c
  updateAnnotation (BinOp op e1 e2 _) = BinOp op e1 e2
  updateAnnotation (ReferenceExpression ak obj _) = ReferenceExpression ak obj
  updateAnnotation (Casting e ty _) = Casting e ty
  updateAnnotation (FunctionCall f es _) = FunctionCall f es
  updateAnnotation (StructInitializer fs _) = StructInitializer fs
  updateAnnotation (EnumVariantInitializer id1 id2 es _) = EnumVariantInitializer id1 id2 es
  updateAnnotation (ArrayInitializer e s _) = ArrayInitializer e s
  updateAnnotation (ArrayExprListInitializer es _) = ArrayExprListInitializer es
  updateAnnotation (OptionVariantInitializer ov _) = OptionVariantInitializer ov
  updateAnnotation (MemberFunctionCall obj f es _) = MemberFunctionCall obj f es
  updateAnnotation (DerefMemberFunctionCall obj f es _) = DerefMemberFunctionCall obj f es
  updateAnnotation (IsEnumVariantExpression obj id1 id2 _) = IsEnumVariantExpression obj id1 id2
  updateAnnotation (IsOptionVariantExpression obj v _) = IsOptionVariantExpression obj v
  updateAnnotation (ArraySliceExpression ak obj e1 e2 _) = ArraySliceExpression ak obj e1 e2


data MatchCase' ty expr obj a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [Identifier]
  , matchBody       :: Block' ty expr obj a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf' ty expr obj a = ElseIf
  {
    elseIfCond       :: expr a
  , elseIfBody       :: Block' ty expr obj a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement' ty expr obj a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    ty -- ^ type of the variable
    (expr a) -- ^ initialization expression
    a
  | AssignmentStmt
    (obj a) -- ^ left hand side of the assignment
    (expr a) -- ^ assignment expression
    a
  | IfElseStmt
    (expr a) -- ^ conditional expression
    (Block' ty expr obj a) -- ^ statements in the if block
    [ElseIf' ty expr obj a] -- ^ list of else if blocks
    (Maybe (Block' ty expr obj a)) -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    ty -- ^ type of iterator variable
    (expr a) -- ^ initial value of the iterator
    (expr a) -- ^ final value of the iterator
    (Maybe (expr a)) -- ^ break condition (optional)
    (Block' ty expr obj a) -- ^ statements in the for loop
    a
  | MatchStmt
    (expr a) -- ^ expression to match
    [MatchCase' ty expr obj a] -- ^ list of match cases
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  | ReturnStmt
    (Maybe (expr a)) -- ^ return expression
    a
  | ContinueStmt
    (expr a)
    a
  deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data Block' ty expr obj a
  = Block
  {
    blockBody :: [Statement' ty expr obj a],
    blockAnnotation :: a
  }
  deriving (Show, Functor)

----------------------------------------
type Parameter = Parameter' TerminaType
type Const = Const' TerminaType
type Modifier = Modifier' TerminaType
type FieldDefinition = FieldDefinition' TerminaType
type EnumVariant = EnumVariant' TerminaType

type Object = Object' TerminaType
type Expression = Expression' TerminaType Object

type Block = Block' TerminaType Expression Object
type AnnASTElement = AnnASTElement' TerminaType Block Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TerminaType Expression

type TypeDef = TypeDef' TerminaType Block

type InterfaceMember = InterfaceMember' TerminaType
type ClassMember = ClassMember' TerminaType Block

type MatchCase = MatchCase' TerminaType Expression Object
type ElseIf = ElseIf' TerminaType Expression Object
type Statement = Statement' TerminaType Expression Object

type AnnotatedProgram a = [AnnASTElement' TerminaType Block Expression a]
