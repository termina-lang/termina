{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

-- | Module defining AST after parsing.
-- The parsing module defines a function |SourceCode -> AnnotatedProgram
-- ParseAnnotations|.
-- In this module, we only define what expressions are after parsing.

module Parser.AST
  ( module Parser.AST
  , module Core.AST
  ) where

-- From |CoreAST| we get all basic blocks.
import Utils.Annotations
import Core.AST

----------------------------------------
-- | Assignable and /accessable/ values. LHS, referencable and accessable.
-- |Object| should not be invoked directly.
data Object a
  = Variable Identifier a
  -- ^ Plain identifier |v|
  | ArrayIndexExpression (Object a) (Expression a) a
  -- ^ TArray indexing | eI [ eIx ]|,
  -- value |eI :: exprI a| is an identifier expression, could be a name or a
  -- function call (depending on what |exprI| is)
  | MemberAccess (Object a) Identifier a
  -- ^ Data structure/Class access | eI.name |, same as before |ei :: exprI a| is an
  -- expression identifier.
  | Dereference (Object a) a
  -- ^ Dereference | *eI |, |eI| is an identifier expression.
  | DereferenceMemberAccess (Object a) Identifier a
  -- ^ Dereference member access | eI->name |, same as before |ei :: exprI a| is an
  | ArraySlice (Object a) (Expression a) (Expression a) a
  -- ^ TArray slicing | eI [ cEx .. cEy ]|,
  -- value |eI :: exprI a| is an identifier expression
  -- |cEx| is an expression for the lower bound
  -- |cEx| is an expression for the upper bound
  deriving (Show, Functor)

  -- | First AST after parsing
data Expression
    a -- ^ Annotations
  = AccessObject (Object a)
  | Constant (Const a) a -- ^ | 24 : i8|
  | BinOp Op (Expression a) (Expression a) a
  | ReferenceExpression AccessKind (Object a) a
  | Casting (Expression a) (TypeSpecifier a) a
  -- Invocation expressions
  | FunctionCall Identifier [Expression a] a
  | MemberFunctionCall (Object a) Identifier [Expression a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  | DerefMemberFunctionCall (Object a) Identifier [Expression a] a
  -- ^ Dereference class method/viewer access | self->name(x_{1}, ... , x_{n})|
  --
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | ArrayInitializer (Expression a) (Expression a) a -- ^ TArray initializer, | (13 : i8) + (2 : i8)|
  | ArrayExprListInitializer [Expression a] a -- ^ TArray expression list initializer, | { 13 : i8, 2 : i8 } |
  | StructInitializer
    [FieldAssignment' Expression a] -- ^ Initial value of each field identifier
    (Maybe (TypeSpecifier a)) -- ^ Structure type identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantInitializer
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [Expression a] -- ^ list of expressions
    a
  | MonadicVariantInitializer (MonadicVariant a) a
  | StringInitializer String a -- ^ String literal
  | IsEnumVariantExpression
    (Object a) -- ^ Enum object
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier a
    a
  | IsMonadicVariantExpression
    (Object a) -- ^ Option object
    MonadicVariantLabel -- ^ Variant label
    a
  deriving (Show, Functor)

instance Annotated Object where
  getAnnotation (Variable _ a)                  = a
  getAnnotation (ArrayIndexExpression _ _ a)    = a
  getAnnotation (MemberAccess _ _ a)            = a
  getAnnotation (Dereference _ a)               = a
  getAnnotation (DereferenceMemberAccess _ _ a) = a
  getAnnotation (ArraySlice _ _ _ a) = a

  updateAnnotation (Variable v _) = Variable v
  updateAnnotation (ArrayIndexExpression e eIx _) = ArrayIndexExpression e eIx
  updateAnnotation (MemberAccess e n _) = MemberAccess e n
  updateAnnotation (Dereference e _) = Dereference e
  updateAnnotation (DereferenceMemberAccess e n _) = DereferenceMemberAccess e n
  updateAnnotation (ArraySlice e cEx cEy _) = ArraySlice e cEx cEy

instance Annotated Expression where
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
  getAnnotation (MonadicVariantInitializer _ a)    = a
  getAnnotation (StringInitializer _ a)           = a
  getAnnotation (MemberFunctionCall _ _ _ a)      = a
  getAnnotation (DerefMemberFunctionCall _ _ _ a) = a
  getAnnotation (IsEnumVariantExpression _ _ _ a) = a
  getAnnotation (IsMonadicVariantExpression _ _ a) = a

  updateAnnotation (AccessObject obj)             = AccessObject . updateAnnotation obj
  updateAnnotation (Constant c _)                 = Constant c
  updateAnnotation (BinOp op e1 e2 _)             = BinOp op e1 e2
  updateAnnotation (ReferenceExpression k obj _)  = ReferenceExpression k obj
  updateAnnotation (Casting e ty _)               = Casting e ty
  updateAnnotation (FunctionCall f args _)        = FunctionCall f args
  updateAnnotation (StructInitializer fields ty _) = StructInitializer fields ty
  updateAnnotation (EnumVariantInitializer e v args _) = EnumVariantInitializer e v args
  updateAnnotation (ArrayInitializer e s _)       = ArrayInitializer e s
  updateAnnotation (ArrayExprListInitializer es _) = ArrayExprListInitializer es
  updateAnnotation (MonadicVariantInitializer v _) = MonadicVariantInitializer v
  updateAnnotation (StringInitializer s _)        = StringInitializer s
  updateAnnotation (MemberFunctionCall obj f args _) = MemberFunctionCall obj f args
  updateAnnotation (DerefMemberFunctionCall obj f args _) = DerefMemberFunctionCall obj f args
  updateAnnotation (IsEnumVariantExpression obj e v _) = IsEnumVariantExpression obj e v
  updateAnnotation (IsMonadicVariantExpression obj v _) = IsMonadicVariantExpression obj v

data MatchCase a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [Identifier]
  , matchBody       :: Block a
  , matchAnnotation :: a
  } deriving (Show, Functor)

data DefaultCase a = DefaultCase
  (Block a)
  a
  deriving (Show, Functor)

data ElseIf a = ElseIf
  {
    elseIfCond       :: Expression a
  , elseIfBody       :: Block a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    (TypeSpecifier a) -- ^ type of the variable
    (Expression a) -- ^ initialization expression
    a
  | AssignmentStmt
    (Object a) -- ^ name of the variable
    (Expression a) -- ^ assignment expression
    a
  | IfElseStmt
    (Expression a) -- ^ conditional expression
    (Block a) -- ^ statements in the if block
    [ElseIf a] -- ^ list of else if blocks
    (Maybe (Block a)) -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    (TypeSpecifier a) -- ^ type of iterator variable
    (Expression a) -- ^ initial value of the iterator
    (Expression a) -- ^ final value of the iterator
    (Maybe (Expression a)) -- ^ break condition (optional)
    (Block a) -- ^ statements in the for loop
    a
  | MatchStmt
    (Expression a) -- ^ expression to match
    [MatchCase a] -- ^ list of match cases
    (Maybe (DefaultCase a)) -- ^ default case
    a
  | SingleExpStmt
    (Expression a) -- ^ expression
    a
  | ReturnStmt
    (Maybe (Expression a)) -- ^ return expression
    a
  | ContinueStmt
    (Expression a)
    a
  | RebootStmt a
  deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data Block a
  = Block
  {
    blockBody :: [Statement a],
    blockAnnotation :: a
  }
  deriving (Show, Functor)

----------------------------------------
type TypeParameter = TypeParameter' Expression
type TypeSpecifier = TypeSpecifier' Expression
type Parameter = Parameter' TypeSpecifier
type Const = Const' TypeSpecifier
type Modifier = Modifier' TypeSpecifier
type FieldDefinition = FieldDefinition' TypeSpecifier
type EnumVariant = EnumVariant' TypeSpecifier
type MonadicVariant = MonadicVariant' Expression

type AnnASTElement = AnnASTElement' TypeSpecifier Block Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TypeSpecifier Expression

type TypeDef = TypeDef' TypeSpecifier Block

type InterfaceMember = InterfaceMember' TypeSpecifier
type ClassMember = ClassMember' TypeSpecifier Block

type AnnotatedProgram a = [AnnASTElement' TypeSpecifier Block Expression a]

type ModuleImport = ModuleImport' [String]
type TerminaModule a = TerminaModule' TypeSpecifier Block Expression [String] a
