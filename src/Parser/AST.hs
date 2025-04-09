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
  | Constant (Const' TypeSpecifier) a -- ^ | 24 : i8|
  | BinOp Op (Expression a) (Expression a) a
  | ReferenceExpression AccessKind (Object a) a
  | Casting (Expression a) TypeSpecifier a
  -- Invocation expressions
  | FunctionCall Identifier [Expression a] a
  | MemberFunctionCall (Object a) Identifier [Expression a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  | DerefMemberFunctionCall (Object a) Identifier [Expression a] a
  -- ^ Dereference class method/viewer access | self->name(x_{1}, ... , x_{n})|
  --
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | ArrayInitializer (Expression a) Size a -- ^ TArray initializer, | (13 : i8) + (2 : i8)|
  | ArrayExprListInitializer [Expression a] a -- ^ TArray expression list initializer, | { 13 : i8, 2 : i8 } |
  | StructInitializer
    [FieldAssignment' Expression a] -- ^ Initial value of each field identifier
    (Maybe TypeSpecifier) -- ^ Structure type identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantInitializer
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [Expression a] -- ^ list of expressions
    a
  | OptionVariantInitializer (OptionVariant Expression a) a
  | StringInitializer String a -- ^ String literal
  | IsEnumVariantExpression
    (Object a) -- ^ Enum object
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier a
    a
  | IsOptionVariantExpression
    (Object a) -- ^ Opion object
    OptionVariantLabel -- ^ Variant label
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
  getAnnotation (OptionVariantInitializer _ a)    = a
  getAnnotation (StringInitializer _ a)           = a
  getAnnotation (MemberFunctionCall _ _ _ a)      = a
  getAnnotation (DerefMemberFunctionCall _ _ _ a) = a
  getAnnotation (IsEnumVariantExpression _ _ _ a) = a
  getAnnotation (IsOptionVariantExpression _ _ a) = a

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
  updateAnnotation (OptionVariantInitializer v _) = OptionVariantInitializer v
  updateAnnotation (StringInitializer s _)        = StringInitializer s
  updateAnnotation (MemberFunctionCall obj f args _) = MemberFunctionCall obj f args
  updateAnnotation (DerefMemberFunctionCall obj f args _) = DerefMemberFunctionCall obj f args
  updateAnnotation (IsEnumVariantExpression obj e v _) = IsEnumVariantExpression obj e v
  updateAnnotation (IsOptionVariantExpression obj v _) = IsOptionVariantExpression obj v

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
    (obj a) -- ^ name of the variable
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
type Parameter = Parameter' TypeSpecifier
type Const = Const' TypeSpecifier
type Modifier = Modifier' TypeSpecifier
type FieldDefinition = FieldDefinition' TypeSpecifier
type EnumVariant = EnumVariant' TypeSpecifier

type Block = Block' TypeSpecifier Expression Object
type AnnASTElement = AnnASTElement' TypeSpecifier Block Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TypeSpecifier Expression

type TypeDef = TypeDef' TypeSpecifier Block

type InterfaceMember = InterfaceMember' TypeSpecifier
type ClassMember = ClassMember' TypeSpecifier Block

type MatchCase = MatchCase' TypeSpecifier Expression Object
type ElseIf = ElseIf' TypeSpecifier Expression Object
type Statement = Statement' TypeSpecifier Expression Object

type AnnotatedProgram a = [AnnASTElement' TypeSpecifier Block Expression a]

type ModuleImport = ModuleImport' [String]
type TerminaModule = TerminaModule' TypeSpecifier Block Expression [String]
