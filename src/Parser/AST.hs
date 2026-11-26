{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Utils.Printer
import qualified Data.Text as T

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
  | DereferenceMemberAccess (Object a) Identifier a
  -- ^ Dereference member access | eI->name |, same as before |ei :: exprI a| is an
  | ArraySlice (Object a) (Expression a) (Expression a) a
  -- ^ TArray slicing | eI [ cEx .. cEy ]|,
  -- value |eI :: exprI a| is an identifier expression
  -- |cEx| is an expression for the lower bound
  -- |cEx| is an expression for the upper bound
  deriving (Show, Functor, Foldable)

instance ShowText (Object a) where
    showText (Variable ident _) = T.pack ident
    showText (ArrayIndexExpression obj expr _) = 
        showText obj <> "[" <> showText expr <> "]"
    showText (MemberAccess obj ident _) = 
        showText obj <> "." <> T.pack ident
    showText (Dereference obj _) = "*" <> showText obj
    showText (DereferenceMemberAccess obj ident _) = 
        "*" <> showText obj <> "." <> T.pack ident
    showText (ArraySlice obj expr1 expr2 _) = 
        showText obj <> "[" <> showText expr1 <> ".." <> showText expr2 <> "]"

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
  | IsEnumVariantExpression
    (Expression a) -- ^ Enum value
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier a
    a
  | IsMonadicVariantExpression
    (Expression a) -- ^ Option, Status or Result value
    MonadicVariantLabel -- ^ Variant label
    a
  deriving (Show, Functor, Foldable)

instance ShowText (Expression a) where
    showText (AccessObject obj) = showText obj
    showText (Constant c _) = showText c
    showText (BinOp op lhe rhe _) = showText lhe <> " " <> showText op <> " " <> showText rhe
    showText (ReferenceExpression ak obj _) = "&" <> showText ak <> " " <> showText obj
    showText (Casting e ty _) = "(" <> showText ty <> ") " <> showText e
    showText (FunctionCall ident args _) = 
        T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (MemberFunctionCall obj ident args _) =
        showText obj <> "." <> T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (DerefMemberFunctionCall obj ident args _) = 
        showText obj <> "->" <> T.pack ident <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (ArrayInitializer value size _) =
        "[" <> showText value <> "; " <> showText size <> "]"
    showText (ArrayExprListInitializer exprs _) = 
        "{" <> T.intercalate ", " (map showText exprs) <> "}"
    showText (StructInitializer fs Nothing _) = 
        "{" <> T.intercalate ", " (map showText fs) <> "}"
    showText (StructInitializer fs (Just ty) _) = 
        "{" <> T.intercalate ", " (map showText fs) <> "} : " <> showText ty
    showText (EnumVariantInitializer ident variant args _) = 
        T.pack ident <> "::" <> T.pack variant <> "(" <> T.intercalate ", " (map showText args) <> ")"
    showText (MonadicVariantInitializer ov _) = 
        "Some(" <> showText ov <> ")"
    showText (StringInitializer str _) = 
        "\"" <> T.pack str <> "\""
    showText (IsEnumVariantExpression obj ident variant _) = 
        showText obj <> " is " <> T.pack ident <> "::" <> T.pack variant
    showText (IsMonadicVariantExpression obj variant _) = 
        showText obj <> " is " <> showText variant

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
  , matchBVars      :: [(Identifier, a)] -- ^ variables the variant binds, each with its own position
  , matchBody       :: Block a
  , matchAnnotation :: a
  } deriving (Show, Functor, Foldable)

instance Annotated MatchCase where
  getAnnotation = matchAnnotation
  updateAnnotation c a = c { matchAnnotation = a }

data DefaultCase a = DefaultCase
  (Block a)
  a
  deriving (Show, Functor, Foldable)

instance Annotated DefaultCase where
  getAnnotation (DefaultCase _ a) = a
  updateAnnotation (DefaultCase b _) = DefaultCase b

data CondIf a = CondIf
  {
    condIfCond       :: Expression a
  , condIfBody       :: Block a
  , condIfAnnotation :: a
  } deriving (Show, Functor, Foldable)

data CondElse a = CondElse
  {
    condElseBody       :: Block a
  , condElseAnnotation :: a
  } deriving (Show, Functor, Foldable)

instance Annotated CondElse where
  getAnnotation = condElseAnnotation
  updateAnnotation c a = c { condElseAnnotation = a }

data CondElseIf a = CondElseIf
  {
    condElseIfCond       :: Expression a
  , condElseIfBody       :: Block a
  , condElseIfAnnotation :: a
  } deriving (Show, Functor, Foldable)

instance Annotated CondElseIf where
  getAnnotation = condElseIfAnnotation
  updateAnnotation c a = c { condElseIfAnnotation = a }

data Statement a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    (TypeSpecifier a) -- ^ type of the variable
    (Maybe (Expression a)) -- ^ initialization expression, absent in a deferred declaration
    a
  | AssignmentStmt
    (Object a) -- ^ name of the variable
    (Expression a) -- ^ assignment expression
    a
  | IfElseStmt
    (CondIf a) -- ^ if condition and body
    [CondElseIf a] -- ^ list of else if blocks
    (Maybe (CondElse a)) -- ^ statements in the else block
    a
  -- | For loop
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
  -- | Read of a fixed-location field whose value is discarded (_ = e;)
  | DiscardStmt
    (Expression a) -- ^ expression
    a
  | ReturnStmt
    (Maybe (Expression a)) -- ^ return expression
    a
  | ContinueStmt
    (Expression a)
    a
  | RebootStmt a
  deriving (Show, Functor, Foldable)

instance Annotated Statement where
  getAnnotation (Declaration _ _ _ _ a) = a
  getAnnotation (AssignmentStmt _ _ a) = a
  getAnnotation (IfElseStmt _ _ _ a) = a
  getAnnotation (ForLoopStmt _ _ _ _ _ _ a) = a
  getAnnotation (MatchStmt _ _ _ a) = a
  getAnnotation (SingleExpStmt _ a) = a
  getAnnotation (DiscardStmt _ a) = a
  getAnnotation (ReturnStmt _ a) = a
  getAnnotation (ContinueStmt _ a) = a
  getAnnotation (RebootStmt a) = a

  updateAnnotation (Declaration n ak ts e _) = Declaration n ak ts e
  updateAnnotation (AssignmentStmt o e _) = AssignmentStmt o e
  updateAnnotation (IfElseStmt i eis e _) = IfElseStmt i eis e
  updateAnnotation (ForLoopStmt n ts f t c b _) = ForLoopStmt n ts f t c b
  updateAnnotation (MatchStmt e cs d _) = MatchStmt e cs d
  updateAnnotation (SingleExpStmt e _) = SingleExpStmt e
  updateAnnotation (DiscardStmt e _) = DiscardStmt e
  updateAnnotation (ReturnStmt e _) = ReturnStmt e
  updateAnnotation (ContinueStmt e _) = ContinueStmt e
  updateAnnotation (RebootStmt _) = RebootStmt

-- | |BlockRet| represent a body block with its return statement
data Block a
  = Block
  {
    blockBody :: [Statement a],
    blockAnnotation :: a
  }
  deriving (Show, Functor, Foldable)

----------------------------------------
type TypeParameter = TypeParameter' Expression

instance ShowText (TypeParameter a) where
    showText (TypeParamIdentifier ident) = T.pack ident
    showText (TypeParamTypeSpec ts) = showText ts
    showText (TypeParamSize size) = showText size

type TypeSpecifier = TypeSpecifier' Expression

instance ShowText (TypeSpecifier a) where
    showText TSUInt8 = "u8"
    showText TSUInt16 = "u16"
    showText TSUInt32 = "u32"
    showText TSUInt64 = "u64"
    showText TSInt8 = "i8"
    showText TSInt16 = "i16"
    showText TSInt32 = "i32"
    showText TSInt64 = "i64"
    showText TSUSize = "usize"
    showText TSBool = "bool"
    showText TSChar = "char"
    showText TSFloat32 = "f32"
    showText TSFloat64 = "f64"
    showText (TSArray ts size) = "[" <> showText ts <> "; "  <> showText size <> "]"
    showText (TSBoxSubtype ts) = "box " <> showText ts
    showText (TSLocation ts) = "loc " <> showText ts
    showText (TSAccessPort ts) = "access " <> showText ts
    showText (TSSinkPort ts ident) = "sink " <> showText ts <> " triggers " <> T.pack ident
    showText (TSInPort ts ident) = "in " <> showText ts <> " triggers " <> T.pack ident
    showText (TSOutPort ts) = "out " <> showText ts
    showText (TSReference ak ts) = "&" <> showText ak <> showText ts
    showText (TSDefinedType ident []) = T.pack ident
    showText (TSDefinedType ident tsps) = T.pack ident <> "<" <> T.intercalate ", " (map showText tsps) <> ">"
    showText TSUnit = "unit"

type Parameter = Parameter' TypeSpecifier
type Const = Const' TypeSpecifier
type Modifier = Modifier' Expression
type FieldDefinition = FieldDefinition' TypeSpecifier
type EnumVariant = EnumVariant' TypeSpecifier
type MonadicVariant = MonadicVariant' Expression

type AnnASTElement = AnnASTElement' TypeSpecifier Expression Block
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TypeSpecifier Expression

type TypeDef = TypeDef' TypeSpecifier Expression Block

type InterfaceMember = InterfaceMember' TypeSpecifier Expression
type ClassMember = ClassMember' TypeSpecifier Block

type AnnotatedProgram a = [AnnASTElement' TypeSpecifier Expression Block a]

type ModuleImport = ModuleImport' [String]
type TerminaModule a = TerminaModule' TypeSpecifier Expression Block [String] a
