{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Semantic.AST
  ( module Semantic.AST
  , module Core.AST
  ) where

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
  | DereferenceMemberAccess (Object a) Identifier a
  -- ^ Dereference member access | eI->name |, same as before |ei :: exprI a| is an
  | Unbox (Object a) a
  deriving (Show, Functor)

instance ShowText (Object a) where
    showText (Variable ident _) = T.pack ident
    showText (ArrayIndexExpression obj expr _) = 
        showText obj <> "[" <> showText expr <> "]"
    showText (MemberAccess obj ident _) = 
        showText obj <> "." <> T.pack ident
    showText (Dereference obj _) = "*" <> showText obj
    showText (DereferenceMemberAccess obj ident _) = 
        "*" <> showText obj <> "." <> T.pack ident
    showText (Unbox obj _) = "unbox " <> showText obj

-- | First AST after parsing
data Expression
    a -- ^ Annotations
  = AccessObject (Object a)
  | Constant (Const a) a -- ^ | 24 : i8|
  | BinOp Op (Expression a) (Expression a) a
  | ReferenceExpression AccessKind (Object a) a
  | Casting (Expression a) (TerminaType a) a
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
    (Object a) -- ^ Opion object
    MonadicVariantLabel -- ^ Variant label
    a
  | ArraySliceExpression AccessKind (Object a) (Expression a) (Expression a) a
  -- ^ TArray slice. This is a reference to an slisce of an array.
  deriving (Show, Functor)

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
    showText (StructInitializer fs _) = 
        "{" <> T.intercalate ", " (map showText fs) <> "}"
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
    showText (ArraySliceExpression ak obj lower upper _) = 
         showText ak <> " " <> showText obj <> "[" <> showText lower <> ".." <> showText upper <> "]"

instance Annotated Object where
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

instance Annotated Expression where
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
  getAnnotation (MonadicVariantInitializer _ a)    = a
  getAnnotation (MemberFunctionCall _ _ _ a)      = a
  getAnnotation (DerefMemberFunctionCall _ _ _ a) = a
  getAnnotation (IsEnumVariantExpression _ _ _ a) = a
  getAnnotation (IsMonadicVariantExpression _ _ a) = a
  getAnnotation (ArraySliceExpression _ _ _ _ a)    = a
  getAnnotation (StringInitializer _ a) = a

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
  updateAnnotation (MonadicVariantInitializer ov _) = MonadicVariantInitializer ov
  updateAnnotation (MemberFunctionCall obj f es _) = MemberFunctionCall obj f es
  updateAnnotation (DerefMemberFunctionCall obj f es _) = DerefMemberFunctionCall obj f es
  updateAnnotation (IsEnumVariantExpression obj id1 id2 _) = IsEnumVariantExpression obj id1 id2
  updateAnnotation (IsMonadicVariantExpression obj v _) = IsMonadicVariantExpression obj v
  updateAnnotation (ArraySliceExpression ak obj e1 e2 _) = ArraySliceExpression ak obj e1 e2
  updateAnnotation (StringInitializer s _) = StringInitializer s

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

data CondIf a = CondIf
  {
    condIfCond       :: Expression a
  , condIfBody       :: Block a
  , condIfAnnotation :: a
  } deriving (Show, Functor)

data CondElse a = CondElse
  {
    condElseBody       :: Block a
  , condElseAnnotation :: a
  } deriving (Show, Functor)

data CondElseIf a = CondElseIf
  {
    condElseIfCond       :: Expression a
  , condElseIfBody       :: Block a
  , condElseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    (TerminaType a) -- ^ type of the variable
    (Expression a) -- ^ initialization expression
    a
  | AssignmentStmt
    (Object a) -- ^ left hand side of the assignment
    (Expression a) -- ^ assignment expression
    a
  | IfElseStmt
    (CondIf a) -- ^ if condition and body
    [CondElseIf a] -- ^ list of else if blocks
    (Maybe (CondElse a)) -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    (TerminaType a) -- ^ type of iterator variable
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
  | ReturnStmt
    (Maybe (Expression a)) -- ^ return expression
    a
  | ContinueStmt
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
type TerminaType = TerminaType' Expression
type Parameter = Parameter' TerminaType
type Const = Const' TerminaType
type Modifier = Modifier' TerminaType
type FieldDefinition = FieldDefinition' TerminaType
type EnumVariant = EnumVariant' TerminaType
type MonadicVariant = MonadicVariant' Expression

type AnnASTElement = AnnASTElement' TerminaType Block Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TerminaType Expression

type TypeDef = TypeDef' TerminaType Block

type InterfaceMember = InterfaceMember' TerminaType
type ClassMember = ClassMember' TerminaType Block

type AnnotatedProgram a = [AnnASTElement' TerminaType Block Expression a]
