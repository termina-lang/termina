{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE KindSignatures    #-}
-- | Module defining core AST

module CoreAST where

import           Annotations

-- Parametric Emtpy Type
data Empty (a :: *)
  deriving (Show, Functor)

data ReturnStmt' expr a
  = ReturnStmt
  {
    returnExpression :: Maybe (expr a)
  , returnAnnotation :: a
  }
  deriving (Show, Functor)

instance Annotated (ReturnStmt' expr) where
  getAnnotation = returnAnnotation

instance HAnnotated ReturnStmt' where
  getHAnnotation = returnAnnotation

-- | |BlockRet| represent a body block with its return statement
data BlockRet' expr lho a
  = BlockRet
  {
    blockBody :: [Statement' expr lho a]
  , blockRet  :: ReturnStmt' expr a
  }
  deriving (Show, Functor)

-- | Annotated AST element
data AnnASTElement' expr lho a =
  -- | Task construtor
  Task
    Identifier -- ^ task identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    TypeSpecifier -- ^ returned value type (should be TaskRet)
    (BlockRet' expr lho a) -- ^ statements block
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Function constructor
  | Function
    Identifier -- ^ function identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    (Maybe TypeSpecifier) -- ^ type of the return value (optional)
    (BlockRet' expr lho a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Handler constructor
  | Handler
    Identifier -- ^ Handler identifier (name)
    [Parameter] -- ^ list of parameters (TBC)
    TypeSpecifier -- ^ returned value type (should be Result)
    (BlockRet' expr lho a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor
  | GlobalDeclaration
    (Global' expr a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef' expr lho a) -- ^ the type definition (struct, union, etc.)
    a

  -- | Module inclusion constructor
  | ModuleInclusion
    Identifier -- ^ identifier of the module
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  deriving (Show,Functor)

instance Annotated (AnnASTElement' expr glb) where
  getAnnotation (Task _ _ _ _ _ a) = a
  getAnnotation (Function _ _ _ _ _ a) = a
  getAnnotation (Handler _ _ _ _ _ a) = a
  getAnnotation (GlobalDeclaration glb) =  getAnnotation glb
  getAnnotation (TypeDefinition _ a) =  a
  getAnnotation (ModuleInclusion {}) =  error "Module Inclusion not defined yet "

-- | This type represents constant expressions.
-- Since we are not implementing it right now, we only have constants.
-- The idea is to eventually replace it by constant (at compilation time)
-- expressions. We also annotate them for debbuging purposes.
data ConstExpression
  = KC Const -- ^ Literal const expression
  | KV Identifier (Maybe Const) -- ^ Global constant? We keep the name for debugging process
  deriving (Show)

-- | Modifier data type
-- Modifiers can be applied to different constructs. They must include
-- an identifier and also may define an expression.
data Modifier = Modifier Identifier (Maybe ConstExpression)
  deriving (Show)

-- | Identifiers as `String`
type Identifier = String

-- | Addresses as `Integer`
type Address = Integer

-- | General type specifier
data TypeSpecifier
  -- Primitive types
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char | DefinedType Identifier
  | Vector TypeSpecifier ConstExpression
  | Option TypeSpecifier
  -- Non-primitive types
  | MsgQueue TypeSpecifier Size
  | Pool TypeSpecifier Size
  | Reference TypeSpecifier
  | DynamicSubtype TypeSpecifier
  -- See Q9
  | Unit
  deriving (Show)

newtype Size = K Integer
 deriving (Show)

data Op
  = Multiplication
  | Division
  | Addition
  | Subtraction
  | BitwiseLeftShift
  | BitwiseRightShift
  | RelationalLT
  | RelationalLTE
  | RelationalGT
  | RelationalGTE
  | RelationalEqual
  | RelationalNotEqual
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LogicalAnd
  | LogicalOr
  deriving Show

data OptionVariant expr = None | Some expr
  deriving (Show, Functor)

-- type FuncName = Annotated Identifier

----------------------------------------
-- | Datatype representing Global Declarations.
-- There are three types of global declarations:
-- - volatile
-- - static
-- - shared
-- - const
data Global' expr a
  =
    -- | Volatile global variable constructor
    Volatile
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      Address -- ^ address where the variable is located
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Static global variable constructor
    | Static
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Shared global variable constructor
    | Shared
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const
      Identifier -- ^ name of the constant
      TypeSpecifier -- ^ type of the constant
      (expr a) -- ^ initialization expression
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

instance Annotated (Global' expr) where
  getAnnotation (Volatile _ _ _ _ a) = a
  getAnnotation (Static _ _ _ _ a)   = a
  getAnnotation (Shared _ _ _ _ a)   = a
  getAnnotation (Const _ _ _ _ a)    = a

-- Extremelly internal type definition
data TypeDef'' (member :: *)
  = Struct Identifier [FieldDefinition]  [ Modifier ]
  | Union Identifier [FieldDefinition] [ Modifier ]
  | Enum Identifier [EnumVariant] [ Modifier ]
  | Class Identifier [member] [ Modifier ]
  deriving (Show, Functor)

-- Type Defs are the above when composed with Class members.
type TypeDef' expr lho a = TypeDef'' (ClassMember' expr lho a)
-------------------------------------------------
-- Class Member
data ClassMember' expr lho a
  -- | Either a Field, basically a variable of the class
  = ClassField Identifier TypeSpecifier a
  -- | Or a method. Methods come in two flavours whedata TypeDef' (expr :: * -> *) (a :: *)ther they use themselves
  -- through variable |self| (needed to invoke another method of the same class)
  -- Or not.
  | ClassMethod Identifier [Parameter] SelfMethod (Block' expr lho a) a
  deriving (Show, Functor)

data SelfMethod = Self | NoSelf
  deriving Show

-------------------------------------------------

----------------------------------------

-- | Parameter data type
--
-- This type constructor is used to build the parameters that are
-- listed as part of the definition of a function.
--
-- This type constructor takes two arguments:
-- - the identifier of the parameter
-- - the type of the parameter
data Parameter = Parameter {
  paramIdentifier      :: Identifier -- ^ paramter identifier (name)
  , paramTypeSpecifier :: TypeSpecifier -- ^ type of the parameter
} deriving (Show)

data FieldValueAssignment' expr a = FieldValueAssignment {
  fieldAssigIdentifier   :: Identifier
  , fieldAssigExpression :: expr a
} deriving (Show, Functor)

data FieldDefinition = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTypeSpecifier :: TypeSpecifier
} deriving (Show)

data EnumVariant = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ TypeSpecifier ]
} deriving (Show)

data MatchCase' expr lho a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [Identifier]
  , matchBody       :: Block' expr lho a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf' expr lho a = ElseIf
  {
    elseIfCond       :: expr a
  , elseIfBody       :: Block' expr lho a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)


  -- | First AST after parsing
data Expression' rho a
  = AccessObject (rho a)
  | Constant Const a -- ^ | 24 : i8|
  | ParensExpression (Expression' rho a) a
  | BinOp Op (Expression' rho a) (Expression' rho a) a
  | ReferenceExpression (rho a) a
  | DereferenceExpression (Expression' rho a) a
  | Casting (Expression' rho a) TypeSpecifier a
  -- Invocation expressions
  | FunctionExpression Identifier [ Expression' rho a ] a
  | MemberMethodAccess (rho a) Identifier [Expression' rho a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  --
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | VectorInitExpression (Expression' rho a) ConstExpression a -- ^ Vector initializer, | (13 : i8) + (2 : i8)|
  | FieldValuesAssignmentsExpression
    Identifier -- ^ Structure type identifier
    [FieldValueAssignment' (Expression' rho) a] -- ^ Initial value of each field identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantExpression
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [ Expression' rho a ] -- ^ list of expressions
    a
  | OptionVariantExpression (OptionVariant (Expression' rho a)) a
  deriving (Show, Functor)

instance (Annotated rho) => Annotated (Expression' rho) where
  getAnnotation (AccessObject obj)                       = getAnnotation obj
  getAnnotation (Constant _ a)                           = a
  getAnnotation (BinOp _ _ _ a)                          = a
  getAnnotation (ReferenceExpression _ a)                = a
  getAnnotation (Casting _ _ a)                          = a
  getAnnotation (FunctionExpression _ _ a)               = a
  getAnnotation (FieldValuesAssignmentsExpression _ _ a) = a
  getAnnotation (EnumVariantExpression _ _ _ a)          = a
  getAnnotation (VectorInitExpression _ _ a)             = a
  getAnnotation (ParensExpression _ a)                   = a
  getAnnotation (DereferenceExpression _ a)              = a
  getAnnotation (OptionVariantExpression _ a)            = a


data Statement' expr lho a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    TypeSpecifier -- ^ type of the variable
    (expr a) -- ^ initialization expression
    a
  | AssignmentStmt
    (lho a) -- ^ name of the variable
    (expr a) -- ^ assignment expression
    a
  | IfElseStmt
    (expr a) -- ^ conditional expression
    [ Statement' expr lho a ] -- ^ statements in the if block
    [ ElseIf' expr lho a ] -- ^ list of else if blocks
    [ Statement' expr lho a ] -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    (expr a) -- ^ initial value of the iterator
    (expr a) -- ^ final value of the iterator
    (Maybe (expr a)) -- ^ break condition (optional)
    [ Statement' expr lho a ] -- ^ statements in the for loop
    a
  | MatchStmt
    (expr a) -- ^ expression to match
    [ MatchCase' expr lho a ] -- ^ list of match cases
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
data Const = B Bool | I TypeSpecifier Integer | C Char
  deriving (Show)

type AnnotatedProgram' expr lho a = [AnnASTElement' expr lho a]
type Block' expr lho a = [Statement' expr lho a]

-- When annotations are just `()` we get a normal ASTs and Programs
-- type AST = AnnASTElement : ()
-- type Program = AnnotatedProgram ()
