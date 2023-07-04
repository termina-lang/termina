{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
-- | Module defining core AST

module CoreAST where

data ReturnStmt' expr a
  = ReturnStmt
  {
    returnExpression :: Maybe (expr a)
  , returnAnnotation :: a
  }
  deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data BlockRet' expr a
  = BlockRet
  {
    blockBody :: [Statement' expr a]
  , blockRet  :: ReturnStmt' expr a
  }
  deriving (Show, Functor)

-- | Annotated AST element
data AnnASTElement' expr a =
  -- | Task construtor
  Task
    Identifier -- ^ task identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    TypeSpecifier -- ^ returned value type (should be TaskRet)
    (BlockRet' expr a) -- ^ statements block
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Function constructor
  | Function
    Identifier -- ^ function identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    (Maybe TypeSpecifier) -- ^ type of the return value (optional)
    (BlockRet' expr a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Handler constructor
  | Handler
    Identifier -- ^ Handler identifier (name)
    [Parameter] -- ^ list of parameters (TBC)
    TypeSpecifier -- ^ returned value type (should be Result)
    (BlockRet' expr a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor
  | GlobalDeclaration
    (Global' expr a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef' expr a) -- ^ the type definition (struct, union, etc.)

  -- | Module inclusion constructor
  | ModuleInclusion
    Identifier -- ^ identifier of the module
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  deriving (Show,Functor)

-- | This type represents constant expressions.
-- Since we are not implementing it right now, we only have constants.
-- The idea is to eventually replace it by constant (at compilation time)
-- expressions. We also annotate them for debbuging purposes.
newtype ConstExpression = KC Const
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
  = MemberAccess
  | Multiplication
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

data TypeDef' (expr :: * -> *) (a :: *)
  = Struct Identifier [FieldDefinition]  [ Modifier ] a
  | Union Identifier [FieldDefinition] [ Modifier ] a
  | Enum Identifier [EnumVariant] [ Modifier ] a
  | Class Identifier [ClassMember' expr a] [ Modifier ] a
  deriving (Show, Functor)

-- instance Functor (TypeDef' expr) where

data ClassMember' expr a
  = ClassField Identifier TypeSpecifier
  | ClassMethod Identifier [Parameter] (Maybe TypeSpecifier) (BlockRet' expr a) a
  deriving (Show, Functor)

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

data MatchCase' expr a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars :: [Identifier]
  , matchBody :: Block' expr a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf' expr a = ElseIf
  {
    elseIfCond :: expr a
  , elseIfBody :: Block' expr a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement' expr a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    TypeSpecifier -- ^ type of the variable
    (expr a) -- ^ initialization expression
    a
  | AssignmentStmt
    Identifier -- ^ name of the variable
    (expr a) -- ^ assignment expression
    a
  | IfElseStmt
    (expr a) -- ^ conditional expression
    [ Statement' expr a ] -- ^ statements in the if block
    [ ElseIf' expr a ] -- ^ list of else if blocks
    [ Statement' expr a ] -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    (expr a) -- ^ initial value of the iterator
    (expr a) -- ^ final value of the iterator
    (Maybe (expr a)) -- ^ break condition (optional)
    [ Statement' expr a ] -- ^ statements in the for loop
    a
  | MatchStmt
    (expr a) -- ^ expression to match
    [ MatchCase' expr a ] -- ^ list of match cases
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
-- - String literals
data Const = B Bool | I TypeSpecifier Integer | C Char -- | S String
  deriving (Show)

type AnnotatedProgram' expr a = [AnnASTElement' expr a]
type Block' expr a = [Statement' expr a]

-- When annotations are just `()` we get a normal ASTs and Programs
-- type AST = AnnASTElement : ()
-- type Program = AnnotatedProgram ()
