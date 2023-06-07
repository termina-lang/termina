{-# LANGUAGE DeriveFunctor #-}
-- | Module where ASTs are defined.

module AST where

data ReturnStmt a 
  = ReturnStmt
  { 
    returnExpression :: Maybe (Expression a)
  , returnAnnotation :: a
  }
  deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data BlockRet a
  = BlockRet
  { 
    blockBody :: [Statement a]
  , blockRet  :: ReturnStmt a
  }
  deriving (Show, Functor)

-- | Annotated AST element
data AnnASTElement a =
  -- | Task construtor
  Task
    Identifier -- ^ task identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    TypeSpecifier -- ^ returned value type (should be TaskRet)
    (BlockRet a) -- ^ statements block
    [ Modifier a ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Function constructor
  | Function 
    Identifier -- ^ function identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    (Maybe TypeSpecifier) -- ^ type of the return value (optional)
    (BlockRet a) -- ^ statements block (with return)
    [ Modifier a ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Handler constructor
  | Handler
    Identifier -- ^ Handler identifier (name)
    [Parameter] -- ^ list of parameters (TBC)
    TypeSpecifier -- ^ returned value type (should be Result)
    (BlockRet a) -- ^ statements block (with return)
    [ Modifier a ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor  
  | GlobalDeclaration
    (Global a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef a) -- ^ the type definition (struct, union, etc.)
  
  -- | Module inclusion constructor
  | ModuleInclusion 
    Identifier -- ^ identifier of the module
    [ Modifier a ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  deriving (Show,Functor)


-- | This type represents constant expressions.
-- Since we are not implementing it right now, we only have constants.
-- The idea is to eventually replace it by constant (at compilation time)
-- expressions. We also annotate them for debbuging purposes.
data ConstExpression a = KC Const a
  deriving (Show,Functor)

-- | Modifier data type
-- Modifiers can be applied to different constructs. They must include
-- an identifier and also may define an expression.
data Modifier a = Modifier Identifier (Maybe (ConstExpression a))
  deriving (Show,Functor)

-- | Identifiers as `String`
type Identifier = String

-- | Addresses as `Integer`
type Address = Integer

-- | General type specifier
data TypeSpecifier
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char | DefinedType Identifier
  | Vector TypeSpecifier Size
  | MsgQueue TypeSpecifier Size
  | Pool TypeSpecifier Size
  | Option TypeSpecifier
  -- enum Option<T> {None | Some (a) }
  | Reference TypeSpecifier
  | DynamicSubtype TypeSpecifier
  -- See Q9
  | Unit
  deriving (Show)

newtype Size = K Integer
 deriving Show

data Op
  = MemberAccess
  | Multiplication
  | Division
  | Addition
  | Substraction
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

data OptBody a = None a | Some (Expression a) a
  deriving (Show, Functor)

data Expression a
  = Variable Identifier a
  | Constant Const a
  | Options (OptBody a)
  | BinOp Op (Expression a) (Expression a) a
  | ReferenceExpression (Expression a) a
  | Casting (Expression a) TypeSpecifier a
  | FunctionExpression Identifier [ Expression a ] a
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment a] a
  | EnumVariantExpression Identifier Identifier [ Expression a ] a
  | VectorIndexExpression (Expression a) (Expression a) a -- Binary operation : array indexing
  | VectorInitExpression (Expression a) (Expression a) a -- Vector initializer
  | MatchExpression (Expression a) [ MatchCase a ] a
  deriving (Show, Functor)

----------------------------------------
-- | Datatype representing Global Declarations.
-- There are three types of global declarations:
-- - volatile
-- - static
-- - shared
-- - const
data Global a
  = 
    -- | Volatile global variable constructor
    Volatile 
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      Address -- ^ address where the variable is located
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Static global variable constructor
    | Static
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (Expression a)) -- ^ initialization expression (optional)
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Shared global variable constructor
    | Shared
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (Expression a)) -- ^ initialization expression (optional)
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const 
      Identifier -- ^ name of the constant
      TypeSpecifier -- ^ type of the constant
      (Expression a) -- ^ initialization expression
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

data TypeDef a
  = Struct Identifier [FieldDefinition]  [ Modifier a ] a
  | Union Identifier [FieldDefinition] [ Modifier a ] a
  | Enum Identifier [EnumVariant] [ Modifier a ] a
  | Class Identifier [ClassMember a] [ Modifier a ] a
  deriving (Show, Functor)

data ClassMember a
  = ClassField Identifier TypeSpecifier
  | ClassMethod Identifier [Parameter] (Maybe TypeSpecifier) (BlockRet a) a
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

data FieldValueAssignment a = FieldValueAssignment {
  fieldAssigIdentifier   :: Identifier
  , fieldAssigExpression :: Expression a
} deriving (Show, Functor)

data FieldDefinition = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTypeSpecifier :: TypeSpecifier
} deriving (Show)

data EnumVariant = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ TypeSpecifier ]
} deriving (Show)

data MatchCase a = MatchCase
  { 
    matchIdentifier :: Identifier
  , matchBVars :: [Identifier]
  , matchBody :: BlockRet a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf a = ElseIf
  { 
    elseIfCond :: Expression a
  , elseIfBody :: Block a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement a =
  Declaration Identifier TypeSpecifier (Maybe (Expression a)) a
  | AssignmentStmt Identifier (Expression a) a
  | IfElseStmt (Expression a) (Block a) [ ElseIf a ] (Block a) a
  -- | For loop
  | ForLoopStmt Identifier (Expression a) (Expression a) (Maybe (Expression a)) (Block a) a
  | SingleExpStmt (Expression a) a
  -- | ReturnStmt (ReturnStmt a) [ a ]
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
-- - String literals
data Const = B Bool | I TypeSpecifier Integer | C Char -- | S String
  deriving (Show)

type AnnotatedProgram a = [AnnASTElement a]
type Block a = [Statement a]

-- When annotations are just `()` we get a normal ASTs and Programs
type AST = AnnASTElement ()
type Program = AnnotatedProgram ()

