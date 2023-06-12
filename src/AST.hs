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
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Function constructor
  | Function 
    Identifier -- ^ function identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    (Maybe TypeSpecifier) -- ^ type of the return value (optional)
    (BlockRet a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Handler constructor
  | Handler
    Identifier -- ^ Handler identifier (name)
    [Parameter] -- ^ list of parameters (TBC)
    TypeSpecifier -- ^ returned value type (should be Result)
    (BlockRet a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
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

-- Ground Type equiality?
groundTyEq :: TypeSpecifier -> TypeSpecifier -> Bool
groundTyEq  UInt8  UInt8 = True
groundTyEq  UInt16  UInt16 = True
groundTyEq  UInt32  UInt32 = True
groundTyEq  UInt64  UInt64 = True
groundTyEq  Int8  Int8 = True
groundTyEq  Int16  Int16 = True
groundTyEq  Int32  Int32 = True
groundTyEq  Int64  Int64 = True
groundTyEq  Bool  Bool = True
groundTyEq  (Option tyspecl) (Option tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference tyspecl) (Reference tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq (DynamicSubtype tyspecl) (DynamicSubtype tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  _ _ = False

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

data OptionVariant a = None | Some (Expression a)
  deriving (Show, Functor)

data Expression a
  = Variable Identifier a
  | Constant Const a
  | ParensExpression (Expression a) a
  | BinOp Op (Expression a) (Expression a) a
  | VectorIndexExpression (Expression a) (Expression a) a -- ^ Binary operation : array indexing
  | ReferenceExpression (Expression a) a
  | DereferenceExpression (Expression a) a
  | Casting (Expression a) TypeSpecifier a
  | FunctionExpression Identifier [ Expression a ] a
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | VectorInitExpression (Expression a) ConstExpression a -- ^ Vector initializer
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment a] a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantExpression Identifier Identifier [ Expression a ] a
  | OptionVariantExpression (OptionVariant a) a
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
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Static global variable constructor
    | Static
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (Expression a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Shared global variable constructor
    | Shared
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (Expression a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const 
      Identifier -- ^ name of the constant
      TypeSpecifier -- ^ type of the constant
      (Expression a) -- ^ initialization expression
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

data TypeDef a
  = Struct Identifier [FieldDefinition]  [ Modifier ] a
  | Union Identifier [FieldDefinition] [ Modifier ] a
  | Enum Identifier [EnumVariant] [ Modifier ] a
  | Class Identifier [ClassMember a] [ Modifier ] a
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
  , matchBody :: Block a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf a = ElseIf
  { 
    elseIfCond :: Expression a
  , elseIfBody :: Block a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement a =
  -- | Declaration statement
  Declaration 
    Identifier -- ^ name of the variable
    TypeSpecifier -- ^ type of the variable
    (Expression a) -- ^ initialization expression
    a
  | AssignmentStmt 
    Identifier -- ^ name of the variable
    (Expression a) -- ^ assignment expression
    a
  | IfElseStmt
    (Expression a) -- ^ conditional expression
    [ Statement a ] -- ^ statements in the if block
    [ ElseIf a ] -- ^ list of else if blocks
    [ Statement a ] -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt 
    Identifier -- ^ name of the iterator variable
    (Expression a) -- ^ initial value of the iterator
    (Expression a) -- ^ final value of the iterator
    (Maybe (Expression a)) -- ^ break condition (optional)
    [ Statement a ] -- ^ statements in the for loop
    a
  | MatchStmt
    (Expression a) -- ^ expression to match
    [ MatchCase a ] -- ^ list of match cases
    a
  | SingleExpStmt
    (Expression a) -- ^ expression
    a
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

forgetAnnotations :: AnnotatedProgram a -> Program
forgetAnnotations = map (fmap (const ()))
