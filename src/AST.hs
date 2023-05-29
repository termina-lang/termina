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
    [Parameter a] -- ^ list of parameters (possibly empty)
    (TypeSpecifier a) -- ^ returned value type (should be TaskRet)
    (BlockRet a) -- ^ statements block
    [ Modifier a ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Function constructor
  | Function 
    Identifier -- ^ function identifier (name)
    [Parameter a] -- ^ list of parameters (possibly empty)
    (Maybe (TypeSpecifier a)) -- ^ type of the return value (optional)
    (BlockRet a) -- ^ statements block (with return)
    [ Modifier a ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Handler constructor
  | Handler
    Identifier -- ^ Handler identifier (name)
    [Parameter a] -- ^ list of parameters (TBC)
    (TypeSpecifier a) -- ^ returned value type (should be Result)
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

newtype ConstExpression a = KC (Const a)
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
data TypeSpecifier a
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char | DefinedType Identifier
  | Vector (TypeSpecifier a) Size
  | MsgQueue (TypeSpecifier a) Size
  | Pool (TypeSpecifier a) Size
  | Option (TypeSpecifier a)
  -- enum Option<T> {None | Some (a) }
  | Reference (TypeSpecifier a)
  | DynamicSubtype (TypeSpecifier a)
  -- See Q9
  | Unit
  deriving (Show, Functor)

newtype Size = K Integer
 deriving Show

-- Ground Type equiality?
groundTyEq :: TypeSpecifier a -> TypeSpecifier a -> Bool
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

data OptBody a = None a | Some (Expression a) a
  deriving (Show, Functor)

data Expression a
  = Variable Identifier
  | Constant (Const a)
  | Options (OptBody a)
  | BinOp Op (Expression a) (Expression a)
  | ReferenceExpression (Expression a)
  | Casting (Expression a) (TypeSpecifier a)
  | FunctionExpression Identifier [ Expression a ]
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment a]
  | EnumVariantExpression Identifier Identifier [ Expression a ]
  | VectorIndexExpression (Expression a) (Expression a) -- Binary operation : array indexing
  | VectorInitExpression (Expression a) (Expression a) -- Vector initializer
  | MatchExpression (Expression a) [ MatchCase a ]
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
      (TypeSpecifier a) -- ^ type of the variable
      Address -- ^ address where the variable is located
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Static global variable constructor
    | Static
      Identifier -- ^ name of the variable
      (TypeSpecifier a) -- ^ type of the variable
      (Maybe (Expression a)) -- ^ initialization expression (optional)
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Shared global variable constructor
    | Shared
      Identifier -- ^ name of the variable
      (TypeSpecifier a) -- ^ type of the variable
      (Maybe (Expression a)) -- ^ initialization expression (optional)
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const 
      Identifier -- ^ name of the constant
      (TypeSpecifier a) -- ^ type of the constant
      (Expression a) -- ^ initialization expression (optional)
      [ Modifier a ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

data TypeDef a
  = Struct Identifier [FieldDefinition a]  [ Modifier a ] a
  | Union Identifier [FieldDefinition a] [ Modifier a ] a
  | Enum Identifier [EnumVariant a] [ Modifier a ] a
  | Class Identifier [ClassMember a] [ Modifier a ] a
  deriving (Show, Functor)

data ClassMember a
  = ClassField Identifier (TypeSpecifier a) (Maybe (Expression a))  a
  | ClassMethod Identifier [Parameter a] (Maybe (TypeSpecifier a)) (BlockRet a) a
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
data Parameter a = Parameter {
  paramIdentifier      :: Identifier -- ^ paramter identifier (name)
  , paramTypeSpecifier :: TypeSpecifier a -- ^ type of the parameter
} deriving (Show, Functor)

data FieldValueAssignment a = FieldValueAssignment {
  fieldAssigIdentifier   :: Identifier
  , fieldAssigExpression :: Expression a
} deriving (Show, Functor)

data FieldDefinition a = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTypeSpecifier :: TypeSpecifier a
} deriving (Show, Functor)

data EnumVariant a = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ TypeSpecifier a ]
} deriving (Show, Functor)

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
  Declaration Identifier (TypeSpecifier a) (Maybe (Expression a)) a
  | AssignmentStmt Identifier (Expression a) a
  | IfElseStmt (Expression a) [ Statement a ] [ ElseIf a ] [ Statement a ] a
  -- | For loop
  | ForLoopStmt Identifier (Expression a) (Expression a) (Maybe (Expression a)) [ Statement a ] a
  | SingleExpStmt (Expression a) a
  -- | ReturnStmt (ReturnStmt a) [ a ]
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
-- - String literals
data Const a = B Bool | I (TypeSpecifier a) Integer | C Char -- | S String
  deriving (Show, Functor)

type AnnotatedProgram a = [AnnASTElement a]
type Block a = [Statement a]

-- When annotations are just `()` we get a normal ASTs and Programs
type AST = AnnASTElement ()
type Program = AnnotatedProgram ()

forgetAnnotations :: AnnotatedProgram a -> Program
forgetAnnotations = map (fmap (const ()))
