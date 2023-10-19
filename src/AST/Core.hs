{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE KindSignatures    #-}

-- | Module defining core AST

module AST.Core where

import           Annotations

-- Parametric Emtpy Type
data Empty a
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
data BlockRet' expr obj a
  = BlockRet
  {
    blockBody :: [Statement' expr obj a]
  , blockRet  :: ReturnStmt' expr a
  }
  deriving (Show, Functor)

-- | Annotated AST element
data AnnASTElement' expr obj a =
  -- | Function constructor
  Function
    Identifier -- ^ function identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    (Maybe TypeSpecifier) -- ^ type of the return value (optional)
    (BlockRet' expr obj a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor
  | GlobalDeclaration
    (Global' expr a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef' expr obj a) -- ^ the type definition (struct, union, etc.)
    a
  deriving (Show,Functor)

instance Annotated (AnnASTElement' expr glb) where
  getAnnotation (Function _ _ _ _ _ a) = a
  getAnnotation (GlobalDeclaration glb) =  getAnnotation glb
  getAnnotation (TypeDefinition _ a) =  a

data Module' pf a = ModInclusion
  { moduleIdentifier ::  pf  -- Filepath!
  , moduleMods :: [ Modifier ]
  , moduleAnns :: a}
  deriving Show

modulePath :: (a -> b) -> Module' a c -> Module' b c
modulePath f m = m{moduleIdentifier = f (moduleIdentifier m)}

instance Annotated (Module' pf) where
  getAnnotation = moduleAnns

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
  | Reference AccessKind TypeSpecifier
  | DynamicSubtype TypeSpecifier
  -- | Fixed-location types
  | Location TypeSpecifier
  -- | Port types
  | Port TypeSpecifier
  -- See Q9
  | Unit
  deriving (Show)

data AccessKind = Immutable | Mutable | Private
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

----------------------------------------
-- | Datatype representing Global Declarations.
-- There are three types of global declarations:
-- - global objects  (tasks, resources or handlers)
-- - constants
data Global' expr a
  =
    -- | Task global variable constructor
    Task
      Identifier -- ^ name of the task
      TypeSpecifier -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Shared resource global variable constructor
    | Resource
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Handler global variable constructor
    | Handler
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
  getAnnotation (Task _ _ _ _ a) = a
  getAnnotation (Resource _ _ _ _ a)   = a
  getAnnotation (Handler _ _ _ _ a)   = a
  getAnnotation (Const _ _ _ _ a)    = a

-- Extremelly internal type definition
data TypeDef'' member
  = Struct Identifier [FieldDefinition]  [ Modifier ]
  | Enum Identifier [EnumVariant] [ Modifier ]
  | Class ClassKind Identifier [member] [ Modifier ]
  deriving (Show, Functor)

data ClassKind = TaskClass | ResourceClass | HandlerClass
  deriving (Show)

-- Type Defs are the above when composed with Class members.
type TypeDef' expr obj a = TypeDef'' (ClassMember' expr obj a)
-------------------------------------------------
-- Class Member
data ClassMember' expr obj a
  = 
    -- | Fields. They form the state  of the object
    ClassField 
      FieldDefinition -- ^ the field
      a -- ^ transpiler annotation
    -- | Methods. Methods are internal functions that can privately access the
    -- state of the object and call other methods of the same class.
    | ClassMethod 
      Identifier  -- ^ name of the method
      (Maybe TypeSpecifier) -- ^ type of the return value (optional)
      (BlockRet' expr obj a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    -- | Procedures. They can only be used on shared resources, and constitute their
    -- interface with the outside world. They define a list of parameters and a block
    -- of statements. They do not return any value.
    | ClassProcedure
      Identifier -- ^ name of the procedure
      [Parameter] -- ^ list of parameters (possibly empty)
      (Block' expr obj a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassViewer
      Identifier -- ^ name of the procedure
      [Parameter] -- ^ list of parameters (possibly empty)
      TypeSpecifier -- ^ return type of the viewer
      (BlockRet' expr obj a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
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

data FieldAssignment' expr a =
  FieldValueAssignment Identifier (expr a)
  | FieldAddressAssignment Identifier Address
  | FieldPortConnection Identifier Identifier
  deriving (Show, Functor)

data FieldDefinition = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTypeSpecifier :: TypeSpecifier
} deriving (Show)

data EnumVariant = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ TypeSpecifier ]
} deriving (Show)

data MatchCase' expr obj a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [Identifier]
  , matchBody       :: Block' expr obj a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf' expr obj a = ElseIf
  {
    elseIfCond       :: expr a
  , elseIfBody       :: Block' expr obj a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

  -- | First AST after parsing
data Expression'
    obj -- ^ objects type
    a -- ^ Annotations
  = AccessObject (obj a)
  | Constant Const a -- ^ | 24 : i8|
  | BinOp Op (Expression' obj a) (Expression' obj a) a
  | ReferenceExpression AccessKind (obj a) a
  | Casting (Expression' obj a) TypeSpecifier a
  -- Invocation expressions
  | FunctionExpression Identifier [ Expression' obj a ] a
  | MemberFunctionAccess (obj a) Identifier [Expression' obj a] a
  -- ^ Class method access | eI.name(x_{1}, ... , x_{n})|
  --
  -- These four constructors cannot be used on regular (primitive?) expressions
  -- These two can only be used as the RHS of an assignment:
  | VectorInitExpression (Expression' obj a) ConstExpression a -- ^ Vector initializer, | (13 : i8) + (2 : i8)|
  | FieldAssignmentsExpression
    Identifier -- ^ Structure type identifier
    [FieldAssignment' (Expression' obj) a] -- ^ Initial value of each field identifier
    a
  -- These two can only be used as the RHS of an assignment or as a case of a match expression:
  | EnumVariantExpression
    Identifier -- ^ Enum identifier
    Identifier -- ^ Variant identifier
    [ Expression' obj a ] -- ^ list of expressions
    a
  | OptionVariantExpression (OptionVariant (Expression' obj a)) a
  deriving (Show, Functor)

instance (Annotated obj) => Annotated (Expression' obj) where
  getAnnotation (AccessObject obj)                       = getAnnotation obj
  getAnnotation (Constant _ a)                           = a
  getAnnotation (BinOp _ _ _ a)                          = a
  getAnnotation (ReferenceExpression _ _ a)                = a
  getAnnotation (Casting _ _ a)                          = a
  getAnnotation (FunctionExpression _ _ a)               = a
  getAnnotation (FieldAssignmentsExpression _ _ a) = a
  getAnnotation (EnumVariantExpression _ _ _ a)          = a
  getAnnotation (VectorInitExpression _ _ a)             = a
  getAnnotation (OptionVariantExpression _ a)            = a
  getAnnotation (MemberFunctionAccess _ _ _ a)             = a


data Statement' expr obj a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    TypeSpecifier -- ^ type of the variable
    (expr a) -- ^ initialization expression
    a
  | AssignmentStmt
    (obj a) -- ^ name of the variable
    (expr a) -- ^ assignment expression
    a
  | IfElseStmt
    (expr a) -- ^ conditional expression
    [ Statement' expr obj a ] -- ^ statements in the if block
    [ ElseIf' expr obj a ] -- ^ list of else if blocks
    [ Statement' expr obj a ] -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    TypeSpecifier -- ^ type of iterator variable
    (expr a) -- ^ initial value of the iterator
    (expr a) -- ^ final value of the iterator
    (Maybe (expr a)) -- ^ break condition (optional)
    [ Statement' expr obj a ] -- ^ statements in the for loop
    a
  | MatchStmt
    (expr a) -- ^ expression to match
    [ MatchCase' expr obj a ] -- ^ list of match cases
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  | Free (obj a) a
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
data Const = B Bool | I TypeSpecifier Integer | C Char
  deriving (Show)

----------------------------------------
-- Termina Programs definitions

-- Blocks are just list of statements
type Block' expr obj a = [Statement' expr obj a]

data TerminaProgram' expr glb pf a b = Termina
  { modules :: [ Module' pf a]
  , frags :: [ AnnASTElement' expr glb b ] }
  deriving Show

type AnnotatedProgram' expr obj a = [AnnASTElement' expr obj a]
