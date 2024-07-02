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

-- | Integer representation.  
-- A value of this type is used to indicate the representation in which the
-- value was introduced. It will be later used by the code generator to generate
-- the correspoding literal using the same representation.
data IntRepr = DecRepr | HexRepr | OctalRepr
  deriving (Show, Eq, Ord)

-- | Termina integers
-- A Termina integer is defined by its value and its numeric representation.
data TInteger = TInteger Integer IntRepr
  deriving (Show, Eq, Ord)

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

data Module' pf = ModInclusion
  { moduleIdentifier ::  pf  -- Filepath!
  , moduleMods :: [ Modifier ]
  }
  deriving Show

instance Functor Module' where
  fmap f m = m{moduleIdentifier = f (moduleIdentifier m)}

-- | This type represents constant expressions.
-- Since we are not implementing it right now, we only have constants.
-- The idea is to eventually replace it by constant (at compilation time)
-- expressions. We also annotate them for debbuging purposes.
data ConstExpression a
  = KC Const a -- ^ Literal const expression
  | KV Identifier a -- ^ Identifier of a constant
  deriving (Show, Functor)

instance Annotated ConstExpression where
  getAnnotation (KC _ a) = a
  getAnnotation (KV _ a) = a

-- | Modifier data type
-- Modifiers can be applied to different constructs. They must include
-- an identifier and also may define an expression.
data Modifier = Modifier Identifier (Maybe Const)
  deriving (Show)

-- | Identifiers as `String`
type Identifier = String

-- | Addresses as `Integer`
type Address = TInteger

-- | General type specifier
data TypeSpecifier
  -- Primitive types
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64 | USize
  | Bool | Char | DefinedType Identifier
  | Array TypeSpecifier Size
  | Slice TypeSpecifier
  | Option TypeSpecifier
  -- Built-in polymorphic types
  | MsgQueue TypeSpecifier Size -- Message queues
  | Pool TypeSpecifier Size -- Memory pools
  | Atomic TypeSpecifier -- Atomic variables
  | AtomicArray TypeSpecifier Size -- Atomic arrays
  | Allocator TypeSpecifier -- Interface of memory pools
  | AtomicAccess TypeSpecifier -- Interface to access atomic variables
  | AtomicArrayAccess TypeSpecifier Size -- Interface to access atomic arrays
  -- Non-primitive types
  | Reference AccessKind TypeSpecifier
  | DynamicSubtype TypeSpecifier
  -- | Fixed-location types
  | Location TypeSpecifier
  -- | Port types
  | AccessPort TypeSpecifier
  | SinkPort TypeSpecifier Identifier
  | InPort TypeSpecifier Identifier
  | OutPort TypeSpecifier
  -- See Q9
  | Unit
  deriving (Show, Ord, Eq)

data AccessKind = Immutable | Mutable
  deriving (Show, Ord, Eq)

data PortConnectionKind = InboundPortConnection | OutboundPortConnection | AccessPortConnection
  deriving (Show, Ord, Eq) 

data Size = K TInteger | V Identifier
  deriving (Show, Ord, Eq) 

data Op
  = Multiplication
  | Division
  | Addition
  | Subtraction
  | Modulo
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

data OptionVariantLabel = NoneLabel | SomeLabel
  deriving (Show)

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

    | Channel
      Identifier -- ^ name of the variable
      TypeSpecifier -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Emitter
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
      (ConstExpression a) -- ^ initialization expression
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

instance Annotated (Global' expr) where
  getAnnotation (Task _ _ _ _ a) = a
  getAnnotation (Resource _ _ _ _ a)   = a
  getAnnotation (Channel _ _ _ _ a)    = a
  getAnnotation (Emitter _ _ _ _ a)    = a
  getAnnotation (Handler _ _ _ _ a)   = a
  getAnnotation (Const _ _ _ _ a)    = a

-- Extremelly internal type definition
data TypeDef' expr obj a
  = Struct Identifier [FieldDefinition]  [ Modifier ]
  | Enum Identifier [EnumVariant] [ Modifier ]
  | Class ClassKind Identifier [ClassMember' expr obj a] [Identifier] [ Modifier ]
  | Interface Identifier [InterfaceMember a] [ Modifier ]
  deriving (Show, Functor)

data ClassKind = TaskClass | ResourceClass | HandlerClass | EmitterClass | ChannelClass 
  deriving (Show)

-------------------------------------------------
-- Interface Member
data InterfaceMember a
  = 
    -- | Procedure
    InterfaceProcedure
      Identifier -- ^ name of the procedure
      [Parameter] -- ^ list of parameters (possibly empty)
      a
  deriving (Show, Functor)

-------------------------------------------------
-- Class Member
data ClassMember' expr obj a
  = 
    -- | Fields. They form the state  of the object
    ClassField 
      FieldDefinition -- ^ the field
      a -- ^ transpiler annotation
    -- | Methods. Methods are internal functions that can access the
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
      Identifier -- ^ name of the viewer
      [Parameter] -- ^ list of parameters (possibly empty)
      (Maybe TypeSpecifier) -- ^ return type of the viewer
      (BlockRet' expr obj a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassAction 
      Identifier  -- ^ name of the method
      Parameter -- ^ input parameter
      TypeSpecifier -- ^ type of the return value
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
  FieldValueAssignment Identifier (expr a) a
  | FieldAddressAssignment Identifier Address a
  | FieldPortConnection PortConnectionKind Identifier Identifier a
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
    (Maybe [ Statement' expr obj a ]) -- ^ statements in the else block
    a
  -- | For loop
  | ForLoopStmt
    Identifier -- ^ name of the iterator variable
    TypeSpecifier -- ^ type of iterator variable
    (ConstExpression a) -- ^ initial value of the iterator
    (ConstExpression a) -- ^ final value of the iterator
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
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Integers
-- - Characters
data Const = B Bool | I TInteger (Maybe TypeSpecifier) | C Char
  deriving (Show)

----------------------------------------
-- Termina Programs definitions

-- Blocks are just list of statements
type Block' expr obj a = [Statement' expr obj a]

data TerminaProgram' expr glb pf a b = Termina
  { modules :: [ Module' pf]
  , frags :: [ AnnASTElement' expr glb b ] }
  deriving Show

type AnnotatedProgram' expr obj a = [AnnASTElement' expr obj a]
