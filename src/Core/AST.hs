{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE KindSignatures    #-}

-- | Module defining core AST

module Core.AST where

import Utils.Annotations

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

-- | Annotated AST element
data AnnASTElement' ty blk expr a =
  -- | Function constructor
  Function
    Identifier -- ^ function identifier (name)
    [Parameter] -- ^ list of parameters (possibly empty)
    (Maybe TerminaType) -- ^ type of the return value (optional)
    (blk a) -- ^ statements block (with return)
    [ Modifier ] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor
  | GlobalDeclaration
    (Global' ty expr a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef' blk a) -- ^ the type definition (struct, union, etc.)
    a
  deriving (Show,Functor)

instance Annotated (AnnASTElement' ty blk expr) where
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

-- | Modifier data type
-- Modifiers can be applied to different constructs. They must include
-- an identifier and also may define an expression.
data Modifier = Modifier Identifier (Maybe Const)
  deriving (Show)

-- | Identifiers as `String`
type Identifier = String

-- | Addresses as `Integer`
type Address = TInteger

data TypeParameter =
  TypeParamName Identifier
  | TypeParamSize Size
  deriving (Show, Ord, Eq)

data TypeSpecifier
  = TSUInt8 | TSUInt16 | TSUInt32 | TSUInt64
  | TSInt8 | TSInt16 | TSInt32 | TSInt64 | TSUSize
  | TSBool | TSChar 
  | TSDefinedType [TypeParameter] Identifier
  -- Non-primitive types
  | TSReference AccessKind TypeSpecifier
  | TSBoxSubtype TypeSpecifier
  -- | Fixed-location types
  | TSLocation TypeSpecifier
  -- | Port types
  | TSAccessPort TypeSpecifier
  | TSSinkPort TypeSpecifier Identifier
  | TSInPort TypeSpecifier Identifier
  | TSOutPort TypeSpecifier  
  | TSUnit
  deriving (Show, Ord, Eq)

-- | Termina types
data TerminaType
  -- Primitive types
  = TUInt8 | TUInt16 | TUInt32 | TUInt64
  | TInt8 | TInt16 | TInt32 | TInt64 | TUSize
  | TBool | TChar | TDefinedType Identifier
  | TArray TerminaType Size
  | TOption TerminaType
  -- Built-in polymorphic types
  | TMsgQueue TerminaType Size -- Message queues
  | TPool TerminaType Size -- Memory pools
  | TAtomic TerminaType -- TAtomic variables
  | TAtomicArray TerminaType Size -- TAtomic arrays
  | TAllocator TerminaType -- Interface of memory pools
  | TAtomicAccess TerminaType -- Interface to access atomic variables
  | TAtomicArrayAccess TerminaType Size -- Interface to access atomic arrays
  -- Non-primitive types
  | TReference AccessKind TerminaType
  | TBoxSubtype TerminaType
  -- | Fixed-location types
  | TLocation TerminaType
  -- | Port types
  | TAccessPort TerminaType
  | TSinkPort TerminaType Identifier
  | TInPort TerminaType Identifier
  | TOutPort TerminaType
  -- See Q9
  | TUnit
  deriving (Show, Ord, Eq)

data AccessKind = Immutable | Mutable | Private
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
data Global' ty expr a
  =
    -- | Task global variable constructor
    Task
      Identifier -- ^ name of the task
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations
    -- | Shared resource global variable constructor
    | Resource
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Channel
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Emitter
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Handler global variable constructor
    | Handler
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const
      Identifier -- ^ name of the constant
      ty -- ^ type of the constant
      (expr a) -- ^ initialization expression
      [ Modifier ] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

instance Annotated (Global' ty expr) where
  getAnnotation (Task _ _ _ _ a) = a
  getAnnotation (Resource _ _ _ _ a)   = a
  getAnnotation (Channel _ _ _ _ a)    = a
  getAnnotation (Emitter _ _ _ _ a)    = a
  getAnnotation (Handler _ _ _ _ a)   = a
  getAnnotation (Const _ _ _ _ a)    = a

-- Extremelly internal type definition
data TypeDef' blk a
  = Struct Identifier [FieldDefinition]  [ Modifier ]
  | Enum Identifier [EnumVariant] [ Modifier ]
  | Class ClassKind Identifier [ClassMember' blk a] [Identifier] [ Modifier ]
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
data ClassMember' blk a
  = 
    -- | Fields. They form the state  of the object
    ClassField 
      FieldDefinition -- ^ the field
      a -- ^ transpiler annotation
    -- | Methods. Methods are internal functions that can access the
    -- state of the object and call other methods of the same class.
    | ClassMethod 
      Identifier  -- ^ name of the method
      (Maybe TerminaType) -- ^ type of the return value (optional)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    -- | Procedures. They can only be used on shared resources, and constitute their
    -- interface with the outside world. They define a list of parameters and a block
    -- of statements. They do not return any value.
    | ClassProcedure
      Identifier -- ^ name of the procedure
      [Parameter] -- ^ list of parameters (possibly empty)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassViewer
      Identifier -- ^ name of the viewer
      [Parameter] -- ^ list of parameters (possibly empty)
      (Maybe TerminaType) -- ^ return type of the viewer
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassAction 
      Identifier  -- ^ name of the method
      Parameter -- ^ input parameter
      TerminaType -- ^ type of the return value
      (blk a) -- ^ statements block (with return) a
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
  , paramTerminaType :: TerminaType -- ^ type of the parameter
} deriving (Show)

data FieldAssignment' expr a =
  FieldValueAssignment Identifier (expr a) a
  | FieldAddressAssignment Identifier Address a
  | FieldPortConnection PortConnectionKind Identifier Identifier a
  deriving (Show, Functor)

data FieldDefinition = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTerminaType :: TerminaType
} deriving (Show)

data EnumVariant = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ TerminaType ]
} deriving (Show)

-- | Constant values:
-- - Booleans
-- - Integers
-- - Characters
data Const = B Bool | I TInteger (Maybe TerminaType) | C Char
  deriving (Show)

----------------------------------------
-- Termina Programs definitions

data TerminaModule' ty blk expr pf a = Termina
  { modules :: [ Module' pf]
  , frags :: [ AnnASTElement' ty blk expr a ] }
  deriving Show

type AnnotatedProgram' ty blk expr a = [AnnASTElement' ty blk expr a]
