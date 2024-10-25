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
    [Parameter' ty] -- ^ list of parameters (possibly empty)
    (Maybe ty) -- ^ type of the return value (optional)
    (blk a) -- ^ statements block (with return)
    [Modifier' ty] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor
  | GlobalDeclaration
    (Global' ty expr a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef' ty blk a) -- ^ the type definition (struct, union, etc.)
    a
  deriving (Show,Functor)

instance Annotated (AnnASTElement' ty blk expr) where
  getAnnotation (Function _ _ _ _ _ a) = a
  getAnnotation (GlobalDeclaration glb) =  getAnnotation glb
  getAnnotation (TypeDefinition _ a) =  a

data Module' ty pf = ModInclusion
  { moduleIdentifier ::  pf  -- Filepath!
  , moduleMods :: [Modifier' ty]
  }
  deriving Show

instance Functor (Module' ty) where
  fmap f m = m{moduleIdentifier = f (moduleIdentifier m)}

-- | Modifier data type
-- Modifiers can be applied to different constructs. They must include
-- an identifier and also may define an expression.
data Modifier' ty = Modifier Identifier (Maybe (Const' ty))
  deriving (Show)

-- | Identifiers as `String`
type Identifier = String

-- | Addresses as `Integer`
type Address = TInteger

data TypeParameter =
  TypeParamTypeSpec TypeSpecifier
  | TypeParamSize Size
  deriving (Show, Ord, Eq)

data TypeSpecifier
  = TSUInt8 | TSUInt16 | TSUInt32 | TSUInt64
  | TSInt8 | TSInt16 | TSInt32 | TSInt64 | TSUSize
  | TSBool | TSChar 
  | TSDefinedType Identifier [TypeParameter]
  | TSArray TypeSpecifier Size
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
  | TBool | TChar
  | TStruct Identifier
  | TEnum Identifier
  | TInterface Identifier
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
  -- | Unit type
  | TUnit
  -- | Global object types
  | TGlobal ClassKind Identifier
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
      [Modifier' ty] -- ^ list of possible modifiers
      a -- ^ transpiler annotations
    -- | Shared resource global variable constructor
    | Resource
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Channel
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Emitter
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Handler global variable constructor
    | Handler
      Identifier -- ^ name of the variable
      ty -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const
      Identifier -- ^ name of the constant
      ty -- ^ type of the constant
      (expr a) -- ^ initialization expression
      [Modifier' ty] -- ^ list of possible modifiers
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
data TypeDef' ty blk a
  = Struct Identifier [FieldDefinition' ty]  [Modifier' ty]
  | Enum Identifier [EnumVariant' ty] [Modifier' ty]
  | Class ClassKind Identifier [ClassMember' ty blk a] [Identifier] [Modifier' ty]
  | Interface Identifier [InterfaceMember' ty a] [Modifier' ty]
  deriving (Show, Functor)

data ClassKind = TaskClass | ResourceClass | HandlerClass | EmitterClass | ChannelClass 
  deriving (Show, Ord, Eq)

-------------------------------------------------
-- Interface Member
data InterfaceMember' ty a
  = 
    -- | Procedure
    InterfaceProcedure
      Identifier -- ^ name of the procedure
      [Parameter' ty] -- ^ list of parameters (possibly empty)
      a
  deriving (Show, Functor)

-------------------------------------------------
-- Class Member
data ClassMember' ty blk a
  = 
    -- | Fields. They form the state  of the object
    ClassField 
      (FieldDefinition' ty) -- ^ the field
      a -- ^ transpiler annotation
    -- | Methods. Methods are internal functions that can access the
    -- state of the object and call other methods of the same class.
    | ClassMethod 
      Identifier  -- ^ name of the method
      (Maybe ty) -- ^ type of the return value (optional)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    -- | Procedures. They can only be used on shared resources, and constitute their
    -- interface with the outside world. They define a list of parameters and a block
    -- of statements. They do not return any value.
    | ClassProcedure
      Identifier -- ^ name of the procedure
      [Parameter' ty] -- ^ list of parameters (possibly empty)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassViewer
      Identifier -- ^ name of the viewer
      [Parameter' ty] -- ^ list of parameters (possibly empty)
      (Maybe ty) -- ^ return type of the viewer
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassAction 
      Identifier  -- ^ name of the method
      (Parameter' ty) -- ^ input parameter
      ty -- ^ type of the return value
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
data Parameter' ty = Parameter {
  paramIdentifier      :: Identifier -- ^ paramter identifier (name)
  , paramType :: ty -- ^ type of the parameter
} deriving (Show)

data FieldAssignment' expr a =
  FieldValueAssignment Identifier (expr a) a
  | FieldAddressAssignment Identifier Address a
  | FieldPortConnection PortConnectionKind Identifier Identifier a
  deriving (Show, Functor)

data FieldDefinition' ty = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTerminaType :: ty
} deriving (Show)

data EnumVariant' ty = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ ty ]
} deriving (Show)

-- | Constant values:
-- - Booleans
-- - Integers
-- - Characters
data Const' ty = B Bool | I TInteger (Maybe ty) | C Char
  deriving (Show)

----------------------------------------
-- Termina Programs definitions

data TerminaModule' ty blk expr pf a = Termina
  { modules :: [Module' ty pf]
  , frags :: [AnnASTElement' ty blk expr a] }
  deriving Show

type AnnotatedProgram' ty blk expr a = [AnnASTElement' ty blk expr a]
