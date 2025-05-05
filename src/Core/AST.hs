{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE InstanceSigs #-}

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
    [Parameter' ty a] -- ^ list of parameters (possibly empty)
    (Maybe (ty a)) -- ^ type of the return value (optional)
    (blk a) -- ^ statements block (with return)
    [Modifier' ty a] -- ^ list of possible modifiers
    a -- ^ transpiler annotations

  -- | Global declaration constructor
  | GlobalDeclaration
    (Global' ty expr a) -- ^ the global object

  -- | Type definition constructor
  | TypeDefinition
    (TypeDef' ty blk a) -- ^ the type definition (struct, union, etc.)
    a
  deriving (Show, Functor)

instance Annotated (AnnASTElement' ty blk expr) where
  getAnnotation :: AnnASTElement' ty blk expr a -> a
  getAnnotation (Function _ _ _ _ _ a) = a
  getAnnotation (GlobalDeclaration glb) =  getAnnotation glb
  getAnnotation (TypeDefinition _ a) =  a

  updateAnnotation :: AnnASTElement' ty blk expr a -> a -> AnnASTElement' ty blk expr a
  updateAnnotation (Function n p r b m _) = Function n p r b m
  updateAnnotation (GlobalDeclaration glb) = GlobalDeclaration . updateAnnotation glb
  updateAnnotation (TypeDefinition t _) = TypeDefinition t

data ModuleImport' pf a = ModuleImport
  { moduleIdentifier ::  pf  -- Filepath!
  , moduleAnn :: a
  }
  deriving (Functor, Show)


-- | Modifier data type
-- Modifiers can be applied to different constructs. They must include
-- an identifier and also may define an expression.
data Modifier' ty a = Modifier Identifier (Maybe (Const' ty a))
  deriving (Show, Functor)

-- | Identifiers as `String`
type Identifier = String

-- | Addresses as `Integer`
type Address = TInteger

data TypeParameter' expr a =
  -- | Identifier that might be a defined type or a constant
  TypeParamIdentifier Identifier
  | TypeParamTypeSpec (TypeSpecifier' expr a)
  | TypeParamSize (expr a)
  deriving (Show, Ord, Eq, Functor)

data TypeSpecifier' expr a
  = TSUInt8 | TSUInt16 | TSUInt32 | TSUInt64
  | TSInt8 | TSInt16 | TSInt32 | TSInt64 | TSUSize
  | TSBool | TSChar 
  | TSConstSubtype (TypeSpecifier' expr a)
  | TSDefinedType Identifier [TypeParameter' expr a]
  | TSArray (TypeSpecifier' expr a) (expr a)
  -- Non-primitive types
  | TSReference AccessKind (TypeSpecifier' expr a)
  | TSBoxSubtype (TypeSpecifier' expr a)
  -- | Fixed-location types
  | TSLocation (TypeSpecifier' expr a)
  -- | Port types
  | TSAccessPort (TypeSpecifier' expr a)
  | TSSinkPort (TypeSpecifier' expr a) Identifier
  | TSInPort (TypeSpecifier' expr a) Identifier
  | TSOutPort (TypeSpecifier' expr a)  
  | TSUnit
  deriving (Show, Ord, Eq, Functor)

-- | Termina types
data TerminaType' expr a
  -- Primitive types
  = TUInt8 | TUInt16 | TUInt32 | TUInt64
  | TInt8 | TInt16 | TInt32 | TInt64 | TUSize
  | TBool | TChar
  | TStruct Identifier
  | TEnum Identifier
  | TInterface InterfaceKind Identifier
  | TArray (TerminaType' expr a) (expr a)
  -- Built-in polymorphic types
  | TOption (TerminaType' expr a)
  | TResult (TerminaType' expr a) (TerminaType' expr a)
  | TStatus (TerminaType' expr a)
  | TMsgQueue (TerminaType' expr a) (expr a) -- Message queues
  | TPool (TerminaType' expr a) (expr a) -- Memory pools
  | TAtomic (TerminaType' expr a) -- TAtomic variables
  | TAtomicArray (TerminaType' expr a) (expr a) -- TAtomic arrays
  | TAllocator (TerminaType' expr a) -- Interface of memory pools
  | TAtomicAccess (TerminaType' expr a) -- Interface to access atomic variables
  | TAtomicArrayAccess (TerminaType' expr a) (expr a) -- Interface to access atomic arrays
  -- Non-primitive types
  | TReference AccessKind (TerminaType' expr a)
  | TBoxSubtype (TerminaType' expr a)
  | TConstSubtype (TerminaType' expr a)
  -- | Fixed-location types
  | TFixedLocation (TerminaType' expr a)
  -- | Port types
  | TAccessPort (TerminaType' expr a)
  | TSinkPort (TerminaType' expr a) Identifier
  | TInPort (TerminaType' expr a) Identifier
  | TOutPort (TerminaType' expr a)
  -- | Unit type
  | TUnit
  -- | Global object types
  | TGlobal ClassKind Identifier
  deriving (Show, Functor)

instance Eq (TerminaType' expr a) where
  TUInt8 == TUInt8 = True
  TUInt16 == TUInt16 = True
  TUInt32 == TUInt32 = True
  TUInt64 == TUInt64 = True
  TInt8 == TInt8 = True
  TInt16 == TInt16 = True
  TInt32 == TInt32 = True
  TInt64 == TInt64 = True
  TStruct ident == TStruct ident' = ident == ident'
  TEnum ident == TEnum ident' = ident == ident'
  TBoxSubtype t == TBoxSubtype t' = t == t'
  TConstSubtype t == TConstSubtype t' = t == t'
  _ == _ = False

instance Ord (TerminaType' expr a) where
  TUInt8 `compare` TUInt8 = EQ
  TUInt16 `compare` TUInt16 = EQ
  TUInt32 `compare` TUInt32 = EQ
  TUInt64 `compare` TUInt64 = EQ
  TInt8 `compare` TInt8 = EQ
  TInt16 `compare` TInt16 = EQ
  TInt32 `compare` TInt32 = EQ
  TInt64 `compare` TInt64 = EQ
  TUSize `compare` TUSize = EQ
  TStruct ident `compare` TStruct ident' = ident `compare` ident'
  TEnum ident `compare` TEnum ident' = ident `compare` ident'
  TOption ty `compare` TOption ty' = ty `compare` ty'
  TStatus ty `compare` TStatus ty' = ty `compare` ty'
  TResult okTy errorTy `compare` TResult okTy' errorTy' = okTy `compare` okTy' <> errorTy `compare` errorTy'
  TBoxSubtype t `compare` TBoxSubtype t' = t `compare` t'
  TConstSubtype t `compare` TConstSubtype t' = t `compare` t'
  _ `compare` _ = LT

data AccessKind = Immutable | Mutable | Private
  deriving (Show, Ord, Eq)

data PortConnectionKind = InboundPortConnection | OutboundPortConnection | AccessPortConnection
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

data MonadicVariant' expr a = 
  None | Some (expr a) 
  | Ok (expr a) | Error (expr a) 
  | Success | Failure (expr a)
  deriving (Show, Functor)

data MonadicVariantLabel = NoneLabel | SomeLabel | OkLabel | ErrorLabel | SuccessLabel | FailureLabel
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
      (ty a) -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations
    -- | Shared resource global variable constructor
    | Resource
      Identifier -- ^ name of the variable
      (ty a) -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Channel
      Identifier -- ^ name of the variable
      (ty a) -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    | Emitter
      Identifier -- ^ name of the variable
      (ty a) -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Handler global variable constructor
    | Handler
      Identifier -- ^ name of the variable
      (ty a) -- ^ type of the variable
      (Maybe (expr a)) -- ^ initialization expression (optional)
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

    -- | Constant constructor
    | Const
      Identifier -- ^ name of the constant
      (ty a) -- ^ type of the constant
      (expr a) -- ^ initialization expression
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations
    -- | Constant expression constructor
    | ConstExpr
      Identifier -- ^ name of the constant
      (ty a) -- ^ type of the constant
      (expr a) -- ^ initialization expression
      [Modifier' ty a] -- ^ list of possible modifiers
      a -- ^ transpiler annotations

  deriving (Show, Functor)

instance Annotated (Global' ty expr) where
  getAnnotation (Task _ _ _ _ a) = a
  getAnnotation (Resource _ _ _ _ a)   = a
  getAnnotation (Channel _ _ _ _ a)    = a
  getAnnotation (Emitter _ _ _ _ a)    = a
  getAnnotation (Handler _ _ _ _ a)   = a
  getAnnotation (Const _ _ _ _ a)    = a
  getAnnotation (ConstExpr _ _ _ _ a)    = a

  updateAnnotation (Task n t i m _) = Task n t i m
  updateAnnotation (Resource n t i m _) = Resource n t i m
  updateAnnotation (Channel n t i m _) = Channel n t i m
  updateAnnotation (Emitter n t i m _) = Emitter n t i m
  updateAnnotation (Handler n t i m _) = Handler n t i m
  updateAnnotation (Const n t i m _) = Const n t i m
  updateAnnotation (ConstExpr n t i m _) = ConstExpr n t i m

-- Extremelly internal type definition
data TypeDef' ty blk a
  = Struct Identifier [FieldDefinition' ty a]  [Modifier' ty a]
  | Enum Identifier [EnumVariant' ty a] [Modifier' ty a]
  | Class ClassKind Identifier [ClassMember' ty blk a] [Identifier] [Modifier' ty a]
  | Interface InterfaceKind Identifier [Identifier] [InterfaceMember' ty a] [Modifier' ty a]
  deriving (Show, Functor)

data InterfaceKind = RegularInterface | SystemInterface
  deriving (Show, Ord, Eq)

data ClassKind = TaskClass | ResourceClass | HandlerClass | EmitterClass | ChannelClass 
  deriving (Show, Ord, Eq)

-------------------------------------------------
-- Interface Member
data InterfaceMember' ty a
  = 
    -- | Procedure
    InterfaceProcedure
      Identifier -- ^ name of the procedure
      [Parameter' ty a] -- ^ list of parameters (possibly empty)
      [Modifier' ty a] -- ^ list of possible modifiers
      a
  deriving (Show, Functor)

-------------------------------------------------
-- Class Member
data ClassMember' ty blk a
  = 
    -- | Fields. They form the state  of the object
    ClassField 
      (FieldDefinition' ty a) -- ^ the field
    -- | Methods. Methods are internal functions that can access the
    -- state of the object and call other methods of the same class.
    | ClassMethod 
      Identifier  -- ^ name of the method
      (Maybe (ty a)) -- ^ type of the return value (optional)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    -- | Procedures. They can only be used on shared resources, and constitute their
    -- interface with the outside world. They define a list of parameters and a block
    -- of statements. They do not return any value.
    | ClassProcedure
      Identifier -- ^ name of the procedure
      [Parameter' ty a] -- ^ list of parameters (possibly empty)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassViewer
      Identifier -- ^ name of the viewer
      [Parameter' ty a] -- ^ list of parameters (possibly empty)
      (Maybe (ty a)) -- ^ return type of the viewer
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassAction 
      Identifier  -- ^ name of the method
      (Parameter' ty a) -- ^ input parameter
      (ty a) -- ^ type of the return value
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
data Parameter' ty a = Parameter {
  paramIdentifier      :: Identifier -- ^ paramter identifier (name)
  , paramType :: ty a -- ^ type of the parameter
} deriving (Show, Functor)

data FieldAssignment' expr a =
  FieldValueAssignment Identifier (expr a) a
  | FieldAddressAssignment Identifier Address a
  | FieldPortConnection PortConnectionKind Identifier Identifier a
  deriving (Show, Functor)

data FieldDefinition' ty a = FieldDefinition {
  fieldIdentifier      :: Identifier
  , fieldTerminaType :: ty a
  , fieldAnnotation :: a
} deriving (Show, Functor)

data EnumVariant' ty a = EnumVariant {
  variantIdentifier :: Identifier
  , assocData       :: [ ty a ]
} deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Integers
-- - Characters
data Const' ty a = B Bool | I TInteger (Maybe (ty a)) | C Char
  deriving (Show, Functor)

----------------------------------------
-- Termina Programs definitions

data TerminaModule' ty blk expr pf a = Termina
  { modules :: [ModuleImport' pf a]
  , frags :: [AnnASTElement' ty blk expr a] }
  deriving Show

type AnnotatedProgram' ty blk expr a = [AnnASTElement' ty blk expr a]
