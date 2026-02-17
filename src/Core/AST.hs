{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module defining core AST

module Core.AST where

import Utils.Annotations
import Utils.Printer
import qualified Data.Text as T
import Numeric
import Data.Bits

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

instance Num TInteger where
    (TInteger v1 r1) + (TInteger v2 _) = TInteger (v1 + v2) r1
    (TInteger v1 r1) - (TInteger v2 _) = TInteger (v1 - v2) r1
    (TInteger v1 r1) * (TInteger v2 _) = TInteger (v1 * v2) r1
    abs (TInteger v r) = TInteger (abs v) r
    signum (TInteger v r) = TInteger (signum v) r
    fromInteger v = TInteger v DecRepr

instance Real TInteger where
    toRational (TInteger v _) = toRational v

instance Enum TInteger where
    toEnum i = TInteger (toEnum i) DecRepr
    fromEnum (TInteger v _) = fromEnum v

instance Integral TInteger where
    toInteger (TInteger v _) = v
    quotRem (TInteger v1 r1) (TInteger v2 _) =
        let (q, r) = quotRem v1 v2 in
        (TInteger q r1, TInteger r r1)

instance Bits TInteger where
    (.&.) (TInteger v1 r1) (TInteger v2 _) = TInteger (v1 .&. v2) r1
    (.|.) (TInteger v1 r1) (TInteger v2 _) = TInteger (v1 .|. v2) r1
    xor (TInteger v1 r1) (TInteger v2 _) = TInteger (v1 `xor` v2) r1
    shiftL (TInteger v r) i = TInteger (shiftL v i) r
    shiftR (TInteger v r) i = TInteger (shiftR v i) r
    rotateL (TInteger v r) i = TInteger (rotateL v i) r
    rotateR (TInteger v r) i = TInteger (rotateR v i) r
    complement (TInteger v r) = TInteger (complement v) r
    bitSizeMaybe _ = Nothing
    bitSize _ = error "bitSize: TInteger is of infinite size"
    isSigned _ = True
    bit i = TInteger (bit i) DecRepr
    popCount (TInteger v _) = popCount v
    testBit (TInteger v _) = testBit v

instance ShowText TInteger where
    showText (TInteger value DecRepr) = T.pack $ show value
    showText (TInteger value HexRepr) = T.toUpper . T.pack $ "0x" <> showHex value ""
    showText (TInteger value OctalRepr) = T.pack ("0" <> showOct value "")

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

data TypeParameter' expr a =
  -- | Identifier that might be a defined type or a constant
  TypeParamIdentifier Identifier
  | TypeParamTypeSpec (TypeSpecifier' expr a)
  | TypeParamSize (expr a)
  deriving (Show, Ord, Eq, Functor)

data TypeSpecifier' expr a
  = TSUInt8 | TSUInt16 | TSUInt32 | TSUInt64
  | TSInt8 | TSInt16 | TSInt32 | TSInt64 | TSUSize
  | TSBool | TSChar
  | TSConstSubtype (TypeSpecifier' expr a)
  | TSDefinedType Identifier [TypeParameter' expr a]
  | TSArray (TypeSpecifier' expr a) (expr a)
  -- Non-primitive types
  | TSReference AccessKind (TypeSpecifier' expr a)
  | TSBoxSubtype (TypeSpecifier' expr a)
  -- | Fixed-location types
  | TSLocation (TypeSpecifier' expr a)
  -- | Port types
  | TSAccessPort (TypeSpecifier' expr a)
  | TSSinkPort (TypeSpecifier' expr a) Identifier
  | TSInPort (TypeSpecifier' expr a) Identifier
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
  | TStatus (TerminaType' expr a)
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

instance (ShowText (expr a)) => ShowText (TerminaType' expr a) where

    showText TUInt8 = "u8"
    showText TUInt16 = "u16"
    showText TUInt32 = "u32"
    showText TUInt64 = "u64"
    showText TInt8 = "i8"
    showText TInt16 = "i16"
    showText TInt32 = "i32"
    showText TInt64 = "i64"
    showText TUSize = "usize"
    showText TBool = "bool"
    showText TChar = "char"
    showText (TConstSubtype ts) = "const " <> showText ts
    showText (TStruct ident) = T.pack ident
    showText (TEnum ident) = T.pack ident
    showText (TInterface _ ident) = T.pack ident
    showText (TGlobal _ ident) = T.pack ident
    showText (TArray ts size) = "[" <> showText ts <> "; "  <> showText size <> "]"
    showText (TOption ty) = "Option<" <> showText ty <> ">"
    showText (TResult tyOk tyError) = "Result<" <> showText tyOk <> "; " <> showText tyError <> ">"
    showText (TStatus ty) = "Status<" <> showText ty <> ">"
    showText (TMsgQueue ts size) = "MsgQueue<" <> showText ts <> "; " <> showText size <> ">"
    showText (TPool ts size) = "Pool<" <> showText ts <> "; " <> showText size <> ">"
    showText (TAllocator ts) = "Allocator<" <> showText ts <> ">"
    showText (TAtomicAccess ts) = "AtomicAccess<" <> showText ts <> ">"
    showText (TAtomicArrayAccess ts size) = "AtomicArrayAccess<" <> showText ts <> "; " <> showText size <> ">"
    showText (TAtomic ts) = "Atomic<" <> showText ts <> ">"
    showText (TAtomicArray ts size) = "AtomicArray<" <> showText ts <> "; " <> showText size <> ">"
    showText (TReference ak ts) = "&" <> showText ak <> showText ts
    showText (TBoxSubtype ts) = "box " <> showText ts
    showText (TFixedLocation ts) = "loc " <> showText ts
    showText (TAccessPort ts) = "access " <> showText ts
    showText (TSinkPort ts ident) = "sink " <> showText ts <> " triggers " <> T.pack ident
    showText (TInPort ts ident) = "in " <> showText ts <> " triggers " <> T.pack ident
    showText (TOutPort ts) = "out " <> showText ts
    showText TUnit = "unit"

data AccessKind = Immutable | Mutable | Private
  deriving (Show, Ord, Eq)

instance ShowText AccessKind where
    showText Mutable = "mut "
    showText Private = "priv "
    showText Immutable = ""

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

instance ShowText Op where
    showText Addition = "+"
    showText Subtraction = "-"
    showText Multiplication = "*"
    showText Division = "/"
    showText Modulo = "%"
    showText BitwiseAnd = "&"
    showText BitwiseOr = "|"
    showText BitwiseXor = "^"
    showText BitwiseLeftShift = "<<"
    showText BitwiseRightShift = ">>"
    showText RelationalLT = "<"
    showText RelationalLTE = "<="
    showText RelationalGT = ">"
    showText RelationalGTE = ">="
    showText RelationalEqual = "=="
    showText RelationalNotEqual = "!="
    showText LogicalAnd = "&&"
    showText LogicalOr = "||"

data MonadicVariant' expr a =
  None | Some (expr a)
  | Ok (expr a) | Error (expr a)
  | Success | Failure (expr a)
  deriving (Show, Functor)

instance (ShowText (expr a)) => ShowText (MonadicVariant' expr a) where
    showText (Some e) = "Some(" <> showText e <> ")"
    showText None = "None"
    showText (Ok e) = "Ok(" <> showText e <> ")"
    showText (Error e) = "Error(" <> showText e <> ")"
    showText Success = "Success"
    showText (Failure e) = "Failure(" <> showText e <> ")"

data MonadicVariantLabel = NoneLabel | SomeLabel | OkLabel | ErrorLabel | SuccessLabel | FailureLabel
  deriving (Show)

instance ShowText MonadicVariantLabel where
    showText SomeLabel = "Some"
    showText NoneLabel = "None"
    showText OkLabel = "Ok"
    showText ErrorLabel = "Error"
    showText SuccessLabel = "Success"
    showText FailureLabel = "Failure"

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

instance ShowText (TypeDef' ty blk a) where
    showText (Struct ident _ _) = T.pack $ "struct " <> ident
    showText (Enum ident _ _) = T.pack $ "enum " <> ident
    showText (Class TaskClass ident _ _ _) = T.pack $ "task class " <> ident
    showText (Class ResourceClass ident _ _ _) = T.pack $ "resource class " <> ident
    showText (Class HandlerClass ident _ _ _) = T.pack $ "handler class " <> ident
    showText (Class EmitterClass ident _ _ _) = T.pack $ "emitter class " <> ident
    showText (Class ChannelClass ident _ _ _) = T.pack $ "channel class " <> ident
    showText (Interface RegularInterface ident _ _ _) = T.pack $ "interface " <> ident
    showText (Interface SystemInterface ident _ _ _) = T.pack $ "system interface " <> ident

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
      AccessKind -- ^ access kind (immutable, mutable)
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
    | ClassMethod
      AccessKind  -- ^ access kind (immutable, mutable or private)
      Identifier  -- ^ name of the method
      [Parameter' ty a] -- ^ list of parameters (possibly empty)
      (Maybe (ty a)) -- ^ type of the return value (optional)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    -- | Procedures. They can only be used on shared resources, and constitute their
    -- interface with the outside world. They define a list of parameters and a block
    -- of statements. They do not return any value.
    | ClassProcedure
      AccessKind -- ^ access kind (immutable, mutable or private)
      Identifier -- ^ name of the procedure
      [Parameter' ty a] -- ^ list of parameters (possibly empty)
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassViewer
      Identifier -- ^ name of the viewer
      [Parameter' ty a] -- ^ list of parameters (possibly empty)
      (Maybe (ty a)) -- ^ return type of the viewer
      (blk a) -- ^ statements block (with return) a
      a -- ^ transpiler annotation
    | ClassAction
      AccessKind  -- ^ access kind (immutable or private)
      Identifier  -- ^ name of the method
      (Maybe (Parameter' ty a)) -- ^ input parameter
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
  | FieldAddressAssignment Identifier (expr a) a
  | FieldPortConnection PortConnectionKind Identifier Identifier a
  deriving (Show, Functor)

instance (ShowText (expr a)) => ShowText (FieldAssignment' expr a) where
    showText (FieldValueAssignment ident expr _) =
        T.pack ident <> " = " <> showText expr
    showText (FieldAddressAssignment ident addr _) =
        T.pack ident <> " @ " <> showText addr
    showText (FieldPortConnection InboundPortConnection ident glb _) =
        T.pack ident <> " <- " <> T.pack glb
    showText (FieldPortConnection OutboundPortConnection ident glb _) =
        T.pack ident <> " -> " <> T.pack glb
    showText (FieldPortConnection AccessPortConnection ident glb _) =
        T.pack ident <> " <-> " <> T.pack glb

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
data Const' ty a = B Bool | I TInteger (Maybe (ty a)) | C Char | Null
  deriving (Show, Functor)


instance (ShowText (ty a)) => ShowText (Const' ty a) where
    showText (I i Nothing) = showText i
    showText (I i (Just ts)) = showText i <> " : " <> showText ts
    showText (B True) = "true"
    showText (B False) = "false"
    showText (C c) = T.pack [c]
    showText Null = "null"

----------------------------------------
-- Termina Programs definitions

data TerminaModule' ty blk expr pf a = Termina
  { modules :: [ModuleImport' pf a]
  , frags :: [AnnASTElement' ty blk expr a] }
  deriving Show

type AnnotatedProgram' ty blk expr a = [AnnASTElement' ty blk expr a]
