{-# LANGUAGE DeriveFunctor #-}
-- | Semantic interpretation of types

module Semantic.Types where

import Semantic.AST
import Core.Utils
import Utils.Annotations

----------------------------------------
-- Semantic interpretation of types.
-- Termina types are the same through out all the transpilation process.
-- We only have weird ints going around.

----------------------------------------

-- | Global type information
data SemGlobal
  = STask TerminaType
  | SResource TerminaType
  | SHandler TerminaType
  | SEmitter TerminaType
  | SChannel TerminaType
  | SConst TerminaType Const
  deriving Show

-- | Semantic type information
data ESeman
  = SimpleType TerminaType
  | ObjectType AccessKind TerminaType
  | AppType [TerminaType] TerminaType
  | PortConnection ConnectionSeman
  deriving Show

data SSeman
  = SimpleStmtType -- ^ Statement with no type
    | MatchCaseStmtType [TerminaType] -- ^ Match case with types
  deriving Show

data SemanProcedure = SemanProcedure Identifier [Parameter]
  deriving (Show)

data ConnectionSeman =
    -- | Access port connection
  APConnTy
  -- | Type specifier of the connected resource
    TerminaType
    -- | List of procedures that can be called on the connected resource
    [SemanProcedure]
  | APAtomicConnTy
    -- | Type specifier of the connected atomic
    TerminaType
  | APAtomicArrayConnTy
    -- | type specifier of the connected atomic array
    TerminaType
    -- | Size of the connected atomic array
    Size
  | APPoolConnTy
    -- | Type specifier of the connected pool
    TerminaType
    -- | Size of the connected pool
    Size
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    TerminaType
    -- | Name of the action that will be triggered when the event emitter emits an event 
    Identifier 
  -- | In port connection
  | InPConnTy
    -- | Type specifier of the connected channel
    TerminaType
    -- | Name of the action that will be triggered when the channel receives a message
    Identifier
  | OutPConnTy
    -- | Type specifier of the connected channel
    TerminaType
  deriving Show

-- | Semantic elements
-- we have three different semantic elements:
data SemanticElems
  =
  -- | Expressions with their types
  ETy ESeman
  -- | Statements 
  | STy SSeman
  -- | Global elements
  | GTy (GEntry SemanticAnn)
  deriving Show

-- | Expression Semantic Annotations
type SemanticAnn = Located SemanticElems

getTySemGlobal :: SemGlobal -> TerminaType
getTySemGlobal (STask ty) = ty
getTySemGlobal (SResource ty)   = ty
getTySemGlobal (SHandler ty)   = ty
getTySemGlobal (SEmitter ty)   = ty
getTySemGlobal (SChannel ty)   = ty
getTySemGlobal (SConst ty _) = ty

----------------------------------------

-- | General global entities
data GEntry a
  = GFun [TerminaType] TerminaType -- ^ const generic parameters, parameters, return type
  -- ^ Functions
  | GGlob SemGlobal
  -- ^ Globals
  | GType (SemanTypeDef a)
  -- ^ Types
  deriving (Functor,Show)

-- Simple TypeDef
-- It only has type information.
-- Aux constant type type-constructor
data EmptyBlockRet a = EmptyBlockRet
  deriving (Show, Functor)
type SemanTypeDef a = TypeDef' EmptyBlockRet a

-- Forgetfull Class member map
kClassMember :: ClassMember' blk a -> ClassMember' EmptyBlockRet a
kClassMember (ClassField fld a) = ClassField fld a
kClassMember (ClassMethod idx ps _blk ann) =
  ClassMethod idx ps EmptyBlockRet ann
kClassMember (ClassProcedure idx ps _blk ann) =
  ClassProcedure idx ps EmptyBlockRet ann
kClassMember (ClassViewer idx ps ty _blk ann) =
  ClassViewer idx ps ty EmptyBlockRet ann
kClassMember (ClassAction idx ps ty _blk ann) =
  ClassAction idx ps ty EmptyBlockRet ann

----------------------------------------
-- Subtyping.
-- This fuction says what types can be casted into others.
casteableTys :: TerminaType -> TerminaType-> Bool
casteableTys a b = numTy a && numTy b

-- Relation between types
-- we use to define (box A \subseteq A)
subTypes :: TerminaType -> TerminaType -> Bool
subTypes (TBoxSubtype a) (TBoxSubtype b) = sameTy a b
subTypes (TBoxSubtype a) b              = sameTy a b
subTypes a (TBoxSubtype b)              = sameTy a b
-- Id \subseteq Subtypes
subTypes a b                           = sameTy a b
