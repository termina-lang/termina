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
  = STask TypeSpecifier
  | SResource TypeSpecifier
  | SHandler TypeSpecifier
  | SEmitter TypeSpecifier
  | SChannel TypeSpecifier
  | SConst TypeSpecifier Const
  deriving Show

-- | Semantic type information
data ESeman
  = SimpleType TypeSpecifier
  | ObjectType AccessKind TypeSpecifier
  | AppType [Parameter] TypeSpecifier
  | PortConnection ConnectionSeman
  deriving Show

data SSeman
  = SimpleStmtType -- ^ Statement with no type
    | MatchCaseStmtType [TypeSpecifier] -- ^ Match case with types
  deriving Show

data SemanProcedure = SemanProcedure Identifier [Parameter]
  deriving (Show)

data ConnectionSeman =
    -- | Access port connection
  APConnTy
  -- | Type specifier of the connected resource
    TypeSpecifier
    -- | List of procedures that can be called on the connected resource
    [SemanProcedure]
  | APAtomicConnTy
    -- | Type specifier of the connected atomic
    TypeSpecifier
  | APAtomicArrayConnTy
    -- | type specifier of the connected atomic array
    TypeSpecifier
    -- | Size of the connected atomic array
    Size
  | APPoolConnTy
    -- | Type specifier of the connected pool
    TypeSpecifier
    -- | Size of the connected pool
    Size
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    TypeSpecifier
    -- | Name of the action that will be triggered when the event emitter emits an event 
    Identifier 
  -- | In port connection
  | InPConnTy
    -- | Type specifier of the connected channel
    TypeSpecifier
    -- | Name of the action that will be triggered when the channel receives a message
    Identifier
  | OutPConnTy
    -- | Type specifier of the connected channel
    TypeSpecifier
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

getTySemGlobal :: SemGlobal -> TypeSpecifier
getTySemGlobal (STask ty) = ty
getTySemGlobal (SResource ty)   = ty
getTySemGlobal (SHandler ty)   = ty
getTySemGlobal (SEmitter ty)   = ty
getTySemGlobal (SChannel ty)   = ty
getTySemGlobal (SConst ty _) = ty

----------------------------------------

-- | General global entities
data GEntry a
  = GFun [Parameter] TypeSpecifier -- ^ const generic parameters, parameters, return type
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
casteableTys :: TypeSpecifier -> TypeSpecifier-> Bool
casteableTys a b = numTy a && numTy b

-- Relation between types
-- we use to define (box A \subseteq A)
subTypes :: TypeSpecifier -> TypeSpecifier -> Bool
subTypes (BoxSubtype a) (BoxSubtype b) = checkEqTypes a b
subTypes (BoxSubtype a) b              = checkEqTypes a b
subTypes a (BoxSubtype b)              = checkEqTypes a b
-- Id \subseteq Subtypes
subTypes a b                           = checkEqTypes a b
