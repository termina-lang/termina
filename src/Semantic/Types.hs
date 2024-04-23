{-# LANGUAGE DeriveFunctor #-}
-- | Semantic interpretation of types

module Semantic.Types where

import           AST.Parser
import           AST.Seman            as SAST
import           Utils.AST.Parser
import           Utils.TypeSpecifier

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
  | SConst TypeSpecifier
  deriving Show

-- type SemGlobal = SemGlobal' SemAnn

getTySemGlobal :: SemGlobal -> TypeSpecifier
getTySemGlobal (STask ty) = ty
getTySemGlobal (SResource ty)   = ty
getTySemGlobal (SHandler ty)   = ty
getTySemGlobal (SEmitter ty)   = ty
getTySemGlobal (SChannel ty)   = ty
getTySemGlobal (SConst ty) = ty

----------------------------------------

-- | General global entities
data GEntry a
  = GFun [ConstParameter] [Parameter] TypeSpecifier -- ^ const generic parameters, parameters, return type
  -- ^ Functions
  | GGlob SemGlobal
  -- ^ Globals
  | GType (SemanTypeDef a)
  -- ^ Types
  deriving (Functor,Show)

-- Simple TypeDef
-- It only has type information.
-- Aux constant type type-constructor
data K a = K
  deriving (Functor, Show)
type SemanTypeDef a = TypeDef' K SAST.Object a

-- Forgetfull Class member map
kClassMember :: ClassMember' exp lhs a -> ClassMember' K lhs a
kClassMember (ClassField fld a) = ClassField fld a
kClassMember (ClassMethod idx ps _blk ann) =
  ClassMethod idx ps (BlockRet [] (ReturnStmt Nothing (returnAnnotation (blockRet _blk)))) ann
kClassMember (ClassProcedure idx cps ps _blk ann) =
  ClassProcedure idx cps ps [] ann
kClassMember (ClassViewer idx cps ps ty _blk ann) =
  ClassViewer idx cps ps ty (BlockRet [] (ReturnStmt Nothing (returnAnnotation (blockRet _blk)))) ann
kClassMember (ClassAction idx ps ty _blk ann) =
  ClassAction idx ps ty (BlockRet [] (ReturnStmt Nothing (returnAnnotation (blockRet _blk)))) ann

-- type GEntry = GEntry' SemAnn

----------------------------------------
-- Subtyping.
-- This fuction says what types can be casted into others.
casteableTys :: TypeSpecifier -> TypeSpecifier-> Bool
casteableTys a b = numTy a && numTy b
-- casteableTys UInt8 UInt16  = True
-- casteableTys UInt8 UInt32  = True
-- casteableTys UInt8 UInt64  = True
--
-- casteableTys UInt16 UInt32 = True
-- casteableTys UInt16 UInt64 = True
--
-- casteableTys UInt32 UInt64 = True
--
-- casteableTys Int8 Int16    = True
-- casteableTys Int8 Int32    = True
-- casteableTys Int8 Int64    = True
--
-- casteableTys Int16 Int32   = True
-- casteableTys Int16 Int64   = True
--
-- casteableTys Int32 Int64   = True
-- Last option being the same.
-- This is a trivial casting :muscle:
-- casteableTys a b           = groundTyEq a b

-- Relation between types
-- we use to define (dyn A \subseteq A)
subTypes :: TypeSpecifier -> TypeSpecifier -> Bool
subTypes (DynamicSubtype a) (DynamicSubtype b) = groundTyEq a b
subTypes (DynamicSubtype a) b                  = groundTyEq a b
subTypes a (DynamicSubtype b)                  = groundTyEq a b
-- Id \subseteq Subtypes
subTypes a b                                   = groundTyEq a b
