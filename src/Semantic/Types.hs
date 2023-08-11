{-# LANGUAGE DeriveFunctor #-}
-- | Semantic interpretation of types

module Semantic.Types where

import           AST
import           Utils.AST
import SemanAST as SAST

----------------------------------------
-- Semantic interpretation of types.
-- Termina types are the same through out all the transpilation process.
-- We only have weird ints going around.

----------------------------------------

-- | Global type information
data SemGlobal
  = SVolatile TypeSpecifier
  | SStatic TypeSpecifier
  | SShared TypeSpecifier
  | SConst TypeSpecifier
  deriving Show

-- type SemGlobal = SemGlobal' SemAnn

getTySemGlobal :: SemGlobal -> TypeSpecifier
getTySemGlobal (SVolatile ty) = ty
getTySemGlobal (SStatic ty)   = ty
getTySemGlobal (SShared ty)   = ty
getTySemGlobal (SConst ty)    = ty

----------------------------------------

-- | General global entities
data GEntry a
  = GFun
    { funArgs :: [Parameter]
    , funRet  :: TypeSpecifier
    }
  -- ^ Functions
  | GTask
    { taskArgs :: [Parameter]
    , taskRet  :: TypeSpecifier
    }
  -- ^ Tasks
  | GHand
  { handlerArgs :: [Parameter]
  , handlerRet  :: TypeSpecifier
  }
  -- ^ Handlers
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
type SemanTypeDef a = TypeDef'' (ClassMember' K SAST.LHSObject a)

-- Forgetfull Class member map
kClassMember :: ClassMember' exp lhs a -> ClassMember' K lhs a
kClassMember (ClassField idx tyx a) = ClassField idx tyx a
kClassMember (ClassMethod idx ps self blk ann) =
  ClassMethod idx ps self [] ann -- (BlockRet [] (ReturnStmt Nothing (returnAnnotation (blockRet blk))))

-- type GEntry = GEntry' SemAnn

----------------------------------------
-- Subtyping.
-- This fuction says what types can be casted into others.
casteableTys :: TypeSpecifier -> TypeSpecifier-> Bool
casteableTys UInt8 UInt16  = True
casteableTys UInt8 UInt32  = True
casteableTys UInt8 UInt64  = True
--
casteableTys UInt16 UInt32 = True
casteableTys UInt16 UInt64 = True
--
casteableTys UInt32 UInt64 = True
--
casteableTys Int8 Int16    = True
casteableTys Int8 Int32    = True
casteableTys Int8 Int64    = True
--
casteableTys Int16 Int32   = True
casteableTys Int16 Int64   = True
--
casteableTys Int32 Int64   = True
-- Last option being the same.
-- This is a trivial casting :muscle:
casteableTys a b           = groundTyEq a b

-- Relation between types
-- we use to define (dyn A \subseteq A)
subTypes :: TypeSpecifier -> TypeSpecifier -> Bool
subTypes (DynamicSubtype a) (DynamicSubtype b) = groundTyEq a b
subTypes (DynamicSubtype a) b = groundTyEq a b
subTypes a (DynamicSubtype b) = groundTyEq a b
-- Id \subseteq Subtypes
subTypes a b = groundTyEq a b
