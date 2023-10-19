-- | Utility AST functions

module Utils.AST.Parser where

import           AST.Parser

import Control.Arrow

-- Ground Type equality
groundTyEq :: TypeSpecifier -> TypeSpecifier -> Bool
groundTyEq  UInt8  UInt8 = True
groundTyEq  UInt16  UInt16 = True
groundTyEq  UInt32  UInt32 = True
groundTyEq  UInt64  UInt64 = True
groundTyEq  Int8  Int8 = True
groundTyEq  Int16  Int16 = True
groundTyEq  Int32  Int32 = True
groundTyEq  Int64  Int64 = True
groundTyEq  Bool  Bool = True
groundTyEq  Unit Unit = True
groundTyEq  (Option _) (Option Unit) = True
groundTyEq  (Option Unit) (Option _) = True
groundTyEq  (Option tyspecl) (Option tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference Mutable tyspecl) (Reference Mutable tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference Immutable tyspecl) (Reference Immutable tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (DynamicSubtype tyspecl) (DynamicSubtype tyspecr) = groundTyEq tyspecl tyspecr
-- TODO: These are considered complex types and should be handled differently
groundTyEq  (Vector typespecl sizeel) (Vector typespecr sizer) = groundTyEq typespecl typespecr && constExprEq sizeel sizer
groundTyEq  (DefinedType idl) (DefinedType idr) = idl == idr
-- Location subtypes
groundTyEq  (Location tyspecl) (Location tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Location tyspecl) tyspecr = groundTyEq tyspecl tyspecr
groundTyEq  tyspecl (Location tyspecr) = groundTyEq tyspecl tyspecr
--
groundTyEq  _ _ = False

constExprEq :: ConstExpression -> ConstExpression -> Bool
constExprEq (KC ((I tyspecl intl))) (KC ((I tyspecr intr))) = groundTyEq tyspecl tyspecr && intl == intr
constExprEq (KC (B vall)) (KC (B valr)) = vall == valr
constExprEq (KC (C charl)) (KC (C charr)) = charl == charr
constExprEq _ _ = False

-- Helper to detect invocations to 'self'
objIsSelf :: Object a -> Bool
objIsSelf (Variable ident _ann ) = ident == "self"
objIsSelf _ = False
