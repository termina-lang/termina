-- | Utilities for Semantic AST

module Utils.SemanAST where

import Utils.AST
import           SemanAST

-- A relation of type specifier.
dynPromotion :: TypeSpecifier -> TypeSpecifier -> Bool
dynPromotion (DynamicSubtype t) (DynamicSubtype t') = groundTyEq t t'
dynPromotion (DynamicSubtype t) q = groundTyEq t q
dynPromotion t (DynamicSubtype q) = groundTyEq t q
-- TODO Q23

cleanDyn :: TypeSpecifier -> TypeSpecifier
cleanDyn (DynamicSubtype t) = cleanDyn t
cleanDyn t = t
