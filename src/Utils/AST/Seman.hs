-- | Utilities for Semantic AST

module Utils.AST.Seman where

import Utils.AST.Parser
import           AST.Seman

-- A relation of type specifier.
dynPromotion :: TypeSpecifier -> TypeSpecifier -> Bool
dynPromotion (DynamicSubtype t) (DynamicSubtype t') = checkEqTypes t t'
dynPromotion (DynamicSubtype t) q = checkEqTypes t q
dynPromotion t (DynamicSubtype q) = checkEqTypes t q
dynPromotion _ _ = False
-- TODO Q23

cleanDyn :: TypeSpecifier -> TypeSpecifier
cleanDyn (DynamicSubtype t) = cleanDyn t
cleanDyn t = t

