-- | Utilities for Semantic AST

module Utils.AST.Seman where

import Utils.AST.Parser
import           AST.Seman

-- A relation of type specifier.
boxPromotion :: TypeSpecifier -> TypeSpecifier -> Bool
boxPromotion (BoxSubtype t) (BoxSubtype t') = checkEqTypes t t'
boxPromotion (BoxSubtype t) q = checkEqTypes t q
boxPromotion t (BoxSubtype q) = checkEqTypes t q
boxPromotion _ _ = False
-- TODO Q23

cleanBox :: TypeSpecifier -> TypeSpecifier
cleanBox (BoxSubtype t) = cleanBox t
cleanBox t = t

