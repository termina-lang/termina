-- | Helpers from the semantic unit

module Semantic.Utils where

import Semantic.Monad
import Semantic.Types
import AST
import Utils.AST
import Annotations

getExpType :: Expression SemanticAnns -> Maybe TypeSpecifier
getExpType = getTySpec . ty_ann . getAnnotation

getElemSemanticInfo :: AnnASTElement SemanticAnns -> Maybe (GEntry SemanticAnns)
getElemSemanticInfo = getGEntry . ty_ann . getAnnotation
