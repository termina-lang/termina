-- | Helpers from the semantic unit

module Semantic.Utils where

import Semantic.Monad
import Semantic.Types
import AST.Parser
import Annotations

getExpType :: Expression SemanticAnns -> Maybe TypeSpecifier
getExpType = getResultingType . ty_ann . getAnnotation

getExpSemType :: Expression SemanticAnns -> Maybe ESeman
getExpSemType = getEType . ty_ann . getAnnotation

getElemSemanticInfo :: AnnASTElement SemanticAnns -> Maybe (GEntry SemanticAnns)
getElemSemanticInfo = getGEntry . ty_ann . getAnnotation
