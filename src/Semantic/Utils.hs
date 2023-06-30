-- | Helpers from the semantic unit

module Semantic.Utils where

import Semantic.Monad
import Semantic.Types
import AST
import Utils.AST

getExpType :: Expression SemanticAnns -> Maybe TypeSpecifier
getExpType = getTySpec . ty_ann . getAnnotations

getElemSemanticInfo :: AnnASTElement SemanticAnns -> Maybe (GEntry SemanticAnns)
getElemSemanticInfo = getGEntry . ty_ann . getAnnotationsAST
