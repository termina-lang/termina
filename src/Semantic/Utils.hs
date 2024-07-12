-- | Helpers from the semantic unit

module Semantic.Utils where

import Semantic.Monad
import AST.Parser
import Utils.Annotations

getExpType :: Expression SemanticAnns -> Maybe TypeSpecifier
getExpType = getResultingType . ty_ann . getAnnotation
