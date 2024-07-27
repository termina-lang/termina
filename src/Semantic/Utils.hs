-- | Helpers from the semantic unit

module Semantic.Utils where

import Semantic.Monad
import AST.Parser
import Utils.Annotations

getExpType :: Expression SemanticAnn -> Maybe TypeSpecifier
getExpType = getResultingType . getSemanticAnn . getAnnotation
