-- | The reading functions of "Core.Tree" applied to the elaborated AST, with the
-- names that "ControlFlow.Traversal" gives them for the lowered one.
module Elaboration.Traversal (
    Child'(..)
  , Child
  , expressionChildren
  , childExpressions
  , indexExpressions
  , objectPath
) where

import Elaboration.AST
import Core.Tree
import Semantic.Utils (AccessPath, objectPathOf)

-- | A child of a node of the elaborated AST.
type Child = Child' Expression Object

-- | The immediate children of an expression, in evaluation order.
expressionChildren :: Expression a -> [Child a]
expressionChildren = treeChildren elaboratedTree

childExpressions :: Expression a -> [Expression a]
childExpressions = childExpressionsOf elaboratedTree

indexExpressions :: Object a -> [Expression a]
indexExpressions = indexExpressionsOf elaboratedTree

objectPath :: Object a -> AccessPath
objectPath = objectPathOf elaboratedTree
