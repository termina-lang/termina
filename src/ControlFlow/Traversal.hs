-- | Structural recursion over the basic-block ASTs.
--
-- Every pass over these ASTs walks the same tree and differs only in what it
-- does at a handful of positions, so the shape of the walk is written here once
-- and each pass says what those positions mean to it. The children of an
-- expression come back tagged with the role they play, because the passes do
-- not agree on all of them: a call argument moves a box in the linearity check
-- and is a plain read everywhere else, and the constant expressions that come
-- from a type or an address are part of the program text but not of the
-- computation.
--
-- What a pass may not delegate here is the control flow. 'simpleBlockChildren'
-- covers the blocks that only evaluate expressions and returns 'Nothing' for the
-- four that branch, which every pass has to interpret for itself.
--
-- The reading functions are the ones of "Core.Tree" applied to the lowered
-- AST. The rewriting side is written for the lowered AST, which is the one the
-- constant folding rewrites.
module ControlFlow.Traversal (
    Child'(..)
  , Child
  , ObjectVisitor'(..)
  , ObjectVisitor
  , Rewriter(..)
  , expressionChildren
  , simpleBlockChildren
  , childExpressions
  , walkObject
  , rootIdent
  , indexExpressions
  , rewriteExpression
  , rewriteObject
  , rewriteFieldAssignment
) where

import Lowering.AST
import Semantic.AST (semanticTree)
import Core.Tree

import Data.Maybe (maybeToList)

-- | A child of a node of the lowered AST.
type Child = Child' Expression Object

type ObjectVisitor = ObjectVisitor' Expression Object

-- | The immediate children of an expression, in evaluation order.
expressionChildren :: Expression a -> [Child a]
expressionChildren = treeChildren semanticTree

childExpressions :: Expression a -> [Expression a]
childExpressions = childExpressionsOf semanticTree

rootIdent :: Object a -> Identifier
rootIdent = rootIdentOf semanticTree

indexExpressions :: Object a -> [Expression a]
indexExpressions = indexExpressionsOf semanticTree

walkObject :: Monad m => ObjectVisitor m a -> Object a -> m ()
walkObject = walkObjectOf semanticTree

-- | The children of a basic block that only evaluates expressions, in
-- evaluation order. The blocks that branch return 'Nothing', since what they
-- mean to a pass is not the list of expressions they contain.
simpleBlockChildren :: BasicBlock' ty expr obj a -> Maybe [Child' expr obj a]
simpleBlockChildren bb = case bb of
  SendMessage obj expr _ -> Just [ChildObject obj, ChildExpr expr]
  ProcedureInvoke obj _ args _ -> Just (ChildObject obj : map ChildArg args)
  SystemCall obj _ args _ -> Just (ChildObject obj : map ChildArg args)
  AtomicLoad obj expr _ -> Just [ChildObject obj, ChildExpr expr]
  AtomicStore obj expr _ -> Just [ChildObject obj, ChildExpr expr]
  AtomicArrayLoad obj index expr _ -> Just [ChildObject obj, ChildExpr index, ChildExpr expr]
  AtomicArrayStore obj index expr _ -> Just [ChildObject obj, ChildExpr index, ChildExpr expr]
  AllocBox obj expr _ -> Just [ChildObject obj, ChildExpr expr]
  FreeBox obj expr _ -> Just [ChildObject obj, ChildExpr expr]
  ReturnBlock mExpr _ -> Just (map ChildExpr (maybeToList mExpr))
  ContinueBlock expr _ -> Just [ChildExpr expr]
  RebootBlock _ -> Just []
  IfElseBlock {} -> Nothing
  ForLoopBlock {} -> Nothing
  MatchBlock {} -> Nothing
  RegularBlock {} -> Nothing

-- | How to rewrite what a node holds. A pass that produces a new AST instead of
-- reading the one it walks gives these three functions and gets the rebuilding
-- of every node for free. Each of the three rewrites one level, so the recursion
-- is the caller's: a pass passes its own rewriting functions in here.
data Rewriter f a = Rewriter
  {
    onExpression :: Expression a -> f (Expression a)
  , onObject :: Object a -> f (Object a)
  , onAnnotation :: a -> f a
  }

-- | Rebuilds an access path from the rewriting of what it holds. The annotation
-- of a node is rewritten before the rest, so a pass whose rewriting can fail
-- reports the outermost annotation first.
rewriteObject :: Applicative f => Rewriter f a -> Object a -> f (Object a)
rewriteObject r obj = case obj of
  Variable ident ann -> Variable ident <$> onAnnotation r ann
  ArrayIndexExpression inner index ann ->
    (\ann' inner' index' -> ArrayIndexExpression inner' index' ann')
      <$> onAnnotation r ann <*> onObject r inner <*> onExpression r index
  MemberAccess inner ident ann ->
    (\ann' inner' -> MemberAccess inner' ident ann')
      <$> onAnnotation r ann <*> onObject r inner
  Dereference inner ann ->
    (\ann' inner' -> Dereference inner' ann') <$> onAnnotation r ann <*> onObject r inner
  DereferenceMemberAccess inner ident ann ->
    (\ann' inner' -> DereferenceMemberAccess inner' ident ann')
      <$> onAnnotation r ann <*> onObject r inner
  Unbox inner ann ->
    (\ann' inner' -> Unbox inner' ann') <$> onAnnotation r ann <*> onObject r inner

-- | Rebuilds an expression, in the same order as 'rewriteObject'.
rewriteExpression :: Applicative f => Rewriter f a -> Expression a -> f (Expression a)
rewriteExpression r expr = case expr of
  AccessObject obj -> AccessObject <$> onObject r obj
  Constant c ann -> Constant c <$> onAnnotation r ann
  StringInitializer str ann -> StringInitializer str <$> onAnnotation r ann
  BinOp op left right ann ->
    (\ann' left' right' -> BinOp op left' right' ann')
      <$> onAnnotation r ann <*> onExpression r left <*> onExpression r right
  ReferenceExpression ak obj ann ->
    (\ann' obj' -> ReferenceExpression ak obj' ann')
      <$> onAnnotation r ann <*> onObject r obj
  Casting inner ty ann ->
    (\ann' inner' -> Casting inner' ty ann') <$> onAnnotation r ann <*> onExpression r inner
  IsEnumVariantExpression obj enum variant ann ->
    (\ann' obj' -> IsEnumVariantExpression obj' enum variant ann')
      <$> onAnnotation r ann <*> onObject r obj
  IsMonadicVariantExpression obj label ann ->
    (\ann' obj' -> IsMonadicVariantExpression obj' label ann')
      <$> onAnnotation r ann <*> onObject r obj
  ArraySliceExpression ak obj lower upper ann ->
    (\ann' obj' lower' upper' -> ArraySliceExpression ak obj' lower' upper' ann')
      <$> onAnnotation r ann <*> onObject r obj
      <*> onExpression r lower <*> onExpression r upper
  MemberFunctionCall obj ident args ann ->
    (\ann' obj' args' -> MemberFunctionCall obj' ident args' ann')
      <$> onAnnotation r ann <*> onObject r obj <*> traverse (onExpression r) args
  DerefMemberFunctionCall obj ident args ann ->
    (\ann' obj' args' -> DerefMemberFunctionCall obj' ident args' ann')
      <$> onAnnotation r ann <*> onObject r obj <*> traverse (onExpression r) args
  FunctionCall ident args ann ->
    (\ann' args' -> FunctionCall ident args' ann')
      <$> onAnnotation r ann <*> traverse (onExpression r) args
  ArrayInitializer inner size ann ->
    (\ann' inner' size' -> ArrayInitializer inner' size' ann')
      <$> onAnnotation r ann <*> onExpression r inner <*> onExpression r size
  ArrayExprListInitializer exprs ann ->
    (\ann' exprs' -> ArrayExprListInitializer exprs' ann')
      <$> onAnnotation r ann <*> traverse (onExpression r) exprs
  StructInitializer fields ann ->
    (\ann' fields' -> StructInitializer fields' ann')
      <$> onAnnotation r ann <*> traverse (rewriteFieldAssignment r) fields
  EnumVariantInitializer enumId variantId args ann ->
    (\ann' args' -> EnumVariantInitializer enumId variantId args' ann')
      <$> onAnnotation r ann <*> traverse (onExpression r) args
  MonadicVariantInitializer variant ann ->
    (\ann' variant' -> MonadicVariantInitializer variant' ann')
      <$> onAnnotation r ann <*> traverseVariant variant

  where

    traverseVariant variant = case variant of
      Some inner -> Some <$> onExpression r inner
      Ok inner -> Ok <$> onExpression r inner
      Error inner -> Error <$> onExpression r inner
      Failure inner -> Failure <$> onExpression r inner
      None -> pure None
      Success -> pure Success

-- | Rebuilds the assignment of a field, which a struct initializer and the
-- global declaration of an object are made of.
rewriteFieldAssignment :: Applicative f
  => Rewriter f a -> FieldAssignment a -> f (FieldAssignment a)
rewriteFieldAssignment r assignment = case assignment of
  FieldValueAssignment ident expr ann ->
    (\ann' expr' -> FieldValueAssignment ident expr' ann')
      <$> onAnnotation r ann <*> onExpression r expr
  FieldAddressAssignment ident expr ann ->
    (\ann' expr' -> FieldAddressAssignment ident expr' ann')
      <$> onAnnotation r ann <*> onExpression r expr
  FieldPortConnection kind id1 id2 ann ->
    FieldPortConnection kind id1 id2 <$> onAnnotation r ann
