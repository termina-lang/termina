{-# LANGUAGE LambdaCase #-}
-- | Structural recursion over the basic-block AST.
--
-- Every pass over this AST walks the same tree and differs only in what it does
-- at a handful of positions, so the shape of the walk is written here once and
-- each pass says what those positions mean to it. The children of an expression
-- come back tagged with the role they play, because the passes do not agree on
-- all of them: a call argument moves a box in the linearity check and is a plain
-- read everywhere else, and the constant expressions that come from a type or an
-- address are part of the program text but not of the computation.
--
-- What a pass may not delegate here is the control flow. 'simpleBlockChildren'
-- covers the blocks that only evaluate expressions and returns 'Nothing' for the
-- four that branch, which every pass has to interpret for itself.
module ControlFlow.BasicBlocks.Traversal (
    Child(..)
  , FieldAccessor(..)
  , ObjectVisitor(..)
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

import ControlFlow.BasicBlocks.AST

import Data.Maybe (maybeToList)

-- | A sub-expression or sub-object of a node, tagged with the role it plays in
-- its parent.
data Child a
  = -- | Evaluated as a value.
    ChildExpr (Expression a)
    -- | Argument of a call, which is where a box is handed over.
  | ChildArg (Expression a)
    -- | Object accessed by value.
  | ChildObject (Object a)
    -- | Object a reference is taken of, either by @&@, by @&mut@ or by slicing.
  | ChildReference AccessKind (Object a)
    -- | Constant expression that belongs to a type or to an address rather than
    -- to the computation, such as the size of an array initializer.
  | ChildConstExpr (Expression a)

-- | How a field is reached, which the linearity check distinguishes and the
-- usage check deliberately does not.
data FieldAccessor = Direct | ThroughReference
  deriving (Eq, Show)

-- | What to do at each position of an access path.
data ObjectVisitor m a = ObjectVisitor
  {
    -- | The variable the access starts from, with its annotation.
    atRoot :: Identifier -> a -> m ()
    -- | A field, with the object it is reached through.
  , atField :: FieldAccessor -> Object a -> Identifier -> m ()
    -- | An index expression along the path.
  , atIndex :: Expression a -> m ()
  }

-- | Walks an access path from the outside in, visiting a field before the object
-- it belongs to and an index after the object it indexes.
walkObject :: Monad m => ObjectVisitor m a -> Object a -> m ()
walkObject v = go

  where

    go (Variable ident ann) = atRoot v ident ann
    go (ArrayIndexExpression obj index _) = go obj >> atIndex v index
    go (MemberAccess obj ident _) = atField v Direct obj ident >> go obj
    go (Dereference obj _) = go obj
    go (DereferenceMemberAccess obj ident _) = atField v ThroughReference obj ident >> go obj
    go (Unbox obj _) = go obj

-- | The variable an access path starts from.
rootIdent :: Object a -> Identifier
rootIdent (Variable ident _) = ident
rootIdent (ArrayIndexExpression obj _ _) = rootIdent obj
rootIdent (MemberAccess obj _ _) = rootIdent obj
rootIdent (Dereference obj _) = rootIdent obj
rootIdent (DereferenceMemberAccess obj _ _) = rootIdent obj
rootIdent (Unbox obj _) = rootIdent obj

-- | The index expressions embedded in an access path (the @i@ in @arr[i]@),
-- gathered along the whole path.
indexExpressions :: Object a -> [Expression a]
indexExpressions obj = case obj of
  ArrayIndexExpression o index _ -> index : indexExpressions o
  MemberAccess o _ _ -> indexExpressions o
  DereferenceMemberAccess o _ _ -> indexExpressions o
  Dereference o _ -> indexExpressions o
  Unbox o _ -> indexExpressions o
  Variable {} -> []

-- | The immediate children of an expression, in evaluation order.
expressionChildren :: Expression a -> [Child a]
expressionChildren e = case e of
  AccessObject obj -> [ChildObject obj]
  Constant {} -> []
  BinOp _ left right _ -> [ChildExpr left, ChildExpr right]
  ReferenceExpression ak obj _ -> [ChildReference ak obj]
  Casting inner _ _ -> [ChildExpr inner]
  IsEnumVariantExpression obj _ _ _ -> [ChildObject obj]
  IsMonadicVariantExpression obj _ _ -> [ChildObject obj]
  ArraySliceExpression ak obj lower upper _ ->
    [ChildReference ak obj, ChildExpr lower, ChildExpr upper]
  MemberFunctionCall obj _ args _ -> ChildObject obj : map ChildArg args
  DerefMemberFunctionCall obj _ args _ -> ChildObject obj : map ChildArg args
  FunctionCall _ args _ -> map ChildArg args
  ArrayInitializer inner size _ -> [ChildExpr inner, ChildConstExpr size]
  ArrayExprListInitializer exprs _ -> map ChildExpr exprs
  StructInitializer fields _ -> concatMap fieldChildren fields
  EnumVariantInitializer _ _ args _ -> map ChildExpr args
  MonadicVariantInitializer variant _ -> map ChildExpr (variantExprs variant)
  StringInitializer {} -> []

  where

    fieldChildren :: FieldAssignment a -> [Child a]
    fieldChildren (FieldValueAssignment _ expr _) = [ChildExpr expr]
    fieldChildren (FieldAddressAssignment _ expr _) = [ChildConstExpr expr]
    fieldChildren FieldPortConnection {} = []

    variantExprs :: MonadicVariant' Expression a -> [Expression a]
    variantExprs = \case
      Some expr -> [expr]
      Ok expr -> [expr]
      Error expr -> [expr]
      Failure expr -> [expr]
      None -> []
      Success -> []

-- | The children of a basic block that only evaluates expressions, in
-- evaluation order. The blocks that branch return 'Nothing', since what they
-- mean to a pass is not the list of expressions they contain.
simpleBlockChildren :: BasicBlock a -> Maybe [Child a]
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

-- | The children of an expression seen as plain expressions, with the objects
-- flattened into the index expressions of their access paths. It is the view a
-- pass that only looks for effects needs.
childExpressions :: Expression a -> [Expression a]
childExpressions = concatMap flatten . expressionChildren

  where

    flatten (ChildExpr expr) = [expr]
    flatten (ChildArg expr) = [expr]
    flatten (ChildConstExpr expr) = [expr]
    flatten (ChildObject obj) = indexExpressions obj
    flatten (ChildReference _ obj) = indexExpressions obj
