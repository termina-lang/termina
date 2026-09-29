-- | The shape of the expressions and the objects of an AST, one level at a
-- time. Each AST that the passes walk gives a 'Tree' with the children of its
-- expressions and the step each of its objects takes, and the walks written on
-- top of it serve all of them.
module Core.Tree (
    Child'(..)
  , ObjectNode(..)
  , Tree(..)
  , ObjectVisitor'(..)
  , childExpressionsOf
  , rootIdentOf
  , indexExpressionsOf
  , walkObjectOf
  , fieldAssignmentChildren
  , monadicVariantExprs
) where

import Core.AST

-- | A sub-expression or sub-object of a node, tagged with the role it plays in
-- its parent.
data Child' expr obj a
  = -- | Evaluated as a value.
    ChildExpr (expr a)
    -- | Argument of a call, which is where a box is handed over.
  | ChildArg (expr a)
    -- | Object accessed by value.
  | ChildObject (obj a)
    -- | Object a reference is taken of, either by @&@, by @&mut@ or by slicing.
  | ChildReference AccessKind (obj a)
    -- | Constant expression that belongs to a type or to an address rather than
    -- to the computation, such as the size of an array initializer.
  | ChildConstExpr (expr a)

-- | One step of an access path.
data ObjectNode expr obj a
  = RootNode Identifier a
  | IndexNode (obj a) (expr a)
  | FieldNode (obj a) Identifier
  | DerefFieldNode (obj a) Identifier
  | DerefNode (obj a)
  | UnboxNode (obj a)

-- | How to read the expressions and the objects of an AST one level at a time.
data Tree expr obj a = Tree
  {
    -- | The immediate children of an expression, in evaluation order.
    treeChildren :: expr a -> [Child' expr obj a]
    -- | The step an object takes from the object it is reached through.
  , treeNode :: obj a -> ObjectNode expr obj a
  }

-- | The children of an expression seen as plain expressions, with the objects
-- flattened into the index expressions of their access paths. It is the view a
-- pass that only looks for effects needs.
childExpressionsOf :: Tree expr obj a -> expr a -> [expr a]
childExpressionsOf tree = concatMap flatten . treeChildren tree

  where

    flatten (ChildExpr expr) = [expr]
    flatten (ChildArg expr) = [expr]
    flatten (ChildConstExpr expr) = [expr]
    flatten (ChildObject obj) = indexExpressionsOf tree obj
    flatten (ChildReference _ obj) = indexExpressionsOf tree obj

-- | The variable an access path starts from.
rootIdentOf :: Tree expr obj a -> obj a -> Identifier
rootIdentOf tree obj = case treeNode tree obj of
  RootNode ident _ -> ident
  IndexNode inner _ -> rootIdentOf tree inner
  FieldNode inner _ -> rootIdentOf tree inner
  DerefFieldNode inner _ -> rootIdentOf tree inner
  DerefNode inner -> rootIdentOf tree inner
  UnboxNode inner -> rootIdentOf tree inner

-- | The index expressions embedded in an access path (the @i@ in @arr[i]@),
-- gathered along the whole path.
indexExpressionsOf :: Tree expr obj a -> obj a -> [expr a]
indexExpressionsOf tree obj = case treeNode tree obj of
  RootNode {} -> []
  IndexNode inner index -> index : indexExpressionsOf tree inner
  FieldNode inner _ -> indexExpressionsOf tree inner
  DerefFieldNode inner _ -> indexExpressionsOf tree inner
  DerefNode inner -> indexExpressionsOf tree inner
  UnboxNode inner -> indexExpressionsOf tree inner

-- | What to do at each position of an access path.
data ObjectVisitor' expr obj m a = ObjectVisitor
  {
    -- | The variable the access starts from, with its annotation.
    atRoot :: Identifier -> a -> m ()
    -- | A field, with the object it is reached through, whether it is reached
    -- directly or through a reference.
  , atField :: obj a -> Identifier -> m ()
    -- | An index expression along the path.
  , atIndex :: expr a -> m ()
  }

-- | Walks an access path from the outside in, visiting a field before the object
-- it belongs to and an index after the object it indexes.
walkObjectOf :: Monad m => Tree expr obj a -> ObjectVisitor' expr obj m a -> obj a -> m ()
walkObjectOf tree v = go

  where

    go obj = case treeNode tree obj of
      RootNode ident ann -> atRoot v ident ann
      IndexNode inner index -> go inner >> atIndex v index
      FieldNode inner ident -> atField v inner ident >> go inner
      DerefFieldNode inner ident -> atField v inner ident >> go inner
      DerefNode inner -> go inner
      UnboxNode inner -> go inner

-- | The children of the assignment of a field in an initializer. The address
-- of a field belongs to the layout and not to the computation.
fieldAssignmentChildren :: FieldAssignment' expr a -> [Child' expr obj a]
fieldAssignmentChildren (FieldValueAssignment _ expr _) = [ChildExpr expr]
fieldAssignmentChildren (FieldAddressAssignment _ expr _) = [ChildConstExpr expr]
fieldAssignmentChildren FieldPortConnection {} = []

-- | The expressions a monadic variant holds.
monadicVariantExprs :: MonadicVariant' expr a -> [expr a]
monadicVariantExprs variant = case variant of
  Some expr -> [expr]
  Ok expr -> [expr]
  Error expr -> [expr]
  Failure expr -> [expr]
  None -> []
  Success -> []
