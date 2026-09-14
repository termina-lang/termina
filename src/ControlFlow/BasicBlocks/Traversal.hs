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
  , expressionChildren
  , simpleBlockChildren
  , childExpressions
  , walkObject
  , rootIdent
  , indexExpressions
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
