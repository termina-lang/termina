-- | What the left operand of a logical operator establishes about the right
-- one. The right operand of @&&@ runs only when the left one is true, and the
-- right operand of @||@ only when it is false, so a comparison there puts an
-- upper bound on a variable for the whole right operand. Nothing in the right
-- operand can change the variable in between, since an effect there is
-- rejected by the side-effect check.
--
-- An array access whose index is so bounded inside the array cannot fail, and
-- the prover discharges its bounds check.
module Elaboration.Prover.Guard (guardProver) where

import Lowering.AST
import Semantic.Types
import Elaboration.Obligations
import Elaboration.Prover
import ControlFlow.Traversal (Child'(..), expressionChildren)
import Core.Utils (arrayOf)
import Utils.Annotations

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Discharges the bounds check of the array accesses that a guard keeps inside
-- the array. The bounds a guard sets hold only within the right operand of its
-- logical operator, so each expression of the scope is walked on its own.
guardProver :: Prover
guardProver = Prover name proveScope

  where

    name = "guard"

    proveScope scope obligations =
      let asked = S.fromList [oid | Obligation oid@(_, IndexInBounds) _ <- obligations]
          guarded = S.fromList (concatMap (inExpression []) (scopeExpressions scope))
      in M.fromSet (const (Evidence name "a guard keeps the index inside the array"))
           (S.intersection asked guarded)

    inExpression :: IndexBounds -> Expression SemanticAnn -> [ObligationId]
    inExpression bounds (BinOp LogicalAnd left right _) =
      inExpression bounds left ++ inExpression (bounds ++ guardedBounds True left) right
    inExpression bounds (BinOp LogicalOr left right _) =
      inExpression bounds left ++ inExpression (bounds ++ guardedBounds False left) right
    inExpression bounds expr = concatMap (inChild bounds) (expressionChildren expr)

    inChild bounds (ChildExpr expr) = inExpression bounds expr
    inChild bounds (ChildArg expr) = inExpression bounds expr
    inChild bounds (ChildConstExpr expr) = inExpression bounds expr
    inChild bounds (ChildObject obj) = inObject bounds obj
    inChild bounds (ChildReference _ obj) = inObject bounds obj

    inObject :: IndexBounds -> Object SemanticAnn -> [ObligationId]
    inObject bounds obj = case obj of
      ArrayIndexExpression inner index _ ->
        [ oid
        | Obligation oid _ <- objectChecks obj
        , Just (_, size) <- [getTypeSemAnn (getAnnotation inner) >>= arrayOf]
        , indexInBounds bounds size index ]
        ++ inObject bounds inner ++ inExpression bounds index
      MemberAccess inner _ _ -> inObject bounds inner
      DereferenceMemberAccess inner _ _ -> inObject bounds inner
      Dereference inner _ -> inObject bounds inner
      Unbox inner _ -> inObject bounds inner
      Variable {} -> []

-- | An upper bound on a variable: strictly below an expression, or at most it.
data IndexBound = Below (Expression SemanticAnn) | AtMost (Expression SemanticAnn)

type IndexBounds = [(Identifier, IndexBound)]

-- | The bounds that hold on the variables of an expression when it evaluates
-- to the given value.
guardedBounds :: Bool -> Expression SemanticAnn -> IndexBounds
guardedBounds True (BinOp LogicalAnd l r _) = guardedBounds True l ++ guardedBounds True r
guardedBounds False (BinOp LogicalOr l r _) = guardedBounds False l ++ guardedBounds False r
guardedBounds holds (BinOp op l r _) =
    case (holds, op, variableOf l, variableOf r) of
        -- | v < e, and e > v
        (True, RelationalLT, Just v, _) -> [(v, Below r)]
        (True, RelationalGT, _, Just v) -> [(v, Below l)]
        -- | v <= e, and e >= v
        (True, RelationalLTE, Just v, _) -> [(v, AtMost r)]
        (True, RelationalGTE, _, Just v) -> [(v, AtMost l)]
        -- | v >= e false, and e <= v false, leave v < e
        (False, RelationalGTE, Just v, _) -> [(v, Below r)]
        (False, RelationalLTE, _, Just v) -> [(v, Below l)]
        -- | v > e false, and e < v false, leave v <= e
        (False, RelationalGT, Just v, _) -> [(v, AtMost r)]
        (False, RelationalLT, _, Just v) -> [(v, AtMost l)]
        _ -> []
guardedBounds _ _ = []

-- | Whether the index of an access to an array of the given size is a variable
-- that the bounds keep inside the array.
indexInBounds :: IndexBounds -> Expression SemanticAnn -> Expression SemanticAnn -> Bool
indexInBounds bounds size index =
    case variableOf index of
        Just v -> any (covers . snd) (filter ((== v) . fst) bounds)
        Nothing -> False

    where

        -- | A bound covers the array when it is not above the size: two
        -- constants are compared, and otherwise a bound below the name that
        -- sizes the array is enough.
        covers :: IndexBound -> Bool
        covers bound =
            case (bound, literalValue size) of
                (Below e, Just n) -> maybe (sameName e size) (<= n) (literalValue e)
                (AtMost e, Just n) -> maybe False (< n) (literalValue e)
                (Below e, Nothing) -> sameName e size
                (AtMost _, Nothing) -> False

        sameName :: Expression SemanticAnn -> Expression SemanticAnn -> Bool
        sameName a b =
            case (variableOf a, variableOf b) of
                (Just x, Just y) -> x == y
                _ -> False

variableOf :: Expression SemanticAnn -> Maybe Identifier
variableOf (AccessObject (Variable v _)) = Just v
variableOf _ = Nothing
