-- | What the left operand of a logical operator establishes about the right
-- one. The right operand of @&&@ runs only when the left one is true, and the
-- right operand of @||@ only when it is false, so a comparison there bounds a
-- variable for the whole right operand. Nothing in the right operand can change
-- the variable in between, since an effect there is rejected by the
-- side-effect check.
--
-- The prover discharges three checks with those bounds: the index of an array
-- access kept inside the array, the amount of a shift kept below the width of
-- the value shifted, and a divisor kept away from zero.
module Elaboration.Prover.Guard (guardProver) where

import Lowering.AST
import Semantic.Types
import Elaboration.Obligations
import Elaboration.Prover
import ControlFlow.Traversal (Child'(..), expressionChildren)
import Configuration.Platform (Platform)
import Core.Utils (arrayOf, intTy, shiftWidth)
import Utils.Annotations

import qualified Data.Map.Strict as M

-- | Discharges the checks that a guard makes unable to fail. The bounds a
-- guard sets hold only within the right operand of its logical operator, so
-- each expression of the scope is walked on its own.
guardProver :: Platform -> Prover
guardProver plt = Prover name proveScope

  where

    name = "guard"

    proveScope scope obligations =
      let asked = M.fromList [(oid, ()) | Obligation oid _ <- obligations]
          guarded = M.fromList (concatMap (inExpression []) (scopeExpressions scope))
      in M.intersection (Evidence name <$> guarded) asked

    inExpression :: Bounds -> Expression SemanticAnn -> [(ObligationId, String)]
    inExpression bounds expr =
      [ (oid, reason)
      | Obligation oid@(_, kind) (BinaryOperation _ left right) <- expressionChecks expr
      , Just reason <- [operationReason bounds kind left right] ]
      ++ case expr of
        BinOp LogicalAnd left right _ ->
          inExpression bounds left ++ inExpression (bounds ++ guardedBounds True left) right
        BinOp LogicalOr left right _ ->
          inExpression bounds left ++ inExpression (bounds ++ guardedBounds False left) right
        _ -> concatMap (inChild bounds) (expressionChildren expr)

    inChild bounds (ChildExpr expr) = inExpression bounds expr
    inChild bounds (ChildArg expr) = inExpression bounds expr
    inChild bounds (ChildConstExpr expr) = inExpression bounds expr
    inChild bounds (ChildObject obj) = inObject bounds obj
    inChild bounds (ChildReference _ obj) = inObject bounds obj

    inObject :: Bounds -> Object SemanticAnn -> [(ObligationId, String)]
    inObject bounds obj = case obj of
      ArrayIndexExpression inner index _ ->
        [ (oid, "a guard keeps the index inside the array")
        | Obligation oid _ <- objectChecks obj
        , Just (_, size) <- [getTypeSemAnn (getAnnotation inner) >>= arrayOf]
        , indexInBounds bounds size index ]
        ++ inObject bounds inner ++ inExpression bounds index
      MemberAccess inner _ _ -> inObject bounds inner
      DereferenceMemberAccess inner _ _ -> inObject bounds inner
      Dereference inner _ -> inObject bounds inner
      Unbox inner _ -> inObject bounds inner
      Variable {} -> []

    operationReason :: Bounds -> CheckKind -> Expression SemanticAnn -> Expression SemanticAnn -> Maybe String
    operationReason bounds ShiftBelowWidth left right =
      case getTypeSemAnn (getAnnotation left) of
        Just ty | intTy ty, amountBelow bounds (shiftWidth plt ty) right ->
          Just "a guard keeps the amount below the width of the value"
        _ -> Nothing
    operationReason bounds NonZeroDivisor _ right
      | nonZero bounds right = Just "a guard keeps the divisor away from zero"
    operationReason _ _ _ _ = Nothing

-- | A bound on a variable, set by a comparison with an expression.
data Bound
  = Below (Expression SemanticAnn)
  | AtMost (Expression SemanticAnn)
  | Above (Expression SemanticAnn)
  | AtLeast (Expression SemanticAnn)
  | NotEqual (Expression SemanticAnn)

type Bounds = [(Identifier, Bound)]

-- | The bounds that hold on the variables of an expression when it evaluates
-- to the given value.
guardedBounds :: Bool -> Expression SemanticAnn -> Bounds
guardedBounds True (BinOp LogicalAnd l r _) = guardedBounds True l ++ guardedBounds True r
guardedBounds False (BinOp LogicalOr l r _) = guardedBounds False l ++ guardedBounds False r
guardedBounds holds (BinOp op l r _) =
    [ (v, bound) | Just v <- [variableOf l], Just bound <- [boundBy held r] ]
    ++ [ (v, bound) | Just v <- [variableOf r], Just bound <- [boundBy (swapped held) l] ]

  where

    held = if holds then Just op else negated op

    -- | The comparison that holds when this one is false.
    negated RelationalLT = Just RelationalGTE
    negated RelationalLTE = Just RelationalGT
    negated RelationalGT = Just RelationalLTE
    negated RelationalGTE = Just RelationalLT
    negated RelationalEqual = Just RelationalNotEqual
    negated RelationalNotEqual = Just RelationalEqual
    negated _ = Nothing

    -- | The comparison read from its right operand: @e < v@ is @v > e@.
    swapped (Just RelationalLT) = Just RelationalGT
    swapped (Just RelationalLTE) = Just RelationalGTE
    swapped (Just RelationalGT) = Just RelationalLT
    swapped (Just RelationalGTE) = Just RelationalLTE
    swapped other = other

    boundBy (Just RelationalLT) e = Just (Below e)
    boundBy (Just RelationalLTE) e = Just (AtMost e)
    boundBy (Just RelationalGT) e = Just (Above e)
    boundBy (Just RelationalGTE) e = Just (AtLeast e)
    boundBy (Just RelationalNotEqual) e = Just (NotEqual e)
    boundBy _ _ = Nothing
guardedBounds _ _ = []

-- | The bounds on the variable an expression is, if it is one.
boundsOf :: Bounds -> Expression SemanticAnn -> [Bound]
boundsOf bounds expr =
    case variableOf expr of
        Just v -> [bound | (v', bound) <- bounds, v' == v]
        Nothing -> []

-- | Whether the index of an access to an array of the given size is a variable
-- that the bounds keep inside the array.
indexInBounds :: Bounds -> Expression SemanticAnn -> Expression SemanticAnn -> Bool
indexInBounds bounds size index = any covers (boundsOf bounds index)

    where

        -- | A bound covers the array when it is not above the size: two
        -- constants are compared, and otherwise a bound below the name that
        -- sizes the array is enough.
        covers :: Bound -> Bool
        covers bound =
            case (bound, literalValue size) of
                (Below e, Just n) -> maybe (sameName e size) (<= n) (literalValue e)
                (AtMost e, Just n) -> maybe False (< n) (literalValue e)
                (Below e, Nothing) -> sameName e size
                _ -> False

        sameName :: Expression SemanticAnn -> Expression SemanticAnn -> Bool
        sameName a b =
            case (variableOf a, variableOf b) of
                (Just x, Just y) -> x == y
                _ -> False

-- | Whether the bounds keep the amount of a shift below the given width.
amountBelow :: Bounds -> Integer -> Expression SemanticAnn -> Bool
amountBelow bounds width amount = any below (boundsOf bounds amount)

    where

        below (Below e) = maybe False (<= width) (literalValue e)
        below (AtMost e) = maybe False (< width) (literalValue e)
        below _ = False

-- | Whether the bounds keep a divisor away from zero.
nonZero :: Bounds -> Expression SemanticAnn -> Bool
nonZero bounds divisor = any excludesZero (boundsOf bounds divisor)

    where

        excludesZero bound =
            case bound of
                NotEqual e -> literalValue e == Just 0
                Above e -> maybe False (>= 0) (literalValue e)
                AtLeast e -> maybe False (>= 1) (literalValue e)
                Below e -> maybe False (<= 0) (literalValue e)
                AtMost e -> maybe False (<= -1) (literalValue e)

variableOf :: Expression SemanticAnn -> Maybe Identifier
variableOf (AccessObject (Variable v _)) = Just v
variableOf _ = Nothing
