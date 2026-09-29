-- | The prover of the operations whose operands are constants. The constant
-- folding evaluates those operations and rejects the ones that fail.
module Elaboration.Prover.Constant (constantProver) where

import Lowering.AST
import Semantic.Types
import Utils.Annotations
import Elaboration.Obligations
import Elaboration.Prover

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

constantProver :: Prover
constantProver = Prover name (const (M.fromList . mapMaybe proveOne))

  where

    name = "constant"

    proveOne (Obligation oid@(_, kind) operation) =
      (,) oid . Evidence name <$> reason kind operation

    reason :: CheckKind -> Operation -> Maybe String
    reason IndexInBounds (IndexOperation _ index)
      | isConstType index = Just "the index is a constant"
    reason SliceInBounds (SliceOperation _ lower upper)
      | isConstType lower && isConstType upper = Just "the bounds are constants"
    reason ShiftBelowWidth (BinaryOperation _ _ right)
      | isConstType right = Just "the amount is a constant"
    reason NonZeroDivisor (BinaryOperation _ _ right)
      | isLiteral right = Just "the divisor is a constant"
    reason NoOverflow (BinaryOperation op left right)
      | isLiteral left && isLiteral right = Just "the operands are constants"
      | op `elem` [Division, Modulo] && maybe False (/= -1) (literalValue right) =
          Just "the divisor is a constant other than -1"
    reason _ _ = Nothing

-- | Whether an expression is a constant, a literal or a named one.
isConstType :: Expression SemanticAnn -> Bool
isConstType expr =
  case getTypeSemAnn (getAnnotation expr) of
    Just (TConstSubtype _) -> True
    _ -> False

isLiteral :: Expression SemanticAnn -> Bool
isLiteral (Constant {}) = True
isLiteral _ = False
