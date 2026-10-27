-- | The run-time checks that the operations of a program call for.
--
-- Each operation that the generated code may check gives one obligation per
-- property the check guarantees, and the operation is emitted without the check
-- only when a pass shows all of them to hold. An obligation is named by the
-- location of the operation and the property, so a pass that walks a whole
-- body, such as a data-flow analysis, gives its proofs by name.
module Elaboration.Obligations (
    CheckKind(..)
  , ObligationId
  , Obligation(..)
  , Operation(..)
  , Evidence(..)
  , Proofs
  , objectChecks
  , expressionChecks
  , atomicIndexChecks
) where

import Lowering.AST
import Semantic.Types
import Core.Utils (intTy, posTy)
import Utils.Annotations

import qualified Data.Map.Strict as M

-- | The property that a check guarantees.
data CheckKind
  = -- | The index of an array access falls inside the array.
    IndexInBounds
    -- | The bounds of a slice fall inside the array and span the size of the
    -- slice.
  | SliceInBounds
    -- | The amount of a shift is below the width of the value shifted.
  | ShiftBelowWidth
    -- | The result of a signed operation is representable in its type.
  | NoOverflow
    -- | The divisor of a division or a remainder is not zero.
  | NonZeroDivisor
  deriving (Show, Eq, Ord)

type ObligationId = (Location, CheckKind)

-- | The operation an obligation comes from.
data Operation
  = IndexOperation (Object SemanticAnn) (Expression SemanticAnn)
  | SliceOperation (Object SemanticAnn) (Expression SemanticAnn) (Expression SemanticAnn)
  | BinaryOperation Op (Expression SemanticAnn) (Expression SemanticAnn)

data Obligation = Obligation
  {
    obligationId :: ObligationId
  , obligationOperation :: Operation
  }

-- | Why an obligation holds: the pass that showed it and the reason.
data Evidence = Evidence
  {
    evidenceProver :: String
  , evidenceReason :: String
  } deriving Show

-- | The obligations that a pass shows to hold.
type Proofs = M.Map ObligationId Evidence

-- | The checks of an access path node, not counting the ones of the objects and
-- the expressions below it.
objectChecks :: Object SemanticAnn -> [Obligation]
objectChecks (ArrayIndexExpression obj index ann) =
  [Obligation (getLocation ann, IndexInBounds) (IndexOperation obj index)]
objectChecks _ = []

-- | The checks of an expression node, not counting the ones of the objects and
-- the expressions below it.
expressionChecks :: Expression SemanticAnn -> [Obligation]
expressionChecks (ArraySliceExpression _ obj lower upper ann) =
  [Obligation (getLocation ann, SliceInBounds) (SliceOperation obj lower upper)]
expressionChecks (BinOp op left right ann) =
  [ Obligation (getLocation ann, kind) (BinaryOperation op left right)
  | kind <- binOpChecks op (getTypeSemAnn (getAnnotation left)) ]
expressionChecks _ = []

-- | The check of an atomic access to an element of an array through a port,
-- whose index falls inside the array as that of any other array access. It is
-- named by the position of the access, since the index may be an array access
-- with an obligation of its own.
atomicIndexChecks :: BasicBlock SemanticAnn -> [Obligation]
atomicIndexChecks bb = case bb of
  AtomicArrayLoad obj index _ ann -> [indexIn obj index ann]
  AtomicArrayStore obj index _ ann -> [indexIn obj index ann]
  _ -> []

  where

    indexIn obj index ann =
      Obligation (getLocation ann, IndexInBounds) (IndexOperation obj index)

-- | The checks of a binary operation, which is carried out in the type of its
-- left operand.
binOpChecks :: Op -> Maybe (TerminaType SemanticAnn) -> [CheckKind]
binOpChecks op leftTy =
  case op of
    BitwiseLeftShift -> [ShiftBelowWidth]
    BitwiseRightShift -> [ShiftBelowWidth]
    Addition | signed -> [NoOverflow]
    Subtraction | signed -> [NoOverflow]
    Multiplication | signed -> [NoOverflow]
    Division | signed -> [NonZeroDivisor, NoOverflow]
    Modulo | signed -> [NonZeroDivisor, NoOverflow]
    Division | unsigned -> [NonZeroDivisor]
    Modulo | unsigned -> [NonZeroDivisor]
    _ -> []

  where

    operandTy = valueType <$> leftTy

    signed = maybe False (\ty -> intTy ty && not (posTy ty)) operandTy
    unsigned = maybe False posTy operandTy

    -- | The type of the value that an object at a fixed location or in a box
    -- holds.
    valueType (TFixedLocation ty) = valueType ty
    valueType (TBoxSubtype ty) = valueType ty
    valueType ty = ty
