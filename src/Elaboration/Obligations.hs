-- | The run-time checks that the operations of a program call for.
--
-- Each operation that the generated code may check gives one obligation per
-- property the check guarantees, and the operation is emitted without the check
-- only when a prover discharges all of them. An obligation is named by the
-- location of the operation and the property, so a prover that works on a
-- whole body, such as a data-flow analysis, can answer by name.
module Elaboration.Obligations (
    CheckKind(..)
  , ObligationId
  , Obligation(..)
  , Operation(..)
  , Scope(..)
  , objectChecks
  , expressionChecks
  , literalValue
  , scopeExpressions
  , scopeObligations
) where

import Lowering.AST
import Semantic.Types
import ControlFlow.Traversal (Child(..), expressionChildren, simpleBlockChildren)
import Core.Utils (intTy, posTy)
import Utils.Annotations

import Data.Maybe (maybeToList)

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

-- | What the provers reason about: a body, or an expression that stands outside
-- any body, such as the initializer of a global.
data Scope
  = BodyScope (Block SemanticAnn)
  | ExpressionScope (Expression SemanticAnn)

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

-- | The value of an integer literal.
literalValue :: Expression SemanticAnn -> Maybe Integer
literalValue (Constant (I (TInteger value _) _) _) = Just value
literalValue _ = Nothing

-- | The expressions of a scope that stand on their own, in the order of the
-- program text. An object that a statement or a block holds outside any
-- expression is given as an access to it.
scopeExpressions :: Scope -> [Expression SemanticAnn]
scopeExpressions (ExpressionScope expr) = [expr]
scopeExpressions (BodyScope blk) = blockExpressions blk

  where

    blockExpressions = concatMap basicBlockExpressions . blockBody

    basicBlockExpressions bb = case bb of
      RegularBlock stmts -> concatMap statementExpressions stmts
      IfElseBlock condIf elseIfs mElse _ ->
        condIfCond condIf : blockExpressions (condIfBody condIf)
        ++ concatMap (\c -> condElseIfCond c : blockExpressions (condElseIfBody c)) elseIfs
        ++ concatMap (blockExpressions . condElseBody) (maybeToList mElse)
      ForLoopBlock _ _ initE endE mBreak body _ ->
        initE : endE : maybeToList mBreak ++ blockExpressions body
      MatchBlock expr cases mDefault _ ->
        expr : concatMap (blockExpressions . matchBody) cases
        ++ concat [blockExpressions b | DefaultCase b _ <- maybeToList mDefault]
      _ -> maybe [] (map childExpression) (simpleBlockChildren bb)

    statementExpressions (Declaration _ _ _ mExpr _) = maybeToList mExpr
    statementExpressions (AssignmentStmt obj expr _) = [AccessObject obj, expr]
    statementExpressions (SingleExpStmt expr _) = [expr]

    childExpression (ChildExpr expr) = expr
    childExpression (ChildArg expr) = expr
    childExpression (ChildConstExpr expr) = expr
    childExpression (ChildObject obj) = AccessObject obj
    childExpression (ChildReference _ obj) = AccessObject obj

-- | Every obligation of a scope, in the order of the program text.
scopeObligations :: Scope -> [Obligation]
scopeObligations = concatMap expressionObligations . scopeExpressions

expressionObligations :: Expression SemanticAnn -> [Obligation]
expressionObligations expr = expressionChecks expr ++ concatMap childObligations (expressionChildren expr)

objectObligations :: Object SemanticAnn -> [Obligation]
objectObligations obj = objectChecks obj ++ case obj of
  ArrayIndexExpression inner index _ -> objectObligations inner ++ expressionObligations index
  MemberAccess inner _ _ -> objectObligations inner
  DereferenceMemberAccess inner _ _ -> objectObligations inner
  Dereference inner _ -> objectObligations inner
  Unbox inner _ -> objectObligations inner
  Variable {} -> []

childObligations :: Child SemanticAnn -> [Obligation]
childObligations (ChildExpr expr) = expressionObligations expr
childObligations (ChildArg expr) = expressionObligations expr
childObligations (ChildConstExpr expr) = expressionObligations expr
childObligations (ChildObject obj) = objectObligations obj
childObligations (ChildReference _ obj) = objectObligations obj
