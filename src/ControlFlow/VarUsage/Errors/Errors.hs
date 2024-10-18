-- | Module With Errors

module ControlFlow.VarUsage.Errors.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import ControlFlow.VarUsage.Types

data Error
  = ESetMaxBound -- ^ The set has reached its maximum bound (Internal)  
  | EMapMaxBound -- ^ The map has reached its maximum bound (Internal)
  | EUnboxingObjectType -- ^ Error when trying to unbox an object type (Internal)
  | EUnboxingExpressionType -- ^ Error when trying to unbox an expression type (Internal)
  | EDefiningBox -- ^ Error when trying to declare variable of box type (Internal)
  | EBadAllocArg -- ^ Bad argument for alloc (Internal)
  | EBadFreeArg -- ^ Bad argument for free (Internal)
  | EBadSendArg -- ^ Bad argument for send (Internal)
  | EVarRedefinition -- ^ Variable redefinition (Internal)
  | EMalformedOptionBoxMatch -- ^ Malformed option-box match (Internal)
  | EMissingOptionBox -- ^ An option-box is missing from a branch (Internal)
  | EBadOptionBoxAssignExpression -- ^ Bad expression for option-box assignment (Internal)
  | EUsedIgnoredParameter Identifier -- ^ Using a variable that is ignored (VE-001)
  | ENotUsed Identifier -- ^ Variable is not used (VE-002)
  | EBoxNotMoved Identifier -- ^ Box variable is not moved (VE-003)
  | EBoxMovedTwice Identifier Location -- ^ Box variable is moved twice (VE-004)
  | EOptionBoxMovedTwice Identifier Location -- ^ Option-box variable is moved twice (VE-005)
  | EDifferentOptionBoxUse Identifier MVars (MVars, Location) -- ^ Option-box final state mismatch (VE-006)
  | EMissingBoxMove Identifier Location -- ^ Box variable is not always moved (VE-007)
  | EDifferentOptionBoxUseSingleBranch Identifier -- ^ Option-box final state mismatch in single branch (VE-008)
  | EBoxMoveConditionalBranch Identifier -- ^ Box variable moved in conditional branch (VE-009)
  | EAllocNotMoved Identifier -- ^ Option-box allocated but not moved(VE-010)
  | EAllocTwice Identifier Location -- ^ Option-box allocated twice (VE-011)
  | EMovedWithoutAlloc Identifier Location -- ^ Option-box moved but not allocated (VE-012)
  | EDefinedTwice Identifier Location -- ^ Variable defined twice (VE-013)
  deriving Show

type VarUsageError = AnnotatedError Error Location
