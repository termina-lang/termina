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
  | EBadOptionBoxAssignExpression -- ^ Bad expression for option-box assignment (Internal)
  | EUnboxingOptionMap -- ^ Error when trying to unbox an option map (Internal)
  | EUnboxingVariableMap -- ^ Error when trying to unbox a variable map (Internal)
  | EDefinedTwice -- ^ Variable defined twice (Internal)
  | EOptionBoxUsedInBadContext -- ^ TOption-box used in bad context (Internal)
  | EUsedIgnoredParameter Identifier -- ^ Using a variable that is ignored (VE-001)
  | ENotUsed Identifier -- ^ Variable is not used (VE-002)
  | EBoxNotMoved Identifier -- ^ Box variable is not moved (VE-003)
  | EBoxMovedTwice Identifier Location -- ^ Box variable is moved twice (VE-004)
  | EOptionBoxMovedTwice Identifier Location -- ^ TOption-box variable is moved twice (VE-005)
  | EDifferentOptionBoxUse Identifier MVars (MVars, Location) -- ^ TOption-box final state mismatch (VE-006)
  | EDifferentNewOptionBoxUse Identifier MVars -- ^ TOption-box used in conditional branch (VE-007)
  | EMissingOptionBox Identifier MVars -- ^ TOption-box unused in a branch but used previously (VE-008)
  | EMissingBoxMove Identifier Location -- ^ Box variable is not always moved (VE-009)
  | EBoxMoveConditionalBranch Identifier -- ^ Box variable moved in conditional branch (VE-010)
  | EAllocNotMoved Identifier -- ^ TOption-box allocated but not moved (VE-011)
  | EAllocTwice Identifier Location -- ^ TOption-box allocated twice (VE-012)
  | EMovedWithoutAlloc Identifier Location -- ^ TOption-box moved but not allocated (VE-013)
  deriving Show

type VarUsageError = AnnotatedError Error Location
