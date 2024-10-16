-- | Module With Errors

module ControlFlow.VarUsage.Errors.Errors where

import Core.AST (Identifier)

import Utils.Annotations

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
  | EUsedIgnoredParameter Identifier -- ^ Using a variable that is ignored (VE-001)
  | ENotUsed Identifier -- ^ Variable is not used (VE-002)
  | EBoxNotUsed Identifier -- ^ Box variable is not used (VE-003)
  | EBoxMovedTwice Identifier Location -- ^ Box variable is moved twice (VE-004)
  | EOptionBoxMovedTwice Identifier Location -- ^ Option-box variable is moved twice (VE-005)
  | EDifferentOptionBoxUse Identifier Location -- ^ Option-box final state mismatch (VE-007)
  | EDifferentBoxSets
  | ForMoreOOpt
  | ForMoreOBox [Identifier]
  -- Special Variable errors
  | AllocNotUsed Identifier
  | AllocTwice Identifier Location
  | DefinedNotAlloc Identifier Location
  | DefinedTwice Identifier Location
  deriving Show

type VarUsageError = AnnotatedError Error Location
