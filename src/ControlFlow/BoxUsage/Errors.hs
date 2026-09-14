{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Errors of the box linearity check: whether a box is moved exactly once and
-- whether every branch leaves the option-boxes in the same state.

module ControlFlow.BoxUsage.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import ControlFlow.BoxUsage.Types
import Utils.Errors
import qualified Data.Text as T
import Utils.Printer

data Error
  = ESetMaxBound -- ^ The set has reached its maximum bound (Internal)
  | EMapMaxBound -- ^ The map has reached its maximum bound (Internal)
  | EExpectedOptionBoxType -- ^ Expected option-box type (Internal)
  | EInvalidObjectTypeAnnotation -- ^ Error when the semantic annotation of an object does not contain the expected type information (Internal)
  | EInvalidExprTypeAnnotation -- ^ Error when the semantic annotation of an expression does not contain the expected type information (Internal)
  | EDefiningBox -- ^ Error when trying to declare variable of box type (Internal)
  | EBadAllocArg -- ^ Bad argument for alloc (Internal)
  | EBadFreeArg -- ^ Bad argument for free (Internal)
  | EBadSendArg -- ^ Bad argument for send (Internal)
  | EVarRedefinition -- ^ Variable redefinition (Internal)
  | EMalformedOptionBoxMatch -- ^ Malformed option-box match (Internal)
  | EBadOptionBoxAssignExpression -- ^ Bad expression for option-box assignment (Internal)
  | EUnboxingOptionMap -- ^ Error when trying to unbox an option map (Internal)
  | EUnboxingVariableMap -- ^ Error when trying to unbox a variable map (Internal)
  | EDefinedTwice -- ^ Variable defined twice (Internal)
  | EOptionBoxUsedInBadContext -- ^ Option-box used in bad context (Internal)
  | EBoxNotMoved Identifier -- ^ Box variable is not moved (BE-001)
  | EBoxMovedTwice Identifier Location -- ^ Box variable is moved twice (BE-002)
  | EOptionBoxMovedTwice Identifier Location -- ^ Option-box variable is moved twice (BE-003)
  | EDifferentOptionBoxUse Identifier MVars (MVars, Location) -- ^ Option-box final state mismatch (BE-004)
  | EDifferentNewOptionBoxUse Identifier MVars -- ^ Option-box used in conditional branch (BE-005)
  | EMissingOptionBox Identifier MVars -- ^ Option-box unused in a branch but used previously (BE-006)
  | EMissingBoxMove Identifier Location -- ^ Box variable is not always moved (BE-007)
  | EBoxMoveConditionalBranch Identifier -- ^ Box variable moved in conditional branch (BE-008)
  | EAllocNotMoved Identifier -- ^ Option-box allocated but not moved (BE-009)
  | EAllocTwice Identifier Location -- ^ Option-box allocated twice (BE-010)
  | EMovedWithoutAlloc Identifier Location -- ^ Option-box moved but not allocated (BE-011)
  | EOptionBoxMatchMissingSomeCase -- ^ Option-box match missing the Some case (BE-012)
  deriving Show

type BoxUsageError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EBoxNotMoved ident) =
        diagnostic "BE-001" "box variable is not moved"
            ("Box variable " <> emph (T.pack ident) <> " is declared but not moved.")
    describe (EBoxMovedTwice ident prevMove) =
        relatedTo prevMove "the previous move" $
            diagnostic "BE-002" "box variable is moved twice"
                ("Box variable " <> emph (T.pack ident) <> " is moved twice.")
    describe (EOptionBoxMovedTwice ident prevMove) =
        relatedTo prevMove "the previous move" $
            diagnostic "BE-003" "option-box variable is moved twice"
                ("Option-box variable " <> emph (T.pack ident) <> " is moved twice.")
    describe (EDifferentOptionBoxUse ident rval (lval, otherPos)) =
        relatedTo otherPos ("here it has been " <> showText lval) $
            diagnostic "BE-004" "option-box final state mismatch"
                ("On this branch, the variable " <> emph (T.pack ident) <>
                    " has been " <> emph (showText rval) <> ".\n" <>
                    "The final state of the option-box variables in all branches must be the same.")
    describe (EDifferentNewOptionBoxUse ident rval) =
        diagnostic "BE-005" "option-box used in a conditional branch"
            ("Option-box variable " <> emph (T.pack ident) <>
                " has been " <> emph (showText rval) <>
                " in a branch that may not be executed or inside a loop.\n" <>
                "This shall cause the final state to be inconsistent.")
    describe (EMissingOptionBox ident prevVal) =
        relatedTo (getLocation prevVal) ("here it was " <> showText prevVal) $
            diagnostic "BE-006" "option-box unused in a branch but used previously"
                ("Option-box variable " <> emph (T.pack ident) <>
                    " is not used on this branch.\n" <>
                    "The final state of the option-box variables must be the same so that the resulting state is consistent.")
    describe (EMissingBoxMove ident otherMove) =
        relatedTo otherMove "moved on this branch" $
            diagnostic "BE-007" "box variable is not always moved"
                ("Box variable " <> emph (T.pack ident) <> " is not moved on this branch.\n" <>
                    "The same box variables must be moved in all branches so that the resulting state is consistent.")
    describe (EBoxMoveConditionalBranch ident) =
        diagnostic "BE-008" "box variable moved in a conditional branch"
            ("Box variable " <> emph (T.pack ident) <>
                " is moved in a branch that may not be executed.")
    describe (EAllocNotMoved ident) =
        diagnostic "BE-009" "option-box allocated but not moved"
            ("Option-box variable " <> emph (T.pack ident) <> " is allocated but not moved.")
    describe (EAllocTwice ident prevAlloc) =
        relatedTo prevAlloc "the previous allocation" $
            diagnostic "BE-010" "option-box allocated twice"
                ("Option-box variable " <> emph (T.pack ident) <> " is allocated twice.")
    describe (EMovedWithoutAlloc ident prevMove) =
        relatedTo prevMove "moved here" $
            diagnostic "BE-011" "option-box moved but not allocated"
                ("Option-box variable " <> emph (T.pack ident) <> " is moved but not allocated.")
    describe EOptionBoxMatchMissingSomeCase =
        diagnostic "BE-012" "option-box match missing the Some case"
            "Option-box match is missing Some case."
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own and is printed as it is shown.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage BoxUsageError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
