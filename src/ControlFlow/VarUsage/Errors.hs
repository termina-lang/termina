{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Errors of the variable usage check, the pass that walks the basic blocks
-- forwards. They are about the objects a body declares: whether every path
-- assigns one before reading it, whether anybody reads it at all, and whether
-- the value of each assignment is read before it is overwritten.

module ControlFlow.VarUsage.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import Utils.Errors
import qualified Data.Text as T

data Error
  = EUsedIgnoredParameter Identifier -- ^ Using a variable that is ignored (VUE-001)
  | ENotUsed Identifier -- ^ Variable is not used (VUE-002)
  | EActionSelfNotUsed Identifier -- ^ Action does not use self (VUE-003)
  | ESelfNotUsed Identifier -- ^ Method, viewer or procedure does not use self (VUE-004)
  | EMemberFunctionNotUsed Identifier -- ^ Method or viewer is never called (VUE-005)
  | EAssignedValueNotUsed Identifier -- ^ Value assigned to a variable is never read (VUE-006)
  | EReadBeforeAssignment Identifier -- ^ Object read on a path where it has not been assigned (VUE-007)
  | EPartialWriteBeforeAssignment Identifier -- ^ Field or element written before the object is assigned as a whole (VUE-008)
  | EInitializerNotUsed Identifier -- ^ Value an initializer gives an object is never read (VUE-009)
  deriving Show

type VarUsageError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EUsedIgnoredParameter ident) =
        unnecessary $ diagnostic "VUE-001" "using an ignored parameter"
            ("Parameter " <> emph (T.pack ident) <>
                " is ignored and should not be used.")
    describe (ENotUsed ident) =
        unnecessary $ diagnostic "VUE-002" "variable not used"
            ("Variable " <> emph (T.pack ident) <>
                " is declared but not used.")
    describe (EActionSelfNotUsed ident) =
        diagnostic "VUE-003" "action does not use self"
            ("Action " <> emph (T.pack ident) <>
                " does not use " <> emph "self" <>
                ", so it cannot have any effect other than its result.")
    describe (ESelfNotUsed ident) =
        diagnostic "VUE-004" "self not used"
            ("Member function " <> emph (T.pack ident) <>
                " does not use " <> emph "self" <>
                ". It must be reimplemented as a function.")
    describe (EMemberFunctionNotUsed ident) =
        unnecessary $ diagnostic "VUE-005" "member function not used"
            ("Member function " <> emph (T.pack ident) <>
                " is not called by any member of the class.")
    describe (EAssignedValueNotUsed ident) =
        unnecessary $ diagnostic "VUE-006" "assigned value never read"
            ("The value assigned to variable " <> emph (T.pack ident) <>
                " is never read.")
    describe (EReadBeforeAssignment ident) =
        diagnostic "VUE-007" "object read before it is assigned"
            ("Variable " <> emph (T.pack ident) <>
                " is declared without an initializer and there is a path that reaches this point without assigning it.\n" <>
                "Assign the whole object on every path before reading it.")
    describe (EPartialWriteBeforeAssignment ident) =
        diagnostic "VUE-008" "partial write to an object that is not assigned yet"
            ("Variable " <> emph (T.pack ident) <>
                " is declared without an initializer and this writes only a part of it.\n" <>
                "The whole object must be assigned before a field or an element of it is written.")
    describe (EInitializerNotUsed ident) =
        unnecessary $ diagnostic "VUE-009" "initializer never read"
            ("The value this initializer gives " <> emph (T.pack ident) <>
                " is overwritten before anybody reads it.\n" <>
                "Move the declaration to where the value is computed, or declare the object without an initializer.")

instance ErrorMessage VarUsageError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
