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
import Text.Parsec
import qualified Data.Map.Strict as M
import qualified Language.LSP.Protocol.Types as LSP

data Error
  = EUsedIgnoredParameter Identifier -- ^ Using a variable that is ignored (VE-001)
  | ENotUsed Identifier -- ^ Variable is not used (VE-002)
  | EActionSelfNotUsed Identifier -- ^ Action does not use self (VE-003)
  | ESelfNotUsed Identifier -- ^ Method, viewer or procedure does not use self (VE-004)
  | EMemberFunctionNotUsed Identifier -- ^ Method or viewer is never called (VE-005)
  | EAssignedValueNotUsed Identifier -- ^ Value assigned to a variable is never read (VE-006)
  | EReadBeforeAssignment Identifier -- ^ Object read on a path where it has not been assigned (VE-007)
  | EPartialWriteBeforeAssignment Identifier -- ^ Field or element written before the object is assigned as a whole (VE-008)
  | EInitializerNotUsed Identifier -- ^ Value an initializer gives an object is never read (VE-009)
  deriving Show

type VarUsageError = AnnotatedError Error Location

instance ErrorMessage VarUsageError where

    errorIdent (AnnotatedError (EUsedIgnoredParameter _ident) _pos) = "VE-001"
    errorIdent (AnnotatedError (ENotUsed _ident) _pos) = "VE-002"
    errorIdent (AnnotatedError (EActionSelfNotUsed _ident) _pos) = "VE-003"
    errorIdent (AnnotatedError (ESelfNotUsed _ident) _pos) = "VE-004"
    errorIdent (AnnotatedError (EMemberFunctionNotUsed _ident) _pos) = "VE-005"
    errorIdent (AnnotatedError (EAssignedValueNotUsed _ident) _pos) = "VE-006"
    errorIdent (AnnotatedError (EReadBeforeAssignment _ident) _pos) = "VE-007"
    errorIdent (AnnotatedError (EPartialWriteBeforeAssignment _ident) _pos) = "VE-008"
    errorIdent (AnnotatedError (EInitializerNotUsed _ident) _pos) = "VE-009"

    errorTitle (AnnotatedError (EUsedIgnoredParameter _ident) _pos) = "using an ignored parameter"
    errorTitle (AnnotatedError (ENotUsed _ident) _pos) = "variable not used"
    errorTitle (AnnotatedError (EActionSelfNotUsed _ident) _pos) = "action does not use self"
    errorTitle (AnnotatedError (ESelfNotUsed _ident) _pos) = "self not used"
    errorTitle (AnnotatedError (EMemberFunctionNotUsed _ident) _pos) = "member function not used"
    errorTitle (AnnotatedError (EAssignedValueNotUsed _ident) _pos) = "assigned value never read"
    errorTitle (AnnotatedError (EReadBeforeAssignment _ident) _pos) = "object read before it is assigned"
    errorTitle (AnnotatedError (EPartialWriteBeforeAssignment _ident) _pos) = "partial write to an object that is not assigned yet"
    errorTitle (AnnotatedError (EInitializerNotUsed _ident) _pos) = "initializer never read"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
        case err of
            EUsedIgnoredParameter ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Parameter \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is ignored and should not be used."))
            ENotUsed ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is declared but not used."))
            EActionSelfNotUsed ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Action \x1b[31m" <> T.pack ident <>
                        "\x1b[0m does not use \x1b[31mself\x1b[0m, so it cannot have any effect other than its result."))
            ESelfNotUsed ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Member function \x1b[31m" <> T.pack ident <>
                        "\x1b[0m does not use \x1b[31mself\x1b[0m. It must be reimplemented as a function."))
            EMemberFunctionNotUsed ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Member function \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is not called by any member of the class."))
            EAssignedValueNotUsed ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("The value assigned to variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is never read."))
            EReadBeforeAssignment ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is declared without an initializer and there is a path that reaches this point without assigning it.\n" <>
                        "Assign the whole object on every path before reading it."))
            EPartialWriteBeforeAssignment ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is declared without an initializer and this writes only a part of it.\n" <>
                        "The whole object must be assigned before a field or an element of it is written."))
            EInitializerNotUsed ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("The value this initializer gives \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is overwritten before anybody reads it.\n" <>
                        "Move the declaration to where the value is computed, or declare the object without an initializer."))
-- | Print the error as is
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]

        where
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."
