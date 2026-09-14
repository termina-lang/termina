{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module With Errors

module ControlFlow.BoxUsage.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import ControlFlow.BoxUsage.Types
import Utils.Errors
import qualified Data.Text as T
import Text.Parsec
import qualified Data.Map.Strict as M
import qualified Language.LSP.Protocol.Types as LSP
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

instance ErrorMessage BoxUsageError where

    errorIdent (AnnotatedError (EBoxNotMoved _ident) _pos) = "BE-001"
    errorIdent (AnnotatedError (EBoxMovedTwice _ident _prevMove) _pos) = "BE-002"
    errorIdent (AnnotatedError (EOptionBoxMovedTwice _ident _prevMove) _pos) = "BE-003"
    errorIdent (AnnotatedError (EDifferentOptionBoxUse _ident _rval _lval) _pos) = "BE-004"
    errorIdent (AnnotatedError (EDifferentNewOptionBoxUse _ident _rval) _pos) = "BE-005"
    errorIdent (AnnotatedError (EMissingOptionBox _ident _prevVal) _pos) = "BE-006"
    errorIdent (AnnotatedError (EMissingBoxMove _ident _otherMove) _pos) = "BE-007"
    errorIdent (AnnotatedError (EBoxMoveConditionalBranch _ident) _pos) = "BE-008"
    errorIdent (AnnotatedError (EAllocNotMoved _ident) _pos) = "BE-009"
    errorIdent (AnnotatedError (EAllocTwice _ident _prevAlloc) _pos) = "BE-010"
    errorIdent (AnnotatedError (EMovedWithoutAlloc _ident _prevMove) _pos) = "BE-011"
    errorIdent (AnnotatedError EOptionBoxMatchMissingSomeCase _pos) = "BE-012"
    errorIdent (AnnotatedError e _pos) = T.pack $ show e

    errorTitle (AnnotatedError (EBoxNotMoved _ident) _pos) = "box variable not moved"
    errorTitle (AnnotatedError (EBoxMovedTwice _ident _prevMove) _pos) = "box variable moved twice"
    errorTitle (AnnotatedError (EOptionBoxMovedTwice _ident _prevMove) _pos) = "option-box variable moved twice"
    errorTitle (AnnotatedError (EDifferentOptionBoxUse _ident _rval _lval) _pos) = "option-box final state mismatch"
    errorTitle (AnnotatedError (EDifferentNewOptionBoxUse _ident _rval) _pos) = "option-box used in conditional branch or loop"
    errorTitle (AnnotatedError (EMissingOptionBox _ident _prevVal) _pos) = "option-box unused in a branch but used previously"
    errorTitle (AnnotatedError (EMissingBoxMove _ident _otherMove) _pos) = "box variable is not always moved"
    errorTitle (AnnotatedError (EBoxMoveConditionalBranch _ident) _pos) = "box variable moved in conditional branch"
    errorTitle (AnnotatedError (EAllocNotMoved _ident) _pos) = "option-box allocated but not moved"
    errorTitle (AnnotatedError (EAllocTwice _ident _prevAlloc) _pos) = "option-box allocated twice"
    errorTitle (AnnotatedError (EMovedWithoutAlloc _ident _prevMove) _pos) = "option-box moved but not allocated"
    errorTitle (AnnotatedError EOptionBoxMatchMissingSomeCase _pos) = "option-box match missing some"
    errorTitle _ = "internal error"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
        case err of
            EBoxNotMoved ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Box variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is declared but not moved."))
            EBoxMovedTwice ident prevMove@(Position _ moveStart _moveEnd) ->
                -- | We can safely assume that the previous move is in the same file
                let moveFileName = sourceName moveStart
                    moveSourceLines = files M.! moveFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Box variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m is moved twice.\n")) <>
                    pprintSimpleError
                        moveSourceLines "The previous move was done here:" moveFileName
                        prevMove Nothing
            EOptionBoxMovedTwice ident prevMove@(Position _ moveStart _moveEnd) ->
                -- | We can safely assume that the previous move is in the same file
                let moveFileName = sourceName moveStart
                    moveSourceLines = files M.! moveFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m is moved twice.\n")) <>
                    pprintSimpleError
                        moveSourceLines "The previous move was done here:" moveFileName
                        prevMove Nothing
            EDifferentOptionBoxUse ident rval (lval, otherPos@(Position _ otherStart _otherEnd)) ->
                -- | We can safely assume that the other position is in the same file
                let otherFileName = sourceName otherStart
                    otherSourceLines = files M.! otherFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("On this branch, the variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m has been \x1b[31m" <> showText rval <> "\x1b[0m.\n" <>
                            "The final state of the option-box variables in all branches must be the same.\n")) <>
                    pprintSimpleError
                        otherSourceLines ("However, in this branch, the option-box \x1b[31m" <> T.pack ident <>
                            "\x1b[0m has been \x1b[31m" <> showText lval <> "\x1b[0m:") otherFileName
                        otherPos Nothing
            EDifferentNewOptionBoxUse ident rval ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m has been \x1b[31m" <> showText rval <>
                        "\x1b[0m in a branch that may not be executed or inside a loop.\n" <> 
                        "This shall cause the final state to be inconsistent."))
            EMissingOptionBox ident prevVal ->
                -- | We can safely assume that the other position is in the same file
                let otherFileName = case getLocation prevVal of
                        Position _ otherStart _otherEnd -> sourceName otherStart
                        _ -> error "EMissingUsedOptionBox: TFixedLocation is not a position"
                    otherSourceLines = files M.! otherFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m is not used on this branch.\n" <>
                            "The final state of the option-box variables must be the same so that the resulting state is consistent.\n")) <>
                    pprintSimpleError
                        otherSourceLines ("However, in this previous branch, option-box \x1b[31m" <> T.pack ident <>
                            "\x1b[0m was \x1b[31m" <> showText prevVal <> "\x1b[0m:") otherFileName
                        (getLocation prevVal) Nothing
            EMissingBoxMove ident otherMove@(Position _ otherStart _othersEnd) ->
                let otherFileName = sourceName otherStart
                    otherSourceLines = files M.! otherFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Box variable \x1b[31m" <> T.pack ident <> "\x1b[0m is not moved on this branch.\n"
                        <> "The same box variables must be moved in all branches so that the resulting state is consistent.\n")) <>
                    pprintSimpleError
                        otherSourceLines ("However, in this branch, the box variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m has been moved:") otherFileName
                        otherMove Nothing
            EBoxMoveConditionalBranch ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Box variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is moved in a branch that may not be executed."))
            EAllocNotMoved ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is allocated but not moved."))
            EAllocTwice ident prevAlloc@(Position _ allocStart _allocEnd) ->
                let allocFileName = sourceName allocStart
                    allocSourceLines = files M.! allocFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m is allocated twice.\n")) <>
                    pprintSimpleError
                        allocSourceLines "The previous allocation was done here:" allocFileName
                        prevAlloc Nothing
            EMovedWithoutAlloc ident prevMove@(Position _ moveStart _moveEnd) ->
                let moveFileName = sourceName moveStart
                    moveSourceLines = files M.! moveFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                            "\x1b[0m is moved but not allocated.\n")) <>
                    pprintSimpleError
                        moveSourceLines "The variable was moved here:" moveFileName
                        prevMove Nothing
            EOptionBoxMatchMissingSomeCase ->
                pprintSimpleError 
                    sourceLines title fileName pos
                    (Just "Option-box match is missing Some case.")

            _ -> T.pack $ show pos ++ ": " ++ show e
-- | Print the error as is
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."