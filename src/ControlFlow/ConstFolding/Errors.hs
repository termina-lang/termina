{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.ConstFolding.Errors where

import Semantic.AST
import Utils.Annotations
import Utils.Errors
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
import Semantic.Types
import Utils.Printer


data Error =
  EExpectedPort -- ^ Error when trying to access a port field (Internal)
  | EInvalidPortAccessExpression -- ^ Error when trying to access a port access expression (Internal)
  | EInvalidObjectTypeAnnotation -- ^ Error when the semantic annotation of an object does not contain the expected type information (Internal)
  | EInvalidExprTypeAnnotation -- ^ Error when the semantic annotation of an expression does not contain the expected type information (Internal)
  | EUnknownTask Identifier -- ^ Unknown task (Internal)
  | EUnknownTaskClass Identifier -- ^ Unknown task class (Internal)
  | EUnknownInputPort Identifier Identifier -- ^ Unknown input port (Internal)
  | EUnknownAccessPort Identifier Identifier -- ^ Unknown access port (Internal)
  | EUnknownHandler Identifier -- ^ Unknown handler (Internal)
  | EUnknownHandlerClass Identifier -- ^ Unknown handler class (Internal)
  | EUnknownChannel Identifier -- ^ Unknown channel (Internal)
  | EUnknownMemberFunction Identifier -- ^ Unknown member function (Internal)
  | EUnknownResource Identifier -- ^ Unknown resource (Internal)
  | EUnknownResourceClass Identifier -- ^ Unknown resource class(Internal)
  | EUnknownResourceProcedure Identifier Identifier -- ^ Unknown resource procedure (Internal)
  | EUnknownIdentifier Identifier -- ^ Unknown identifier (Internal)
  | EInvalidObject -- ^ Invalid object (Internal)
  | EInvalidExpression String -- ^ Invalid expression (Internal)
  | EInvalidConstantEvaluation -- ^ Invalid constant evaluation (Internal)
  | ENotConstant -- ^ Not constant (Internal)
  | EInvalidParameterList -- ^ Invalid parameter list (Internal)
  | EInvalidFieldValueAssignmentAnnotation -- ^ Invalid field value assignment annotation (Internal)
  | EInvalidReferenceType -- ^ Invalid reference type (Internal)
  | EInvalidSystemCallAnnotation -- ^ Invalid system call annotation (Internal)
  | EAtomicArrayConnectionSizeMismatch Integer Integer -- ^ Atomic array connection size mismatch
  | EArrayInitializerSizeMismatch Integer Integer -- ^ Array initializer size mismatch
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ Array expression list array initializer size mismatch
  | EStringInitializerInvalidSize Integer Integer -- ^ String initializer size mismatch
  | EConstIntegerOverflow Integer (TerminaType SemanticAnn) -- ^ Constant integer overflow
  | EConstIntegerUnderflow Integer (TerminaType SemanticAnn) -- ^ Constant integer overflow
  | EConstDivisionByZero -- ^ Constant division by zero
  | EConstCondition (Const SemanticAnn) -- ^ Constant condition
  | EForLoopStatementZeroIterations -- ^ For loop statement with zero iterations
  | EForLoopStatementNegativeIterations Integer Integer -- ^ For loop statement with negative iterations
  | EArraySliceOutOfBounds Integer Integer -- ^ Array slice out of bounds
  | EArraySliceNegativeRange Integer Integer -- ^ Array slice negative range
  | EArraySliceInvalidRange Integer Integer Integer -- ^ Array slice invalid range
  | EArrayIndexOutOfBounds Integer Integer -- ^ Array index out of bounds
  | EAtomicArrayIndexOutOfBounds Integer Integer -- ^ Array index out of bounds
  | EReferencedArraySizeMismatch Integer Integer -- ^ Referenced array size mismatch
  deriving Show

type ConstFoldError = AnnotatedError Error Location

instance ErrorMessage ConstFoldError where

    errorIdent (AnnotatedError (EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize) _pos) = "CPE-001"
    errorIdent (AnnotatedError (EArrayInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "CPE-002"
    errorIdent (AnnotatedError (EArrayExprListInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "CPE-003"
    errorIdent (AnnotatedError (EStringInitializerInvalidSize _expectedSize _initializerSize) _pos) = "CPE-004"
    errorIdent (AnnotatedError (EConstIntegerOverflow _value _ty) _pos) = "CPE-005"
    errorIdent (AnnotatedError (EConstIntegerUnderflow _value _ty) _pos) = "CPE-006"
    errorIdent (AnnotatedError EConstDivisionByZero _pos) = "CPE-007"
    errorIdent (AnnotatedError (EConstCondition _const) _pos) = "CPE-008"
    errorIdent (AnnotatedError EForLoopStatementZeroIterations _pos) = "CPE-009"
    errorIdent (AnnotatedError (EForLoopStatementNegativeIterations _start _end) _pos) = "CPE-010"
    errorIdent (AnnotatedError (EArraySliceOutOfBounds _size _upperIndex) _pos) = "CPE-011"
    errorIdent (AnnotatedError (EArraySliceNegativeRange _lowerIndex _upperIndex) _pos) = "CPE-012"
    errorIdent (AnnotatedError (EArraySliceInvalidRange _size _lowerIndex _upperIndex) _pos) = "CPE-013"
    errorIdent (AnnotatedError (EArrayIndexOutOfBounds _index _size) _pos) = "CPE-014"
    errorIdent (AnnotatedError (EAtomicArrayIndexOutOfBounds _index _size) _pos) = "CPE-015"
    errorIdent (AnnotatedError (EReferencedArraySizeMismatch _expectedSize _actualSize) _pos) = "CPE-016"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize) _pos) = "atomic array connection size mismatch"
    errorTitle (AnnotatedError (EArrayInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "array initializer size mismatch"
    errorTitle (AnnotatedError (EArrayExprListInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "array expression list initializer size mismatch"
    errorTitle (AnnotatedError (EStringInitializerInvalidSize _expectedSize _initializerSize) _pos) = "invalid string initializer size"
    errorTitle (AnnotatedError (EConstIntegerOverflow _value _ty) _pos) = "constant integer overflow"
    errorTitle (AnnotatedError (EConstIntegerUnderflow _value _ty) _pos) = "constant integer underflow"
    errorTitle (AnnotatedError EConstDivisionByZero _pos) = "constant division by zero"
    errorTitle (AnnotatedError (EConstCondition _const) _pos) = "constant condition"
    errorTitle (AnnotatedError EForLoopStatementZeroIterations _pos) = "for loop statement with zero iterations"
    errorTitle (AnnotatedError (EForLoopStatementNegativeIterations _start _end) _pos) = "for loop statement with negative iterations"
    errorTitle (AnnotatedError (EArraySliceOutOfBounds _size _upperIndex) _pos) = "array slice out of bounds"
    errorTitle (AnnotatedError (EArraySliceNegativeRange _lowerIndex _upperIndex) _pos) = "array slice negative range"
    errorTitle (AnnotatedError (EArraySliceInvalidRange _size _lowerIndex _upperIndex) _pos) = "array slice invalid range"
    errorTitle (AnnotatedError (EArrayIndexOutOfBounds _index _size) _pos) = "array index out of bounds"
    errorTitle (AnnotatedError (EAtomicArrayIndexOutOfBounds _index _size) _pos) = "atomic array index out of bounds"
    errorTitle (AnnotatedError (EReferencedArraySizeMismatch _expectedSize _actualSize) _pos) = "referenced array size mismatch"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of 
                EAtomicArrayConnectionSizeMismatch expectedSize actualSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> T.pack (show expectedSize) <> 
                            "\x1b[0m but the array has size \x1b[31m" <> T.pack (show actualSize) <> "\x1b[0m."))
                EArrayInitializerSizeMismatch expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the array initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                            "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                            "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                EStringInitializerInvalidSize expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the string initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                            "\x1b[0m but the array size is of \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                EConstIntegerOverflow value ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The resulting value \x1b[31m" <> T.pack (show value) <>
                            "\x1b[0m is too large for the type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EConstIntegerUnderflow value ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The resulting value \x1b[31m" <> T.pack (show value) <>
                            "\x1b[0m produces an underflow of the type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EConstDivisionByZero ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Division by zero in constant expression.")
                EConstCondition value ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The condition always evaluates to \x1b[31m" <> showText value <> "\x1b[0m."))
                EForLoopStatementZeroIterations ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The for loop statement has zero iterations.")
                EForLoopStatementNegativeIterations startIndex endIndex ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The for loop statement has negative iterations from \x1b[31m" <> T.pack (show startIndex) <>
                            "\x1b[0m to \x1b[31m" <> T.pack (show endIndex) <> "\x1b[0m."))
                EArraySliceOutOfBounds size upperIndex ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The array slice is out of bounds. The upper index \x1b[31m" <> T.pack (show upperIndex) <>
                            "\x1b[0m is greater than the size of the array \x1b[31m" <> T.pack (show size) <> "\x1b[0m."))
                EArraySliceNegativeRange lowerIndex upperIndex ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The array slice has a negative range. The lower index \x1b[31m" <> T.pack (show lowerIndex) <>
                            "\x1b[0m is greater than the upper index \x1b[31m" <> T.pack (show upperIndex) <> "\x1b[0m."))
                EArraySliceInvalidRange size lowerIndex upperIndex ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The array slice has an invalid range. The size of the slice is expected to be \x1b[31m" <> T.pack (show size) <>
                            "\x1b[0m and the range is from \x1b[31m" <> T.pack (show lowerIndex) <>
                            "\x1b[0m to \x1b[31m" <> T.pack (show upperIndex) <> "\x1b[0m."))
                EArrayIndexOutOfBounds size index ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The array index is out of bounds. The index \x1b[31m" <> T.pack (show index) <>
                            "\x1b[0m is greater than the size of the array \x1b[31m" <> T.pack (show size) <> "\x1b[0m."))
                EAtomicArrayIndexOutOfBounds index size ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The atomic array index is out of bounds. The index \x1b[31m" <> T.pack (show index) <>
                            "\x1b[0m is greater than the size of the atomic array \x1b[31m" <> T.pack (show size) <> "\x1b[0m."))
                EReferencedArraySizeMismatch expectedSize actualSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The referenced array size is \x1b[31m" <> T.pack (show actualSize) <>
                            "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                _ -> pprintSimpleError sourceLines title fileName pos Nothing

    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."