{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.ConstFolding.Errors where

import Semantic.AST
import Utils.Annotations
import Utils.Errors
import qualified Data.Text as T
import Semantic.Types
import Utils.Printer


data Error =
  EExpectedPort -- ^ Error when trying to access a port field (Internal)
  | EInvalidPortAccessExpression -- ^ Error when trying to access a port access expression (Internal)
  | EInvalidObjectTypeAnnotation -- ^ Error when the semantic annotation of an object does not contain the expected type information (Internal)
  | EInvalidExprTypeAnnotation -- ^ Error when the semantic annotation of an expression does not contain the expected type information (Internal)
  | EUnknownTask Identifier -- ^ Unknown task (Internal)
  | EUnknownTaskClass Identifier -- ^ Unknown task class (Internal)
  | EUnknownAccessPort Identifier Identifier -- ^ Unknown access port (Internal)
  | EUnknownHandler Identifier -- ^ Unknown handler (Internal)
  | EUnknownHandlerClass Identifier -- ^ Unknown handler class (Internal)
  | EUnknownMemberFunction Identifier -- ^ Unknown member function (Internal)
  | EUnknownResource Identifier -- ^ Unknown resource (Internal)
  | EUnknownResourceClass Identifier -- ^ Unknown resource class(Internal)
  | EUnknownResourceProcedure Identifier Identifier -- ^ Unknown resource procedure (Internal)
  | EUnknownIdentifier Identifier -- ^ Unknown identifier (Internal)
  | EInvalidObject -- ^ Invalid object (Internal)
  | EInvalidExpression String -- ^ Invalid expression (Internal)
  | EInvalidConstantEvaluation -- ^ Invalid constant evaluation (Internal)
  | ENotConstant -- ^ Not constant (Internal)
  | EInvalidReferenceType -- ^ Invalid reference type (Internal)
  | EInvalidSystemCallAnnotation -- ^ Invalid system call annotation (Internal)
  | EAtomicArrayConnectionSizeMismatch Integer Integer -- ^ Atomic array connection size mismatch
  | EArrayInitializerSizeMismatch Integer Integer -- ^ Array initializer size mismatch
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
  | EShiftAmountOutOfBounds Integer Integer -- ^ Shift amount out of bounds (width, amount)
  | EInvariantComparison Integer (TerminaType SemanticAnn) Bool -- ^ Comparison against a constant with a fixed result
  deriving Show

type ConstFoldError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EAtomicArrayConnectionSizeMismatch expectedSize actualSize) =
        diagnostic "CFE-001" "atomic array connection size mismatch"
            ("The size of the connected atomic array is expected to be " <> emph (T.pack (show expectedSize)) <>
                " but the array has size " <> emph (T.pack (show actualSize)) <> ".")
    describe (EArrayInitializerSizeMismatch expectedSize initializerSize) =
        diagnostic "CFE-002" "array initializer size mismatch"
            ("The size of the array initializer is " <> emph (T.pack (show initializerSize)) <>
                " but the expected size is " <> emph (T.pack (show expectedSize)) <> ".")
    describe (EStringInitializerInvalidSize expectedSize initializerSize) =
        diagnostic "CFE-003" "invalid string initializer size"
            ("The size of the string initializer is " <> emph (T.pack (show initializerSize)) <>
                " but the array size is of " <> emph (T.pack (show expectedSize)) <> ".")
    describe (EConstIntegerOverflow value ty) =
        diagnostic "CFE-004" "constant integer overflow"
            ("The resulting value " <> emph (T.pack (show value)) <>
                " is too large for the type " <> emph (showText ty) <> ".")
    describe (EConstIntegerUnderflow value ty) =
        diagnostic "CFE-005" "constant integer underflow"
            ("The resulting value " <> emph (T.pack (show value)) <>
                " produces an underflow of the type " <> emph (showText ty) <> ".")
    describe EConstDivisionByZero =
        diagnostic "CFE-006" "constant division by zero"
            "Division by zero in constant expression."
    describe (EConstCondition value) =
        diagnostic "CFE-007" "constant condition"
            ("The condition always evaluates to " <> emph (showText value) <> ".")
    describe EForLoopStatementZeroIterations =
        diagnostic "CFE-008" "for loop statement with zero iterations"
            "The for loop statement has zero iterations."
    describe (EForLoopStatementNegativeIterations startIndex endIndex) =
        diagnostic "CFE-009" "for loop statement with negative iterations"
            ("The for loop statement has negative iterations from " <> emph (T.pack (show startIndex)) <>
                " to " <> emph (T.pack (show endIndex)) <> ".")
    describe (EArraySliceOutOfBounds size upperIndex) =
        diagnostic "CFE-010" "array slice out of bounds"
            ("The array slice is out of bounds. The upper index " <> emph (T.pack (show upperIndex)) <>
                " is greater than the size of the array " <> emph (T.pack (show size)) <> ".")
    describe (EArraySliceNegativeRange lowerIndex upperIndex) =
        diagnostic "CFE-011" "array slice negative range"
            ("The array slice has a negative range. The lower index " <> emph (T.pack (show lowerIndex)) <>
                " is greater than the upper index " <> emph (T.pack (show upperIndex)) <> ".")
    describe (EArraySliceInvalidRange size lowerIndex upperIndex) =
        diagnostic "CFE-012" "array slice invalid range"
            ("The array slice has an invalid range. The size of the slice is expected to be " <> emph (T.pack (show size)) <>
                " and the range is from " <> emph (T.pack (show lowerIndex)) <>
                " to " <> emph (T.pack (show upperIndex)) <> ".")
    describe (EArrayIndexOutOfBounds size index) =
        diagnostic "CFE-013" "array index out of bounds"
            ("The array index is out of bounds. The index " <> emph (T.pack (show index)) <>
                " is greater than the size of the array " <> emph (T.pack (show size)) <> ".")
    describe (EAtomicArrayIndexOutOfBounds index size) =
        diagnostic "CFE-014" "atomic array index out of bounds"
            ("The atomic array index is out of bounds. The index " <> emph (T.pack (show index)) <>
                " is greater than the size of the atomic array " <> emph (T.pack (show size)) <> ".")
    describe (EReferencedArraySizeMismatch expectedSize actualSize) =
        diagnostic "CFE-015" "referenced array size mismatch"
            ("The referenced array size is " <> emph (T.pack (show actualSize)) <>
                " but the expected size is " <> emph (T.pack (show expectedSize)) <> ".")
    describe (EShiftAmountOutOfBounds width amount) =
        diagnostic "CFE-016" "shift amount out of bounds"
            ("The shift amount " <> emph (T.pack (show amount)) <>
                " is greater than or equal to the width " <> emph (T.pack (show width)) <>
                " of the shifted type.")
    describe (EInvariantComparison value ty result) =
        diagnostic "CFE-017" "invariant comparison"
            ("The comparison against " <> emph (T.pack (show value)) <>
                " always evaluates to " <> emph (if result then "true" else "false") <>
                " for any value of type " <> emph (showText ty) <> ".")
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage ConstFoldError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
