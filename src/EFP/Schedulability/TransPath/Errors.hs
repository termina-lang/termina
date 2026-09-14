{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.TransPath.Errors where
import Utils.Annotations
import qualified Data.Text as T
import Utils.Errors
import EFP.Schedulability.Core.AST

--------------------------------------------------
-- Transactional Path Generator error handling
--------------------------------------------------

data Error
  =
    EInvalidTransStepType -- ^ Invalid transactional step type (internal)
    | EUnknownComponent Identifier -- ^ Unknown component referenced in transactional step (internal)
    | EUnknownAction -- ^ Unknown action referenced in transactional step (internal)
    | EInvalidForLoop -- ^ Invalid for-loop structure in transactional step (internal)
    | EInvalidArgumentPassing -- ^ Invalid argument passing to transactional step (internal)
    | EUnknownAccessPort Identifier Identifier -- ^ Unknown access port referenced in worst-case execution path (internal)
    | EInvalidWCETExpression -- ^ Invalid worst-case execution time expression (internal)
    | EConstExpressionDivisionByZero -- ^ Division by zero in constant expression (internal)
    | EInvalidConstExpressionOperand Op -- ^ Invalid operand for constant expression (internal)
    | EInvalidRTElementForTransPath -- ^ Invalid RT element provided for transactional path generation (internal)
    | EConstExpressionTypeMismatch ConstExprType ConstExprType -- ^ Type mismatch in constant expression (internal)
    | EInvalidConstExpressionOperandTypes -- ^ Invalid operand types for constant expression (internal)
    | EUnknownConstant Identifier -- ^ Unknown constant in constant expression (internal)
    | ENoPathsFound Identifier Identifier -- ^ No worst-case paths found for the given component and member names
    | ENoWCETForPath Identifier Identifier Identifier Identifier -- ^ No worst-case execution time found
    deriving Show

type TRPGenErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (ENoPathsFound componentClass memberName) =
        diagnostic "TPE-001" "no worst-case execution paths found"
            ("No worst-case paths found for member function " <>
                emph (T.pack componentClass <> "::" <> T.pack memberName) <> ".")
    describe (ENoWCETForPath componentClass funcName pathId plt) =
        diagnostic "TPE-002" "no worst-case execution time found"
            ("No worst-case execution time found for path " <>
                emph (T.pack componentClass <> "::" <> T.pack funcName <> "::" <> T.pack pathId) <>
                " on platform " <> emph (T.pack plt) <> ".")
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage TRPGenErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
