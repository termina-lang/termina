{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.WCET.Errors where
import Utils.Annotations
import EFP.Schedulability.WCET.AST
import qualified Data.Text as T
import Utils.Errors
import Modules.Utils
import Utils.Printer


--------------------------------------------------
-- Transactional Path type checker error handling
--------------------------------------------------

data Error
  =
    EInvalidConstExpressionOperandTypes -- ^ Invalid constant expression operand types (internal)
    | EUnknownClass Identifier
    | EUnknownMemberFunction Identifier (Identifier, Location)
    | EDuplicatedWCETAssignment  Identifier Identifier (Identifier, Identifier, Location)
    | EUnknownVariable Identifier
    | EConstParamsNumMismatch Identifier Identifier Integer Integer Location
    | EConstVarAlreadyDefined (Identifier, Location)
    | EConstParamAlreadyDefined Identifier
    | EClassPathMismatch Identifier (Location, Location)
    | EInvalidPlatform Identifier
    | EUnknownTransactionalPath Identifier Identifier Identifier
    | EConstExpressionTypeMismatch ConstExprType ConstExprType -- ^ Constant expression type mismatch
    deriving Show

type WCEPathErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EUnknownClass ident) =
        diagnostic "WTE-001" "unknown class"
            ("Unknown class " <> emph (T.pack ident) <> ".")
    describe (EUnknownMemberFunction ident (classId, clsIdPos)) =
        relatedTo clsIdPos "the class is defined here" $
            diagnostic "WTE-002" "unknown member function"
                ("Class " <> emph (T.pack classId) <>
                    " does not have a member function called " <> emph (T.pack ident) <> ".")
    describe (EDuplicatedWCETAssignment pathName plt (classId, functionId, prevPos)) =
        relatedTo prevPos "the previous definition" $
            diagnostic "WTE-003" "duplicate path name"
                ("Duplicate worst-case execution time assignment on platform " <> emph (T.pack plt) <>
                    " for transactional path " <> emph (T.pack pathName) <>
                    " of member function " <> emph (T.pack functionId) <>
                    " of class " <> emph (T.pack classId) <> ".")
    describe (EUnknownVariable ident) =
        diagnostic "WTE-004" "unknown variable"
            ("Unknown variable " <> emph (T.pack ident) <> ".")
    describe (EConstParamsNumMismatch classId functionId expected got functionPos) =
        relatedTo functionPos "the member function is defined here" $
            diagnostic "WTE-005" "constant parameters number mismatch"
                ("Member function " <> emph (T.pack functionId) <>
                    " of class " <> emph (T.pack classId) <>
                    " defines " <> emph (T.pack (show expected)) <>
                    " constant parameters but " <> emph (T.pack (show got)) <>
                    " were provided.")
    describe (EConstVarAlreadyDefined (ident, identLoc)) =
        relatedTo identLoc "the previous definition" $
            diagnostic "WTE-006" "constant variable already defined"
                ("There exists a constant variable with the same name " <>
                    emph (T.pack ident) <> ".")
    describe (EConstParamAlreadyDefined ident) =
        diagnostic "WTE-007" "constant parameter already defined"
            ("There already exists a constant parameter with the name " <>
                emph (T.pack ident) <> " in the local scope.")
    describe (EClassPathMismatch classId (Position clsSource _ _, Position pathSource _ _)) =
        diagnostic "WTE-008" "class path mismatch"
            ("The transactional path is defined in a different module than the class.\nClass " <>
                emph (T.pack classId) <> " is defined in module " <>
                emph (T.pack (qualifiedToModuleName clsSource)) <>
                ", but the transactional path is defined in module " <>
                emph (T.pack (qualifiedToModuleName pathSource)) <> ".")
    describe (EClassPathMismatch _classId _locs) =
        diagnosticWithoutDetail "WTE-008" "class path mismatch"
    describe (EInvalidPlatform plt) =
        diagnostic "WTE-009" "invalid platform"
            ("Invalid platform " <> emph (T.pack plt) <>
                " specified for the worst-case execution time assignment.")
    describe (EUnknownTransactionalPath functionId classId pathName) =
        diagnostic "WTE-010" "unknown transactional path"
            ("Unknown transactional path " <> emph (T.pack pathName) <>
                " for member function " <> emph (T.pack functionId) <>
                " of class " <> emph (T.pack classId) <> ".")
    describe (EConstExpressionTypeMismatch t1 t2) =
        diagnostic "WTE-011" "constant expression type mismatch"
            ("Constant expression type mismatch: found " <> emph (showText t1) <>
                " and " <> emph (showText t2) <> ".")
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage WCEPathErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
