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
    describe (EClassPathMismatch classId (Position clsSource _ _, Position pathSource _ _)) =
        diagnostic "WTE-005" "class path mismatch"
            ("The transactional path is defined in a different module than the class.\nClass " <>
                emph (T.pack classId) <> " is defined in module " <>
                emph (T.pack (qualifiedToModuleName clsSource)) <>
                ", but the transactional path is defined in module " <>
                emph (T.pack (qualifiedToModuleName pathSource)) <> ".")
    describe (EClassPathMismatch _classId _locs) =
        diagnosticWithoutDetail "WTE-005" "class path mismatch"
    describe (EInvalidPlatform plt) =
        diagnostic "WTE-006" "invalid platform"
            ("Invalid platform " <> emph (T.pack plt) <>
                " specified for the worst-case execution time assignment.")
    describe (EUnknownTransactionalPath functionId classId pathName) =
        diagnostic "WTE-007" "unknown transactional path"
            ("Unknown transactional path " <> emph (T.pack pathName) <>
                " for member function " <> emph (T.pack functionId) <>
                " of class " <> emph (T.pack classId) <> ".")
    describe (EConstExpressionTypeMismatch t1 t2) =
        diagnostic "WTE-008" "constant expression type mismatch"
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
