{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.WCEPath.Errors where
import Utils.Annotations
import EFP.Schedulability.WCEPath.AST
import qualified Data.Text as T
import Utils.Errors
import Modules.Utils
import Utils.Printer


---------------------------------------------------------
-- Worst-Case Execution Path type checker error handling
---------------------------------------------------------

data Error
  =
    EInvalidAccessPortAnnotation -- ^ Invalid access port annotation (internal)
    | EInvalidConstExpressionOperandTypes -- ^ Invalid constant expression operand types (internal)
    | EUnknownClass Identifier -- ^ Unknown class
    | EUnknownMemberFunction Identifier (Identifier, Location) -- ^ Unknown member function
    | EDuplicatedPathName  Identifier (Identifier, Identifier, Location) -- ^ Duplicated path name
    | EUnknownAccessPort Identifier (Identifier, Location) -- ^ Unknown access port
    | EUnknownVariable Identifier -- ^ Unknown variable
    | EUnknownOutputPort Identifier (Identifier, Location) -- ^ Unknown output port
    | EUnknownProcedure Identifier Identifier Identifier -- ^ Unknown procedure
    | EInvalidAccessToAllocator Identifier Identifier -- ^ Invalid access to allocator
    | EClassPathMismatch Identifier (Location, Location) -- ^ Class path mismatch
    | EConstExpressionTypeMismatch ConstExprType ConstExprType -- ^ Constant expression type mismatch
    deriving Show

type WCEPathErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EUnknownClass ident) =
        diagnostic "WCEPE-001" "unknown class"
            ("Unknown class " <> emph (T.pack ident) <> ".")
    describe (EUnknownMemberFunction ident (classId, clsIdPos)) =
        relatedTo clsIdPos "the class is defined here" $
            diagnostic "WCEPE-002" "unknown member function"
                ("Class " <> emph (T.pack classId) <>
                    " does not have a member function called " <> emph (T.pack ident) <> ".")
    describe (EDuplicatedPathName pathName (classId, functionId, prevPos)) =
        relatedTo prevPos "the previous definition" $
            diagnostic "WCEPE-003" "duplicated path name"
                ("Duplicate path name " <> emph (T.pack pathName) <>
                    " for member function " <> emph (T.pack functionId) <>
                    " of class " <> emph (T.pack classId) <> ".")
    describe (EUnknownAccessPort ident (classId, clsIdPos)) =
        relatedTo clsIdPos "the class is defined here" $
            diagnostic "WCEPE-004" "unknown access port"
                ("Class " <> emph (T.pack classId) <>
                    " does not have an access port called " <> emph (T.pack ident) <> ".")
    describe (EUnknownVariable ident) =
        diagnostic "WCEPE-005" "unknown variable"
            ("Unknown variable " <> emph (T.pack ident) <> ".")
    describe (EUnknownOutputPort ident (classId, clsIdPos)) =
        relatedTo clsIdPos "the class is defined here" $
            diagnostic "WCEPE-006" "unknown output port"
                ("Class " <> emph (T.pack classId) <>
                    " does not have an output port called " <> emph (T.pack ident) <> ".")
    describe (EConstExpressionTypeMismatch t1 t2) =
        diagnostic "WCEPE-007" "constant expression type mismatch"
            ("Constant expression type mismatch: found " <> emph (showText t1) <>
                " and " <> emph (showText t2) <> ".")
    describe (EUnknownProcedure procName portName iface) =
        diagnostic "WCEPE-008" "unknown procedure"
            ("The interface " <> emph (T.pack iface) <>
                " of access port " <> emph (T.pack portName) <>
                " does not have a procedure called " <> emph (T.pack procName) <> ".")
    describe (EInvalidAccessToAllocator procName portName) =
        diagnostic "WCEPE-009" "invalid access to allocator"
            ("Cannot access allocator port " <> emph (T.pack portName) <>
                " to invoke procedure " <> emph (T.pack procName) <> ".\n" <>
                "Allocator port accesses must be done via 'alloc' and 'free' operations.")
    describe (EClassPathMismatch classId (Position clsSource _ _, Position pathSource _ _)) =
        diagnostic "WCEPE-010" "class path mismatch"
            ("The transactional path is defined in a different module than the class.\nClass " <>
                emph (T.pack classId) <> " is defined in module " <>
                emph (T.pack (qualifiedToModuleName clsSource)) <>
                ", but the transactional path is defined in module " <>
                emph (T.pack (qualifiedToModuleName pathSource)) <> ".")
    describe (EClassPathMismatch _classId _locs) =
        diagnosticWithoutDetail "WCEPE-010" "class path mismatch"
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage WCEPathErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
