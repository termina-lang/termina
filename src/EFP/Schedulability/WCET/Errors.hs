{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.WCET.Errors where
import Utils.Annotations
import EFP.Schedulability.WCET.AST
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
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

instance ErrorMessage WCEPathErrors where


    errorIdent (AnnotatedError (EUnknownClass _id) _pos) = "WTE-001"
    errorIdent (AnnotatedError (EUnknownMemberFunction _id _clsId) _pos) = "WTE-002"
    errorIdent (AnnotatedError (EDuplicatedWCETAssignment _pathName _plt (_classId, _functionId, _prevPos)) _pos) = "PE-003"
    errorIdent (AnnotatedError (EUnknownVariable _id) _pos) = "TPE-004"
    errorIdent (AnnotatedError (EConstParamsNumMismatch _clsId _functionId _expected _got _functionPos) _pos') = "TPE-005"
    errorIdent (AnnotatedError (EConstVarAlreadyDefined _identLoc) _pos) = "TPE-006"
    errorIdent (AnnotatedError (EConstParamAlreadyDefined _ident) _pos) = "TPE-007"
    errorIdent (AnnotatedError (EClassPathMismatch _classId _locs) _pos) = "TPE-008"
    errorIdent (AnnotatedError (EInvalidPlatform _plt) _pos) = "PE-009"
    errorIdent (AnnotatedError (EUnknownTransactionalPath _functionId _classId _pathName) _pos) = "PE-010"
    errorIdent (AnnotatedError (EConstExpressionTypeMismatch _expected _got) _pos) = "PE-011"
    errorIdent _ = "Internal"


    errorTitle (AnnotatedError (EUnknownClass _id) _pos) = "unknown class"
    errorTitle (AnnotatedError (EUnknownMemberFunction _id (_classId, _classIdPos)) _pos) = "unknown member function"
    errorTitle (AnnotatedError (EDuplicatedWCETAssignment _pathName _plt (_classId, _functionId, _prevPos)) _pos) = "duplicate path name"
    errorTitle (AnnotatedError (EUnknownVariable _id) _pos) = "unknown variable"
    errorTitle (AnnotatedError (EConstParamsNumMismatch _clsId _functionId _expected _got _functionPos) _pos') = "constant parameters number mismatch"
    errorTitle (AnnotatedError (EConstVarAlreadyDefined _identLoc) _pos) = "constant variable already defined"
    errorTitle (AnnotatedError (EConstParamAlreadyDefined _ident) _pos) = "constant parameter already defined"
    errorTitle (AnnotatedError (EClassPathMismatch _classId _locs) _pos) = "class path mismatch"
    errorTitle (AnnotatedError (EInvalidPlatform _plt) _pos) = "invalid platform"
    errorTitle (AnnotatedError (EUnknownTransactionalPath _functionId _classId _pathName) _pos) = "unknown transactional path"
    errorTitle (AnnotatedError (EConstExpressionTypeMismatch _expected _got) _pos) = "constant expression type mismatch"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of 
                EUnknownClass ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown class \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EUnknownMemberFunction ident (classId, clsIdPos@(Position _ clsStart _)) ->
                    let clsFileName = sourceName clsStart
                        clsSourceLines = files M.! clsFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Class \x1b[31m" <> T.pack classId <>
                                "\x1b[0m does not have a member function called \x1b[31m" <>
                                T.pack ident <> "\x1b[0m.")) <>
                        pprintSimpleError
                            clsSourceLines "The class is defined here:" clsFileName
                            clsIdPos Nothing
                EDuplicatedWCETAssignment pathName plt (classId, functionId, prevPos@(Position _ prevStart _)) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Duplicate worst-case execution time assignment on platform \x1b[31m" <> T.pack plt <>
                                "\x1b[0m for transactional path \x1b[31m" <> T.pack pathName <>
                                "\x1b[0m of member function \x1b[31m" <> T.pack functionId <>
                                "\x1b[0m of class \x1b[31m" <> T.pack classId <> "\x1b[0m."))
                        <>
                        pprintSimpleError
                            prevSourceLines "The previous definition is here:" prevFileName
                            prevPos Nothing
                EUnknownVariable ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown variable \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EConstParamsNumMismatch classId functionId expected got functionPos@(Position _ funcStart _) ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Member function \x1b[31m" <> T.pack functionId <>
                                "\x1b[0m of class \x1b[31m" <> T.pack classId <>
                                "\x1b[0m defines \x1b[31m" <> T.pack (show expected) <>
                                "\x1b[0m constant parameters but \x1b[31m" <> T.pack (show got) <>
                                "\x1b[0m were provided."))
                        <>
                        pprintSimpleError
                            funcSourceLines "The member function is defined here:" funcFileName
                            functionPos Nothing 
                EConstVarAlreadyDefined (ident, identLoc@(Position _ identStart _)) ->
                    let identFileName = sourceName identStart
                        identSourceLines = files M.! identFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("There exists a constant variable with the same name \x1b[31m" <> 
                                T.pack ident <> "\x1b[0m."))
                        <>
                        pprintSimpleError
                            identSourceLines "The previous definition is here:" identFileName
                            identLoc Nothing
                EConstParamAlreadyDefined ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("There already exists a constant parameter with the name \x1b[31m" <> 
                            T.pack ident <> "\x1b[0m in the local scope."))
                EClassPathMismatch classId (Position clsSource _ _, Position pathSource _ _) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The transactional path is defined in a different module than the class.\nClass \x1b[31m" <> 
                            T.pack classId <> "\x1b[0m is defined in module \x1b[31m" <> T.pack (qualifiedToModuleName clsSource) <>
                            "\x1b[0m, but the transactional path is defined in module \x1b[31m" <>
                            T.pack (qualifiedToModuleName pathSource) <> "\x1b[0m."))
                EInvalidPlatform plt ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid platform \x1b[31m" <> T.pack plt <> "\x1b[0m specified for the worst-case execution time assignment."))
                EUnknownTransactionalPath functionId classId pathName ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown transactional path \x1b[31m" <> T.pack pathName <>
                            "\x1b[0m for member function \x1b[31m" <> T.pack functionId <>
                            "\x1b[0m of class \x1b[31m" <> T.pack classId <> "\x1b[0m."))
                EConstExpressionTypeMismatch t1 t2 ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Constant expression type mismatch: found \x1b[31m" <> showText t1 <> "\x1b[0m and \x1b[31m" <> showText t2 <> "\x1b[0m."))
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."