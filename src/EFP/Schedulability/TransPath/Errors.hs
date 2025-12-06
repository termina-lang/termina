{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.TransPath.Errors where
import Utils.Annotations
import EFP.Schedulability.TransPath.AST
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
import Utils.Errors
import Modules.Utils


--------------------------------------------------
-- Transactional Path type checker error handling
--------------------------------------------------

data Error
  = 
    EInvalidAccessPortAnnotation
    | EUnknownClass Identifier
    | EUnknownMemberFunction Identifier (Identifier, Location)
    | EDuplicatedPathName  Identifier (Identifier, Identifier, Location)
    | EUnknownAccessPort Identifier (Identifier, Location)
    | EUnknownVariable Identifier
    | EUnknownOutputPort Identifier (Identifier, Location)
    | EConstParamsNumMismatch Identifier Identifier Integer Integer Location
    | EConstVarAlreadyDefined (Identifier, Location)
    | EConstParamAlreadyDefined Identifier
    | EUnknownProcedure Identifier Identifier Identifier
    | EInvalidAccessToAllocator Identifier Identifier
    | EClassPathMismatch Identifier (Location, Location)
    deriving Show

type TransPathErrors = AnnotatedError Error Location

instance ErrorMessage TransPathErrors where


    errorIdent (AnnotatedError (EUnknownClass _id) _pos) = "TPE-001"
    errorIdent (AnnotatedError (EUnknownMemberFunction _id _clsId) _pos) = "TPE-002"
    errorIdent (AnnotatedError (EDuplicatedPathName _pathName (_classId, _functionId, _prevPos)) _pos) = "TPE-003"
    errorIdent (AnnotatedError (EUnknownAccessPort _id (_clsId, _clsIdPos)) _pos) = "TPE-004"
    errorIdent (AnnotatedError (EUnknownVariable _id) _pos) = "TPE-005"
    errorIdent (AnnotatedError (EUnknownOutputPort _id (_clsId, _clsIdPos)) _pos) = "TPE-006"
    errorIdent (AnnotatedError (EConstParamsNumMismatch _clsId _functionId _expected _got _functionPos) _pos') = "TPE-007"
    errorIdent (AnnotatedError (EConstVarAlreadyDefined _identLoc) _pos) = "TPE-008"
    errorIdent (AnnotatedError (EConstParamAlreadyDefined _ident) _pos) = "TPE-009"
    errorIdent (AnnotatedError (EUnknownProcedure _procName _portName _iface) _pos) = "TPE-010"
    errorIdent (AnnotatedError (EInvalidAccessToAllocator _procName _portName) _pos) = "TPE-011"
    errorIdent (AnnotatedError (EClassPathMismatch _classId _locs) _pos) = "TPE-012"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (EUnknownClass _id) _pos) = "unknown class"
    errorTitle (AnnotatedError (EUnknownMemberFunction _id (_classId, _classIdPos)) _pos) = "unknown member function"
    errorTitle (AnnotatedError (EDuplicatedPathName _pathName (_classId, _functionId, _prevPos)) _pos) = "duplicate path name"
    errorTitle (AnnotatedError (EUnknownAccessPort _id (_clsId, _clsIdPos)) _pos) = "unknown access port"
    errorTitle (AnnotatedError (EUnknownVariable _id) _pos) = "unknown variable"
    errorTitle (AnnotatedError (EUnknownOutputPort _id (_clsId, _clsIdPos)) _pos) = "unknown output port"
    errorTitle (AnnotatedError (EConstParamsNumMismatch _clsId _functionId _expected _got _functionPos) _pos') = "constant parameters number mismatch"
    errorTitle (AnnotatedError (EConstVarAlreadyDefined _identLoc) _pos) = "constant variable already defined"
    errorTitle (AnnotatedError (EConstParamAlreadyDefined _ident) _pos) = "constant parameter already defined"
    errorTitle (AnnotatedError (EUnknownProcedure _procName _portName _iface) _pos) = "unknown procedure"
    errorTitle (AnnotatedError (EInvalidAccessToAllocator _procName _portName) _pos) = "invalid access to allocator"
    errorTitle (AnnotatedError (EClassPathMismatch _classId _locs) _pos) = "class path mismatch"
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
                EDuplicatedPathName pathName (classId, functionId, prevPos@(Position _ prevStart _)) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Duplicate path name \x1b[31m" <> T.pack pathName <>
                                "\x1b[0m for member function \x1b[31m" <> T.pack functionId <>
                                "\x1b[0m of class \x1b[31m" <> T.pack classId <> "\x1b[0m."))
                        <>
                        pprintSimpleError
                            prevSourceLines "The previous definition is here:" prevFileName
                            prevPos Nothing
                EUnknownAccessPort ident (classId, clsIdPos@(Position _ clsStart _)) ->
                    let clsFileName = sourceName clsStart
                        clsSourceLines = files M.! clsFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Class \x1b[31m" <> T.pack classId <>
                                "\x1b[0m does not have an access port called \x1b[31m" <>
                                T.pack ident <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            clsSourceLines "The class is defined here:" clsFileName
                            clsIdPos Nothing
                EUnknownVariable ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown variable \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EUnknownOutputPort ident (classId, clsIdPos@(Position _ clsStart _)) ->
                    let clsFileName = sourceName clsStart
                        clsSourceLines = files M.! clsFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Class \x1b[31m" <> T.pack classId <>
                                "\x1b[0m does not have an output port called \x1b[31m" <>
                                T.pack ident <> "\x1b[0m.")) <>
                        pprintSimpleError
                            clsSourceLines "The class is defined here:" clsFileName
                            clsIdPos Nothing
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
                EUnknownProcedure procName portName iface ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The interface \x1b[31m" <> T.pack iface <>
                            "\x1b[0m of access port \x1b[31m" <> T.pack portName <>
                            "\x1b[0m does not have a procedure called \x1b[31m" <> T.pack procName <>
                            "\x1b[0m."))
                EInvalidAccessToAllocator procName portName ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Cannot access allocator port \x1b[31m" <> T.pack portName <>
                            "\x1b[0m to invoke procedure \x1b[31m" <> T.pack procName <>
                            "\x1b[0m\n." <> "Allocator port accesses must be done via 'alloc' and 'free' operations."))
                EClassPathMismatch classId (Position clsSource _ _, Position pathSource _ _) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The transactional path is defined in a different module than the class.\nClass \x1b[31m" <> 
                            T.pack classId <> "\x1b[0m is defined in module \x1b[31m" <> T.pack (qualifiedToModuleName clsSource) <>
                            "\x1b[0m, but the transactional path is defined in module \x1b[31m" <>
                            T.pack (qualifiedToModuleName pathSource) <> "\x1b[0m."))
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."