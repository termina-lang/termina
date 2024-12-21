{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Errors where

import Modules.Modules
import Utils.Annotations
import Text.Parsec
import Utils.Errors
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Parsec.Error

----------------------------------------
-- Type checker error handling
----------------------------------------
data Error
  = 
    EEmptyModuleName -- ^ Empty module name (Internal)
    | EParseError ParseError 
    | EInvalidModuleName QualifiedName -- ^ Invalid module name (PE-002)
    | EImportedFileNotFound QualifiedName -- ^ Imported file not found (PE-003)
  deriving Show

type ParsingErrors = AnnotatedError Error Location

instance ErrorMessage ParsingErrors where

    errorIdent (AnnotatedError (EParseError _err) _pos) = "PE-001"
    errorIdent (AnnotatedError ( EInvalidModuleName _qname) _pos) = "PE-002"
    errorIdent (AnnotatedError (EImportedFileNotFound _qname) _pos) = "PE-003"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (EParseError _err) _pos) = "parsing error"
    errorTitle (AnnotatedError (EInvalidModuleName _qname) _pos) = "invalid module name"
    errorTitle (AnnotatedError (EImportedFileNotFound _qname) _pos) = "imported file not found"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of
                EParseError pErr ->
                    let errorMsgs = showErrorMessages "or" "Unknown parse error"
                            "Expecting" "Unexpected" "end of input"
                           (errorMessages pErr)
                    in
                    pprintSimpleError
                        sourceLines title fileName pos Nothing <> 
                        T.pack errorMsgs
                EInvalidModuleName qname ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid module name: \x1b[31m" <> T.pack (show qname) <> "\x1b[0m.\n" <>
                        "No modules can be located at the root level."))
                EImportedFileNotFound qname -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Imported file invalid or not found: " <> T.pack (show qname)))
                _ -> T.pack $ show pos ++ ": " ++ show e
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostic _ _files = 
        LSP.Diagnostic 
            emptyRange
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        where 
            text = T.pack "\x1b[31mUknown\x1b[0m."