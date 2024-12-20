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

----------------------------------------
-- Type checker error handling
----------------------------------------
data Error
  = 
    EEmptyModuleName -- ^ Empty module name (Internal)
    | EParsingError ParseError -- ^ Parsing error (PE-001)
    | EInvalidModuleName QualifiedName -- ^ Invalid module name (PE-002)
    | EImportedFileNotFound QualifiedName -- ^ Imported file not found (PE-003)
  deriving Show

type ParsingErrors = AnnotatedError Error Location

instance ErrorMessage ParsingErrors where

    errorIdent (AnnotatedError (EParsingError _err) _pos) = "PE-001"
    errorIdent (AnnotatedError ( EInvalidModuleName _qname) _pos) = "PE-002"
    errorIdent (AnnotatedError (EImportedFileNotFound _qname) _pos) = "PE-003"
    errorIdent _ = "Unkonwn"

    errorTitle (AnnotatedError (EParsingError _err) _pos) = "parsing error"
    errorTitle (AnnotatedError (EInvalidModuleName _qname) _pos) = "invalid module name"
    errorTitle (AnnotatedError (EImportedFileNotFound _qname) _pos) = "imported file not found"
    errorTitle _ = "Unknown"

    toText e@(AnnotatedError err pos@(Position start _end)) files =
        let title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of
                EParsingError parsingErr -> title <> "\n" <> T.pack (show parsingErr)
                EInvalidModuleName qname ->
                        "Invalid module name: \x1b[31m" <> T.pack (show qname) <> "\x1b[0m.\n" <>
                        "No modules can be located at the root level."
                EImportedFileNotFound qname -> 
                        "Imported file invalid or not found: " <> T.pack (show qname)
                _ -> T.pack $ show err
    toText (AnnotatedError e pos) _files = T.pack $ "Errorcito!" ++ show pos ++ ": " ++ show e
    
    toDiagnostic e@(AnnotatedError (EParsingError _err) pos) _files =
        LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        
        where 
            text = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
    toDiagnostic _ _files = 
        LSP.Diagnostic 
            emptyRange
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        where 
            text = T.pack "\x1b[31mUknown\x1b[0m."