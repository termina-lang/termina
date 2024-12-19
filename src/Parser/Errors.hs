{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Errors where

import Modules.Modules
import Utils.Annotations
import Text.Parsec
import Utils.Errors
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Text as T

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
    errorIdent _ = "Unkonwn"

    errorTitle (AnnotatedError (EParsingError _err) _pos) = "parsing error"
    errorTitle _ = "Unknown"

    toText e@(AnnotatedError (EParsingError err) _pos) _files =
        let title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> ".\n"
        in
            title <> T.pack (show err)
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
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