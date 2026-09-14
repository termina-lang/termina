{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Errors where

import Modules.Modules
import Utils.Annotations
import Text.Parsec
import Utils.Errors
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Text.Parsec.Error

----------------------------------------
-- Type checker error handling
----------------------------------------
data Error
  =
    EEmptyModuleName -- ^ Empty module name (Internal)
    | EParseError ParseError
    | EInvalidModuleName QualifiedName -- ^ Invalid module name (PE-002)
    | EImportedFileNotFound QualifiedName -- ^ Imported file not found (PE-003)
    | EImportedFilesLoop [ModuleDependency] -- ^ Imported files loop (PE-004)
  deriving Show

type ParsingErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EParseError pErr) =
        diagnostic "PE-001" "parsing error"
            (T.pack (showErrorMessages "or" "Unknown parse error"
                "Expecting" "Unexpected" "end of input" (errorMessages pErr)))
    describe (EInvalidModuleName qname) =
        diagnostic "PE-002" "invalid module name"
            ("Invalid module name: " <> emph (T.pack (show qname)) <> ".\n" <>
                "No modules can be located at the root level.")
    describe (EImportedFileNotFound qname) =
        diagnostic "PE-003" "imported file not found"
            ("Imported file invalid or not found: " <> T.pack (show qname))
    describe (EImportedFilesLoop _imports) =
        diagnostic "PE-004" "cycle between project source files"
            "A recursive module import loop has been detected."
    describe EEmptyModuleName = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage ParsingErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError

    -- | An import loop walks several files, so its message is a block per module
    -- of the cycle instead of one block with pointers.
    toText e@(AnnotatedError (EImportedFilesLoop (ModuleDependency currentModule _ : xs)) _pos) files =
        errorToText e files <> printImportTrace currentModule xs

        where

            printImportTrace :: QualifiedName -> [ModuleDependency] -> T.Text
            printImportTrace _ [] = ""
            printImportTrace currentFile [ModuleDependency finalCall tracePos@(Position _ traceStartPos _)] =
                let title = "\nFinally, module " <> emph (T.pack currentFile) <>
                        " imports module " <> emph (T.pack finalCall) <> " again here:"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing
            printImportTrace currentFile (ModuleDependency nextCall tracePos@(Position _ traceStartPos _) : xr) =
                let title = "\nModule " <> emph (T.pack currentFile) <>
                        " imports module " <> emph (T.pack nextCall) <> " here:"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing <> printImportTrace nextCall xr
            printImportTrace _ _ = error "Internal error: invalid error position"

    toText e files = errorToText e files

    toDiagnostics = errorToDiagnostics
