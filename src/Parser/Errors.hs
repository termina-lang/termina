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
import Data.List (intercalate)
import System.FilePath (addTrailingPathSeparator)

----------------------------------------
-- Type checker error handling
----------------------------------------
data Error
  =
    EEmptyModuleName -- ^ Empty module name (Internal)
    | EParseError ParseError
    | EInvalidModuleName QualifiedName -- ^ Invalid module name (PE-002)
    | EImportedModuleNotFound QualifiedName FilePath [FilePath] -- ^ Imported module in none of the folders that the importer, given by its file, sees (PE-003)
    | EImportedFilesLoop [ModuleDependency] -- ^ Imported files loop (PE-004)
    | EImportedModuleHidden T.Text -- ^ Imported module in a folder the importer does not see (PE-005)
    | EImportedModuleNotExported T.Text -- ^ Imported module outside the exports of a library (PE-006)
    | EModuleNameCollision T.Text -- ^ Two modules of a build with one name (PE-007)
    | EInvalidUtf8 -- ^ Module file that is not valid UTF-8 (PE-008)
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
    describe (EImportedModuleNotFound qname importer folders) =
        diagnostic "PE-003" "imported module not found"
            ("The module " <> T.pack (map (\c -> if c == '/' then '.' else c) qname)
                <> " is in none of the folders that " <> T.pack importer <> " sees: "
                <> T.pack (inWords (map addTrailingPathSeparator folders)) <> ".")

        where

            -- | The folders with commas between them and "and" before the last
            inWords [] = ""
            inWords [one] = one
            inWords several = intercalate ", " (init several) ++ " and " ++ last several
    describe (EImportedFilesLoop _imports) =
        diagnostic "PE-004" "cycle between project source files"
            "A recursive module import loop has been detected."
    describe (EImportedModuleHidden detail) =
        diagnostic "PE-005" "imported module not visible" detail
    describe (EImportedModuleNotExported detail) =
        diagnostic "PE-006" "imported module not exported" detail
    describe (EModuleNameCollision detail) =
        diagnostic "PE-007" "two modules with the same name" detail
    describe EInvalidUtf8 =
        diagnostic "PE-008" "invalid file encoding" "The file is not valid UTF-8."
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
