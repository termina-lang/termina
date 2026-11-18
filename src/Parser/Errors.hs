{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.Errors where

import Modules.Modules
import Parser.Parsing (misplacedDocComment, twoDocComments, docTagError)
import Core.AST (DocPlacement(..))
import Doc.Model (DocError(..))
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
    | EMisplacedDocComment DocPlacement -- ^ Documentation comment where the grammar takes none (PE-009)
    | ETwoDocComments -- ^ Element with a documentation comment before it and another one after it (PE-010)
    | EUnknownDocTag String -- ^ Documentation tag that does not exist (PE-011)
    | EMalformedDocTag DocError -- ^ Documentation tag without its argument, or text after the tags (PE-012)
  deriving Show

-- | The error of a failed parse of a module. The errors of the documentation
-- comments have their own codes.
parseError :: ParseError -> Error
parseError err
  | UnknownTag name : _ <- docErrors = EUnknownDocTag name
  | docError : _ <- docErrors = EMalformedDocTag docError
  | twoDocComments `elem` messages = ETwoDocComments
  | otherwise =
      case [ placement | placement <- [minBound .. maxBound]
                       , misplacedDocComment placement `elem` messages ] of
        placement : _ -> EMisplacedDocComment placement
        [] -> EParseError err

  where

    -- The Eq instance of Message compares only the kind of message, so the
    -- texts are compared instead.
    messages = [ messageString m | m@(Message _) <- errorMessages err ]
    docErrors = [ e | Just e <- map docTagError messages ]

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
    describe (EMisplacedDocComment placement) =
        diagnostic "PE-009" "misplaced documentation comment"
            (wherePlaced placement <> "\n"
                <> "A note that documents nothing is written with " <> emph "//"
                <> " or " <> emph "/* */" <> ".")

        where

            wherePlaced DocBefore =
                "A documentation comment that starts with " <> emph "///" <> " or "
                    <> emph "/**" <> " goes right before the declaration, field, variant,"
                    <> " statement or case that it documents."
            wherePlaced DocAfter =
                "A documentation comment that starts with " <> emph "///<" <> " or "
                    <> emph "/**<" <> " goes after the declaration, field, variant or"
                    <> " statement that it documents, on the same line."
            wherePlaced DocModule =
                "A documentation comment that starts with " <> emph "//!" <> " or "
                    <> emph "/*!" <> " documents the module and goes at its start,"
                    <> " before the imports."
    describe ETwoDocComments =
        diagnostic "PE-010" "element with two documentation comments"
            ("This element already has a documentation comment before it, and an element"
                <> " takes only one. The two texts go together in one comment, before the"
                <> " element or after it.")
    describe (EUnknownDocTag name) =
        diagnostic "PE-011" "unknown documentation tag"
            ("There is no tag " <> emph (T.pack ('@' : name)) <> ". The tags are "
                <> T.intercalate ", " (map (emph . ("@" <>)) (init tagNames))
                <> " and " <> emph ("@" <> last tagNames) <> "."
                <> hint)

        where

            tagNames =
                [ "param", "return", "error", "pre", "post", "invariant", "range", "unit"
                , "note", "warning", "see", "deprecated", "since", "satisfies", "verifies"
                , "example" ]
            hint
                | name == "brief" = "\nThe first paragraph of the comment is its summary."
                | "param[" `T.isPrefixOf` T.pack name =
                    "\nThe direction of a parameter comes from its type; " <> emph "@param[out]"
                        <> " marks a " <> emph "&mut" <> " parameter that is only written."
                | otherwise = "\nA line of text that starts with " <> emph "@" <> " is written "
                    <> emph "\\@" <> "."
    describe (EMalformedDocTag docError) =
        diagnostic "PE-012" "malformed documentation tag" (detail docError)

        where

            detail (MissingArgument tag) =
                emph (T.pack ('@' : tag)) <> " is followed by " <> argumentOf tag <> "."
            detail (OutOnOtherTag tag) =
                emph "[out]" <> " goes only on " <> emph "@param" <> ", not on "
                    <> emph (T.pack ('@' : tag)) <> "."
            detail TextAfterTags =
                "The tags go at the end of the comment, and this text follows them. A tag"
                    <> " continues on the lines right after it, and after a blank line on"
                    <> " indented lines; any other text goes before the first tag."
            detail (UnknownTag tag) = "There is no tag " <> emph (T.pack ('@' : tag)) <> "."
            argumentOf "param" = "the name of a parameter"
            argumentOf "error" = "a variant of the error type, written " <> emph "V"
                <> " or " <> emph "E::V"
            argumentOf "see" = "a link"
            argumentOf "since" = "a version"
            argumentOf "example" = "a link to a test case"
            argumentOf _ = "the identifier of a requirement"
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
