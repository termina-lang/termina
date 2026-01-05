{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Errors where

import qualified Data.Text as T
import Errata
import Errata.Styles

import Semantic.AST
import Numeric
import Utils.Annotations
import Text.Parsec.Pos
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as TL
import qualified Parser.AST as PAST













class ErrorMessage a where

    -- | Error identifier 
    errorIdent :: a -> T.Text

    -- | Error title
    errorTitle :: a -> T.Text

    -- | Generates a message from a given error.
    toText :: 
        a -- ^ The error
        -> M.Map FilePath T.Text -- ^ Map of the project's source files to their contents
        -> T.Text

    -- | Generates an LSP diagnostic from a given error
    toDiagnostics :: 
        a -- ^ The error
        -> M.Map FilePath T.Text -- ^ Map of the project's source files to their contents
        -> [LSP.Diagnostic]
    
emptyRange :: LSP.Range
emptyRange = LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)

loc2Range :: Location -> LSP.Range
loc2Range (Position _ start end) = 
    LSP.Range 
        (LSP.Position (fromIntegral (sourceLine start) - 1) (fromIntegral (sourceColumn start) - 1))
        (LSP.Position (fromIntegral (sourceLine end) - 1) (fromIntegral (sourceColumn end) - 1))
loc2Range _ = emptyRange

pprintSimpleError :: T.Text -> T.Text -> String -> Location -> Maybe T.Text -> T.Text
pprintSimpleError sourceLines errorMessage fileName (Position _ start end) msg = 
    TL.toStrict $ prettyErrors sourceLines [genSimpleErrata]
    
    where

        startLine = sourceLine start
        endLine = sourceLine end
        startColumn = sourceColumn start
        endColumn = 
            if startLine == endLine then 
                sourceColumn end 
            else 
                T.length (T.lines sourceLines !! (startLine - 1)) + 1

        genSimpleBlock :: Errata.Block
        genSimpleBlock = Errata.Block
            fancyRedStyle
            (fileName, startLine, startColumn)
            Nothing
            [Pointer startLine startColumn endColumn False Nothing fancyRedPointer]
            Nothing

        genSimpleErrata :: Errata
        genSimpleErrata = Errata
            (Just errorMessage)
            [genSimpleBlock]
            msg
pprintSimpleError _ _ _ _ _ = error "Internal error: invalid error position"
