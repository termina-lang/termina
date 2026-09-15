{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Errors where

import qualified Data.Text as T
import Errata
import Errata.Styles

import Utils.Annotations
import Text.Parsec.Pos
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as TL
import Data.List (sortOn)
import Data.Maybe (mapMaybe)


-- | What an error says to whoever reads it. Besides the text, it carries what an
-- editor needs to do more than print it: whether the code it points at is
-- unnecessary, which is what greys it out, and the other places in the source
-- that explain the error.
data Diagnostic = Diagnostic
    {
      diagCode :: T.Text
    , diagTitle :: T.Text
    , diagDetail :: Maybe T.Text
    , diagUnnecessary :: Bool
    , diagRelated :: [(Location, T.Text)]
    }

-- | An error with a code, a title and an explanation.
diagnostic :: T.Text -> T.Text -> T.Text -> Diagnostic
diagnostic code title detail = Diagnostic code title (Just detail) False []

-- | An error whose title says everything there is to say.
diagnosticWithoutDetail :: T.Text -> T.Text -> Diagnostic
diagnosticWithoutDetail code title = Diagnostic code title Nothing False []

-- | Marks an error that points at code nobody needs, which an editor greys out
-- instead of underlining.
unnecessary :: Diagnostic -> Diagnostic
unnecessary d = d { diagUnnecessary = True }

-- | Adds another place in the source that explains the error, such as where a
-- box was moved before or which previous borrow is in the way.
relatedTo :: Location -> T.Text -> Diagnostic -> Diagnostic
relatedTo loc what d = d { diagRelated = diagRelated d ++ [(loc, what)] }

-- | An error that answers with one table, a row per constructor, instead of with
-- one case analysis per question.
class Show e => Diagnosable e where

    describe :: e -> Diagnostic

-- | Highlights a name quoted inside the explanation of an error.
emph :: T.Text -> T.Text
emph text = "\x1b[31m" <> text <> "\x1b[0m"

-- | Drops the colour escapes of a message, which a terminal reads and an editor
-- shows verbatim.
stripAnsi :: T.Text -> T.Text
stripAnsi = T.concat . go

    where

        go text =
            case T.breakOn "\x1b[" text of
                (before, rest) | T.null rest -> [before]
                (before, rest) -> before : go (T.drop 1 (T.dropWhile (/= 'm') rest))

-- | The message of an annotated error, from its description.
errorToText :: Diagnosable e
    => AnnotatedError e Location -> M.Map FilePath T.Text -> T.Text
-- | The source of the file the error points at is quoted when it is at hand. It
-- is not when the error comes from a stage that is given no sources, such as the
-- generation of the scheduling model.
errorToText (AnnotatedError err pos@(Position _ start _end)) files
    | M.member (sourceName start) files =
    let diag = describe err
        title = "\x1b[31merror [" <> diagCode diag <> "]\x1b[0m: "
            <> diagTitle diag <> "."
    in
        pprintError files title pos (diagRelated diag) (diagDetail diag)
-- | An error with no position in the source has nothing to quote, so it prints
-- its message alone. An internal error has no message either, and then the value
-- that produced it is the only clue there is.
errorToText (AnnotatedError err pos) _files =
    let diag = describe err
        title = "\x1b[31merror [" <> diagCode diag <> "]\x1b[0m: "
            <> diagTitle diag <> "."
    in
        case diagDetail diag of
            Just detail -> title <> "\n" <> detail
            Nothing -> title <> "\n" <> T.pack (show pos ++ ": " ++ show err)

-- | The LSP diagnostic of an annotated error, from its description. The fields
-- are, in order, range, severity, code, code description, source, message, tags,
-- related information and data.
errorToDiagnostics :: Diagnosable e
    => AnnotatedError e Location -> M.Map FilePath T.Text -> [LSP.Diagnostic]
errorToDiagnostics (AnnotatedError err pos) _files =
    [LSP.Diagnostic (loc2Range pos)
        (Just LSP.DiagnosticSeverity_Error)
        (Just (LSP.InR (diagCode diag)))
        Nothing
        (Just "termina")
        message
        (Just tags)
        (Just related)
        Nothing]

    where

        diag = describe err

        -- | The editor shows the explanation next to the title, without the
        -- colour escapes the terminal reads.
        message =
            case diagDetail diag of
                Nothing -> diagTitle diag <> "."
                Just detail -> diagTitle diag <> ".\n" <> stripAnsi detail

        tags = [LSP.DiagnosticTag_Unnecessary | diagUnnecessary diag]

        related =
            [ LSP.DiagnosticRelatedInformation lspLoc (stripAnsi what)
            | (loc, what) <- diagRelated diag
            , Just lspLoc <- [loc2Location loc] ]

-- | The LSP location of a position in the source, which a related piece of
-- information needs so that the editor can navigate to it.
loc2Location :: Location -> Maybe LSP.Location
loc2Location pos@(Position file _ _) = Just (LSP.Location (LSP.filePathToUri file) (loc2Range pos))
loc2Location _ = Nothing

class ErrorMessage a where

    -- | Error identifier 
    errorIdent :: a -> T.Text

    -- | Error title
    errorTitle :: a -> T.Text

    -- | Generates a message from a given error.
    toText :: 
        a -- ^ The error
        -> M.Map FilePath T.Text -- ^ Map of the project's source files to their contents
        -> T.Text

    -- | Generates an LSP diagnostic from a given error
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

-- | The message of an error that quotes one piece of source and nothing else.
pprintSimpleError :: T.Text -> T.Text -> String -> Location -> Maybe T.Text -> T.Text
pprintSimpleError sourceLines errorMessage fileName pos =
    pprintError (M.singleton fileName sourceLines) errorMessage pos []

-- | The message of an error: the piece of source it points at, one more pointer
-- for each position related to it, and the explanation underneath. A related
-- position carries its own label, and one that quotes another file goes in a
-- block of its own, since a block quotes a single source.
pprintError :: M.Map FilePath T.Text -> T.Text -> Location
    -> [(Location, T.Text)] -> Maybe T.Text -> T.Text
pprintError files errorMessage pos related msg =
    mainMessage <> T.concat (map otherFileMessage otherFile)

    where

        sourceLines = M.findWithDefault "" fileName files

        fileName =
            case pos of
                Position _ start _ -> sourceName start
                _ -> ""

        mainMessage =
            case mkPointer Nothing pos of
                Nothing -> error "Internal error: invalid error position"
                Just mainPointer ->
                    let pointers = sortOn pointerLine
                            (mainPointer : mapMaybe (\(loc, what) -> mkPointer (Just what) loc) sameFile)
                    in
                        TL.toStrict $ prettyErrors sourceLines [genErrata pointers]

        -- | A position in another file is quoted on its own, with its label as
        -- the heading.
        otherFileMessage :: (Location, T.Text) -> T.Text
        otherFileMessage (loc@(Position _ start _), what) =
            pprintSimpleError (M.findWithDefault "" (sourceName start) files)
                ("\n" <> emph what <> ":") (sourceName start) loc Nothing
        otherFileMessage _ = ""

        (sameFile, otherFile) = span' related

        -- | A related position is drawn in the same block only when it quotes the
        -- same source.
        span' rs = ([ r | r@(loc, _) <- rs, sameSource pos loc ]
                   ,[ r | r@(loc, _) <- rs, not (sameSource pos loc), isPosition loc ])

        isPosition (Position {}) = True
        isPosition _ = False

        merged = not (null sameFile)

        mkPointer :: Maybe T.Text -> Location -> Maybe Pointer
        mkPointer label (Position _ start end) =
            let startLine = sourceLine start
                endLine = sourceLine end
                startColumn = sourceColumn start
                endColumn =
                    if startLine == endLine then
                        sourceColumn end
                    else
                        T.length (T.lines sourceLines !! (startLine - 1)) + 1
            in
                Just $ Pointer startLine startColumn endColumn merged
                    ((\what -> " " <> emph what) <$> label) fancyRedPointer
        mkPointer _ _ = Nothing

        genErrata :: [Pointer] -> Errata
        genErrata pointers = Errata
            (Just errorMessage)
            [Errata.Block
                fancyRedStyle
                (fileName, pointerLine (head pointers), pointerColStart (head pointers))
                Nothing
                pointers
                Nothing]
            msg
