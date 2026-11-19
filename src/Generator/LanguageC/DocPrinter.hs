{-# LANGUAGE OverloadedStrings #-}
-- | The printer of the documentation comments of the C: a 'CDoc' as a
-- Doxygen block, with the text of the author escaped twice, so that Doxygen
-- shows it as it is and so that the C comment cannot end early or start
-- another one.
module Generator.LanguageC.DocPrinter
  ( pprintCDoc
  , pprintComment
  , pprintTrailingCDoc
  , cDocLines
  , escapeDoxygen
  , escapeComment
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (align, column, concatWith, hardline, pretty)

import Doc.Model (DocBlock'(..), DocInline'(..), DocListKind(..), DocAlignment(..))
import Generator.LanguageC.AST
import Utils.Printer (DocStyle)

-- | A documentation comment as a Doxygen block, with its prose wrapped at 80
-- columns from the column where the comment starts.
pprintCDoc :: CDoc -> DocStyle
pprintCDoc doc = column $ \c ->
    align (concatWith (\a b -> a <> hardline <> b)
        ("/**" : map commentLine (cDocLines (max 40 (80 - c - 3)) doc) ++ [" */"]))

    where

        commentLine l = pretty (if T.null l then " *" else " * " <> l)

-- | The comment of a statement, on one line when it has one and as a block
-- with a star on each line otherwise. Doxygen does not read it, so only the
-- escaping of C applies.
pprintComment :: CComment -> DocStyle
pprintComment (CComment [l]) = pretty ("/* " <> escapeComment l <> " */")
pprintComment (CComment ls) =
    align (concatWith (\a b -> a <> hardline <> b) ("/*" : map commentLine ls ++ [" */"]))

    where

        commentLine l = pretty (if T.null l then " *" else " * " <> escapeComment l)

-- | A documentation comment after a member or an enumerator, @/**< ... */@,
-- when it is only a brief of at most 60 characters on one line.
pprintTrailingCDoc :: CDoc -> Maybe DocStyle
pprintTrailingCDoc (CDoc brief [] []) =
    case map escapeComment (wrapText maxBound (inlineText brief)) of
        [text] | T.length text <= 60 -> Just (pretty ("/**< " <> text <> " */"))
        _ -> Nothing
pprintTrailingCDoc _ = Nothing

-- | The lines of a documentation comment, without the delimiters and the
-- star of each line, wrapped at a width. The brief, each block of the body,
-- the tags and each section are separated by a blank line.
cDocLines :: Int -> CDoc -> [Text]
cDocLines width (CDoc brief body tags) =
    map escapeComment (L.intercalate [""] (filter (not . null) groups))

    where

        groups = [ hanging width "@brief " [DPara brief] | not (null brief) ]
            ++ map (blockLines width) body
            ++ [ concatMap (tagLines width) [ t | t <- tags, not (isSection t) ] ]
            ++ [ tagLines width t | t <- tags, isSection t ]

        isSection (CDocSection _ _) = True
        isSection _ = False

-- | The lines of a tag. Its text goes on its line and on the lines below,
-- indented under it, without blank lines, which would end it.
tagLines :: Int -> CDocTag -> [Text]
tagLines width tag = case tag of
    CDocParam dir name bs -> hanging width ("@param" <> direction dir <> " " <> T.pack name <> " ") bs
    CDocReturn bs -> hanging width "@return " bs
    CDocPre bs -> hanging width "@pre " bs
    CDocPost bs -> hanging width "@post " bs
    CDocInvariant bs -> hanging width "@invariant " bs
    CDocNote bs -> hanging width "@note " bs
    CDocWarning bs -> hanging width "@warning " bs
    CDocSee bs -> hanging width "@see " bs
    CDocDeprecated bs -> hanging width "@deprecated " bs
    CDocSince bs -> hanging width "@since " bs
    CDocSection title bs -> ("@par " <> escapeDoxygen title) : concatMap (blockLines width) bs

    where

        direction CParamIn = "[in]"
        direction CParamOut = "[out]"
        direction CParamInOut = "[in,out]"

-- | Blocks after a prefix on their first line, with the lines below indented
-- by its width.
hanging :: Int -> Text -> [DocBlock' Ident] -> [Text]
hanging width prefix bs =
    case concatMap (blockLines (max 20 (width - T.length prefix))) bs of
        first : rest -> (prefix <> first) : map (\l -> if T.null l then l else indentation <> l) rest
        [] -> [T.stripEnd prefix]

    where

        indentation = T.replicate (T.length prefix) " "

-- | The lines of a block of Markdown.
blockLines :: Int -> DocBlock' Ident -> [Text]
blockLines width block = case block of
    DPara is -> wrapText width (inlineText is)
    DHeading _ is -> hanging width "@par " [DPara is]
    DList kind items -> concat (zipWith (hanging width) (markers kind) items)
    DCodeBlock lang code -> ("```" <> lang) : T.lines code ++ ["```"]
    DQuote bs -> map ("> " <>) (L.intercalate [""] (map (blockLines (width - 2)) bs))
    DRule -> ["---"]
    DTable aligns header rows -> row header : row (map alignment aligns) : map row rows

    where

        markers DBullets = repeat "- "
        markers (DNumbered n) = [ T.pack (show k) <> ". " | k <- [n ..] ]
        row cells = "| " <> T.intercalate " | " (map (T.map restore . inlineText) cells) <> " |"
        alignment DAlignDefault = [DText "---"]
        alignment DAlignLeft = [DText ":--"]
        alignment DAlignCenter = [DText ":-:"]
        alignment DAlignRight = [DText "--:"]

-- | The text of inline elements in Markdown for Doxygen. The spaces where a
-- line cannot be broken are 'unbreakable'.
inlineText :: [DocInline' Ident] -> Text
inlineText = T.concat . map one

    where

        one (DText t) = escapeDoxygen t
        one (DCode t) = whole (codeSpan t)
        one DSoftBreak = " "
        one DLineBreak = " <br> "
        one (DEmph is) = "*" <> inlineText is <> "*"
        one (DStrong is) = "**" <> inlineText is <> "**"
        one (DLink name) = whole (codeSpan (T.pack name))
        one (DUrl url is) = "[" <> inlineText is <> "](" <> whole url <> ")"
        one (DImage url is) = "![" <> inlineText is <> "](" <> whole url <> ")"
        one (DMath t) = whole ("\\f$" <> t <> "\\f$")
        one (DDisplayMath t) = whole ("\\f[" <> t <> "\\f]")

        whole = T.map (\c -> if c == ' ' then unbreakable else c)

        -- A code span with more backticks around it than in any run inside.
        codeSpan t =
            let fence = T.replicate (1 + maximum (0 : map T.length (filter ("`" `T.isPrefixOf`) (T.group t)))) "`"
                pad = if "`" `T.isPrefixOf` t || "`" `T.isSuffixOf` t then " " else ""
            in fence <> pad <> t <> pad <> fence

-- | The space between two words that a line is not broken at.
unbreakable :: Char
unbreakable = '\x1F'

restore :: Char -> Char
restore c = if c == unbreakable then ' ' else c

-- | A text broken into lines of at most a width, at its spaces. A word that
-- Markdown would read as the start of a list, a quote, a heading or a fence at
-- the start of a line is kept on the line of the word before it.
wrapText :: Int -> Text -> [Text]
wrapText width text = map (T.map restore) (fill (glue (filter (not . T.null) (T.split (== ' ') text))))

    where

        glue (w : w' : ws) | startsBlock w' = glue ((w <> T.singleton unbreakable <> w') : ws)
        glue (w : ws) = w : glue ws
        glue [] = []

        startsBlock w = case T.unpack w of
            c : _ | c `elem` ("-+>|=~" :: String) -> True
            ds@(d : _) | isDigit d, (_, p : _) <- span isDigit ds -> p == '.' || p == ')'
            _ -> False

        fill [] = []
        fill (w : ws) = go w ws

        go current [] = [current]
        go current (w : ws)
            | T.length current + 1 + T.length w <= width = go (current <> " " <> w) ws
            | otherwise = current : go w ws

-- | Text of the author written so that Doxygen shows it as it is: its commands,
-- the Markdown that marks emphasis or code, and what it would read as a link,
-- an HTML tag or an entity, escaped.
escapeDoxygen :: Text -> Text
escapeDoxygen = T.pack . go ' ' . T.unpack

    where

        go _ [] = []
        go prev (c : rest) = escaped prev c rest ++ go c rest

        escaped prev c rest = case c of
            '\\' -> "\\\\"
            '@' -> "\\@"
            '*' -> "\\*"
            '`' -> "\\`"
            '$' -> "\\$"
            '_' | not (isAlphaNum prev) || not (startsWith isAlphaNum rest) -> "\\_"
            '#' | startsWith (\x -> isAlphaNum x || x == ':') rest -> "\\#"
            '%' | startsWith isAlpha rest -> "\\%"
            '<' | startsWith (\x -> isAlpha x || x == '/' || x == '!') rest -> "\\<"
            '&' | startsWith (\x -> isAlpha x || x == '#') rest -> "\\&"
            _ -> [c]

        startsWith p (x : _) = p x
        startsWith _ [] = False

-- | A line of a comment written so that it cannot end the comment or start
-- another one, as MISRA C:2023 asks (Rules 3.1 and 4.2): a space breaks each
-- @/*@, @*/@, @//@ outside a URI and trigraph.
escapeComment :: Text -> Text
escapeComment = T.pack . go [] . T.unpack

    where

        go _ [] = []
        go before ('/' : '*' : rest) = '/' : ' ' : go (' ' : '/' : before) ('*' : rest)
        go before ('*' : '/' : rest) = '*' : ' ' : go (' ' : '*' : before) ('/' : rest)
        go before ('/' : '/' : rest)
            | not (afterScheme before) = '/' : ' ' : go (' ' : '/' : before) ('/' : rest)
        go before ('?' : '?' : c : rest)
            | c `elem` ("=/'()!<>-" :: String) = '?' : ' ' : go (' ' : '?' : before) ('?' : c : rest)
        go before (c : rest) = c : go (c : before) rest

        -- The text before a // ends in the scheme of a URI and its colon.
        afterScheme (':' : before) =
            let scheme = takeWhile (\x -> isAlphaNum x || x `elem` ("+.-" :: String)) before
            in not (null scheme) && isAlpha (last scheme)
        afterScheme _ = False
