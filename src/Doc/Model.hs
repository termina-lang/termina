{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The documentation model of a comment: its lines without the margin, the
-- split of those lines into the body and the tags, and the Markdown of both.
module Doc.Model
  ( DocLine(..)
  , ParamMode(..)
  , TagKind(..)
  , DocTag(..)
  , DocError(..)
  , DocModel(..)
  , DocBlock(..)
  , DocListKind(..)
  , DocAlignment(..)
  , DocInline(..)
  , DocRef(..)
  , linesAt
  , commentLines
  , splitDoc
  , docModel
  , markdown
  ) where

import Commonmark (IsInline(..), IsBlock(..), Rangeable(..), HasAttributes(..), ListType(..), commonmarkWith, defaultSyntaxSpec)
import Commonmark.Extensions (HasMath(..), HasPipeTable(..), ColAlignment(..), mathSpec, pipeTableSpec)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Functor.Identity (runIdentity)
import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos, updatePosString, incSourceLine, incSourceColumn, initialPos)

import Core.AST (DocComment(..), DocForm(..), DocPlacement(..))
import Utils.Annotations (Location(..))

-- | A line of a comment and the position of its first character.
data DocLine = DocLine
  { linePos :: SourcePos
  , lineText :: String
  } deriving (Show, Eq)

-- | Whether a parameter is marked as only written by @[out]@.
data ParamMode = ParamPlain | ParamOut
  deriving (Show, Eq)

-- | The kind of a tag, with the arguments that come before its text.
data TagKind
  = TagParam ParamMode String
  | TagReturn
  | TagError String
  | TagPre
  | TagPost
  | TagInvariant
  | TagRange
  | TagUnit
  | TagNote
  | TagWarning
  | TagSee
  | TagDeprecated
  | TagSince
  | TagSatisfies String
  | TagVerifies String
  | TagExample
  deriving (Show, Eq)

-- | A tag: the position of its @\@@, its kind and its text, as lines when the
-- comment is split and as Markdown blocks in the model.
data DocTag a = DocTag
  { tagPos :: SourcePos
  , tagKind :: TagKind
  , tagText :: a
  } deriving (Show, Eq)

-- | The documentation of an element: the first paragraph as its summary, the
-- rest of the body, the tags and the comment as written.
data DocModel = DocModel
  { docSummary :: [DocInline]
  , docBody :: [DocBlock]
  , docTags :: [DocTag [DocBlock]]
  , docSource :: DocComment
  } deriving Show

-- | A block of Markdown. A paragraph of a tight list is a paragraph too.
data DocBlock
  = DPara [DocInline]
  | DHeading Int [DocInline]
  | DList DocListKind [[DocBlock]]
  | DCodeBlock T.Text T.Text -- ^ The language, maybe empty, and the code
  | DQuote [DocBlock]
  | DRule
  | DTable [DocAlignment] [[DocInline]] [[[DocInline]]] -- ^ Alignments, header and rows
  deriving (Show, Eq)

data DocListKind = DBullets | DNumbered Int
  deriving (Show, Eq)

data DocAlignment = DAlignDefault | DAlignLeft | DAlignCenter | DAlignRight
  deriving (Show, Eq)

-- | An inline element of Markdown. Raw HTML is kept as text.
data DocInline
  = DText T.Text
  | DCode T.Text
  | DSoftBreak
  | DLineBreak
  | DEmph [DocInline]
  | DStrong [DocInline]
  | DLink DocRef -- ^ A link to a name, written [`Name`], not resolved yet
  | DUrl T.Text [DocInline] -- ^ A link with a destination, and its text
  | DImage T.Text [DocInline] -- ^ An image, and its description
  | DMath T.Text
  | DDisplayMath T.Text
  deriving (Show, Eq)

-- | A name of the program or of the language, as @Type@, @Type::member@ or
-- @Enum::Variant@, split at each @::@.
newtype DocRef = DocRef [T.Text]
  deriving (Show, Eq)

-- | An error in the tags of a comment.
data DocError
  = UnknownTag String -- ^ A tag that does not exist, as written after the @\@@
  | MissingArgument String -- ^ A tag without the argument that it takes
  | OutOnOtherTag String -- ^ @[out]@ on a tag other than @\@param@
  | TextAfterTags -- ^ Text that belongs to no tag after the first tag
  deriving (Show, Read, Eq)

-- | The lines of a text that starts at a position.
linesAt :: SourcePos -> String -> [DocLine]
linesAt pos text =
  case break (== '\n') text of
    (line, '\n' : rest) -> DocLine pos line : linesAt (updatePosString pos (line ++ "\n")) rest
    (line, _) -> [DocLine pos line]

-- | The lines of a comment without its margin, given its form and its lines as
-- written. In a block comment the first line is the text after the opening
-- delimiter; if every other non-blank line starts with @*@ after white space,
-- that @*@ and the space after it are removed, and otherwise their common
-- indentation. In a run of line comments the common indentation is removed.
-- Trailing white space and the blank lines at both ends are dropped.
commentLines :: DocForm -> [DocLine] -> [DocLine]
commentLines form raw = trimBlank (map trimEnd (margin form raw))

  where

    margin LineDoc ls = dedent ls
    margin BlockDoc [] = []
    margin BlockDoc (first : rest)
      | all starred (filter (not . blank) rest) = dropSpaces first : map unstar rest
      | otherwise = dropSpaces first : dedent rest

    starred l = "*" `isPrefixOf` dropWhile isSpace (lineText l)

    unstar l =
      let afterStar = dropChars (indentation l + 1) l
      in if " " `isPrefixOf` lineText afterStar then dropChars 1 afterStar else afterStar

    dedent ls =
      let common = minimumOr 0 (map indentation (filter (not . blank) ls))
      in map (dropChars common) ls

    trimEnd l = l { lineText = dropWhileEnd isSpace (lineText l) }

-- | The documentation model of a comment. The tags were already checked when
-- the comment was parsed, so the error is only reached by a comment built by
-- hand.
docModel :: DocComment -> Either (SourcePos, DocError) DocModel
docModel doc = do
  (body, tags) <- splitDoc (commentLines (docForm doc) (writtenLines doc))
  let (summary, rest) = case markdown body of
        DPara inlines : more -> (inlines, more)
        blocks -> ([], blocks)
  return (DocModel summary rest [ t { tagText = markdown (tagText t) } | t <- tags ] doc)

-- | The lines of a comment as written, each with its position. The parser
-- keeps the text without the delimiters and the position of the first one; a
-- run of line comments is taken as aligned on that column.
writtenLines :: DocComment -> [DocLine]
writtenLines doc =
  case docForm doc of
    BlockDoc -> linesAt (afterOpener start) (docText doc)
    LineDoc -> [ DocLine (afterOpener (incSourceLine start n)) l | (n, l) <- zip [0 ..] (lines' (docText doc)) ]

  where

    start = case docLocation doc of
      Position _ s _ -> s
      _ -> initialPos ""
    afterOpener pos = incSourceColumn pos (if docPlacement doc == DocAfter then 4 else 3)
    lines' text = map lineText (linesAt start text)

-- | The Markdown blocks of some lines: CommonMark with GitHub tables and math.
-- A link to a name is recognized after the parse, since CommonMark leaves a
-- [`Name`] without a reference definition as text and code.
markdown :: [DocLine] -> [DocBlock]
markdown ls =
  case runIdentity (commonmarkWith (mathSpec <> pipeTableSpec <> defaultSyntaxSpec) "" source) of
    Right (Blocks blocks) -> map linksInBlock blocks
    Left _ -> [DPara [DText source]]

  where

    -- Every line ends with a newline: the tables of commonmark-extensions drop
    -- a last row without one, and a closing fence without one leaves an empty
    -- paragraph after the code block.
    source = T.concat [ T.pack (lineText l) <> "\n" | l <- ls ]

-- | The links to names of a block, and its runs of text merged.
linksInBlock :: DocBlock -> DocBlock
linksInBlock block = case block of
  DPara is -> DPara (linksIn is)
  DHeading n is -> DHeading n (linksIn is)
  DList k items -> DList k (map (map linksInBlock) items)
  DQuote bs -> DQuote (map linksInBlock bs)
  DTable as h rows -> DTable as (map linksIn h) (map (map linksIn) rows)
  other -> other

linksIn :: [DocInline] -> [DocInline]
linksIn = merge . go

  where

    go (DText "[" : DCode name : DText "]" : rest)
      | Just ref <- docRef name = DLink ref : go rest
    go (DEmph is : rest) = DEmph (linksIn is) : go rest
    go (DStrong is : rest) = DStrong (linksIn is) : go rest
    go (DUrl u is : rest) = DUrl u (linksIn is) : go rest
    go (i : rest) = i : go rest
    go [] = []

    merge (DText a : DText b : rest) = merge (DText (a <> b) : rest)
    merge (i : rest) = i : merge rest
    merge [] = []

-- | The name that a code span in brackets links to, if it is a name.
docRef :: T.Text -> Maybe DocRef
docRef name
  | all (isIdentifier . T.unpack) parts = Just (DocRef parts)
  | otherwise = Nothing

  where

    parts = T.splitOn "::" name

-- The model as the target of the parser of CommonMark.

newtype Inlines = Inlines [DocInline]
  deriving Show

newtype Blocks = Blocks [DocBlock]
  deriving Show

instance Semigroup Inlines where
  Inlines a <> Inlines b = Inlines (a ++ b)

instance Monoid Inlines where
  mempty = Inlines []

instance Semigroup Blocks where
  Blocks a <> Blocks b = Blocks (a ++ b)

instance Monoid Blocks where
  mempty = Blocks []

instance Rangeable Inlines where
  ranged _ x = x

instance Rangeable Blocks where
  ranged _ x = x

instance HasAttributes Inlines where
  addAttributes _ x = x

instance HasAttributes Blocks where
  addAttributes _ x = x

instance IsInline Inlines where
  lineBreak = Inlines [DLineBreak]
  softBreak = Inlines [DSoftBreak]
  str t = Inlines [DText t]
  entity t = Inlines [DText t]
  escapedChar c = Inlines [DText (T.singleton c)]
  emph (Inlines is) = Inlines [DEmph is]
  strong (Inlines is) = Inlines [DStrong is]
  link url _ (Inlines is) = Inlines [DUrl url is]
  image url _ (Inlines is) = Inlines [DImage url is]
  code t = Inlines [DCode t]
  rawInline _ t = Inlines [DText t]

instance HasMath Inlines where
  inlineMath t = Inlines [DMath t]
  displayMath t = Inlines [DDisplayMath t]

instance IsBlock Inlines Blocks where
  paragraph (Inlines is) = Blocks [DPara is]
  plain (Inlines is) = Blocks [DPara is]
  thematicBreak = Blocks [DRule]
  blockQuote (Blocks bs) = Blocks [DQuote bs]
  codeBlock lang t = Blocks [DCodeBlock lang t]
  heading n (Inlines is) = Blocks [DHeading n is]
  rawBlock _ t = Blocks [DPara [DText t]]
  referenceLinkDefinition _ _ = mempty
  list kind _ items = Blocks [DList (listKind kind) [ bs | Blocks bs <- items ]]

    where

      listKind (OrderedList n _ _) = DNumbered n
      listKind _ = DBullets

instance HasPipeTable Inlines Blocks where
  pipeTable aligns header rows =
    Blocks [DTable (map alignment aligns) [ is | Inlines is <- header ] [ [ is | Inlines is <- row ] | row <- rows ]]

    where

      alignment LeftAlignedCol = DAlignLeft
      alignment CenterAlignedCol = DAlignCenter
      alignment RightAlignedCol = DAlignRight
      alignment DefaultAlignedCol = DAlignDefault

-- | The body of a comment and its tags, from its lines without the margin. The
-- first line that starts with @\@@ and a letter outside a code block opens the
-- tags. A tag continues on the lines right after it, and after a blank line on
-- the indented ones, as an item of a Markdown list; the indentation of its
-- first indented line is removed from all of them. A line without indentation
-- after a blank line is text after the tags.
splitDoc :: [DocLine] -> Either (SourcePos, DocError) ([DocLine], [DocTag [DocLine]])
splitDoc ls = do
  tags <- tagsOf rest
  return (trimBlank body, tags)

  where

    (body, rest) = bodyLines Nothing ls

    bodyLines _ [] = ([], [])
    bodyLines fence (l : more)
      | Nothing <- fence, isTagLine l = ([], l : more)
      | otherwise =
          let (b, r) = bodyLines (fenceAfter fence l) more
          in (l : b, r)

-- | The tags of the lines that start with the first tag line.
tagsOf :: [DocLine] -> Either (SourcePos, DocError) [DocTag [DocLine]]
tagsOf [] = Right []
tagsOf (l : ls) = do
  (pos, kind, firstText) <- tagLine l
  let (continued, rest) = continuation Nothing False ls
      text = trimBlank (firstText : dedentContinued continued)
  checkText pos kind text
  case rest of
    r : _ | not (isTagLine r) -> Left (linePos r, TextAfterTags)
    _ -> (DocTag pos kind text :) <$> tagsOf rest

  where

    -- The lines that continue a tag, and the rest. A tag line outside a code
    -- block, or a line without indentation after a blank one, ends it.
    continuation _ _ [] = ([], [])
    continuation fence afterBlank (c : more)
      | Just _ <- fence = taken
      | isTagLine c = ([], c : more)
      | blank c = taken
      | afterBlank && indentation c == 0 = ([], c : more)
      | otherwise = taken

      where

        taken =
          let (cs, r) = continuation (fenceAfter fence c) (blank c) more
          in (c : cs, r)

    dedentContinued continued =
      let first = [ indentation c | c <- continued, not (blank c), indentation c > 0 ]
      in case first of
        k : _ -> map (\c -> dropChars (min k (indentation c)) c) continued
        [] -> continued

    -- The tags whose argument is their text need one.
    checkText pos kind text
      | kind `elem` [TagSee, TagSince, TagExample], null text = Left (pos, MissingArgument (tagName kind))
      | otherwise = Right ()

    tagName TagSee = "see"
    tagName TagSince = "since"
    tagName _ = "example"

-- | The position of the @\@@ of a tag line, the kind of the tag and the rest of
-- the line after its arguments.
tagLine :: DocLine -> Either (SourcePos, DocError) (SourcePos, TagKind, DocLine)
tagLine l = do
  (kind, takesArgument) <- classify
  let afterArgs = dropSpaces (if takesArgument then dropChars (length argument) afterWord else afterWord)
  return (pos, kind, afterArgs)

  where

    atLine = dropChars (indentation l) l
    pos = linePos atLine
    afterAt = drop 1 (lineText atLine)
    name = takeWhile isAlpha afterAt
    bracket = case drop (length name) afterAt of
      '[' : more | (inside, ']' : _) <- break (== ']') more -> "[" ++ inside ++ "]"
      _ -> ""
    word = '@' : name ++ bracket
    afterWord = dropChars (length word) atLine
    -- The spaces before the argument and the argument itself.
    spacesBefore = length (takeWhile isSpace (lineText afterWord))
    token = takeWhile (not . isSpace) (drop spacesBefore (lineText afterWord))
    argument = replicate spacesBefore ' ' ++ token

    classify
      | name `notElem` map fst simpleTags ++ ["param", "error", "satisfies", "verifies"] =
          Left (pos, UnknownTag (name ++ bracket))
      | bracket == "[out]" && name /= "param" = Left (pos, OutOnOtherTag name)
      | not (null bracket) && bracket /= "[out]" = Left (pos, UnknownTag (name ++ bracket))
      | name == "param" =
          withArgument isIdentifier (TagParam (if null bracket then ParamPlain else ParamOut))
      | name == "error" = withArgument isVariantName TagError
      | name == "satisfies" = withArgument (not . null) TagSatisfies
      | name == "verifies" = withArgument (not . null) TagVerifies
      | Just kind <- lookup name simpleTags = Right (kind, False)
      | otherwise = Left (pos, UnknownTag name)

    withArgument valid kind
      | valid token = Right (kind token, True)
      | otherwise = Left (pos, MissingArgument name)

    -- The tags whose first word is part of their text.
    simpleTags =
      [ ("return", TagReturn), ("pre", TagPre), ("post", TagPost)
      , ("invariant", TagInvariant), ("range", TagRange), ("unit", TagUnit)
      , ("note", TagNote), ("warning", TagWarning), ("see", TagSee)
      , ("deprecated", TagDeprecated), ("since", TagSince), ("example", TagExample) ]

-- | Does a line start with a tag, an @\@@ and a letter after white space?
isTagLine :: DocLine -> Bool
isTagLine l = case dropWhile isSpace (lineText l) of
  '@' : c : _ -> isAlpha c
  _ -> False

-- | The fence of the code block open after a line, given the one open before
-- it: a run of at least three backticks or tildes opens one, and a run of the
-- same character at least as long, with nothing after it, closes it.
fenceAfter :: Maybe (Char, Int) -> DocLine -> Maybe (Char, Int)
fenceAfter fence l =
  case (fence, run) of
    (Nothing, Just (c, n)) | n >= 3 -> Just (c, n)
    (Just (c, n), Just (c', n')) | c == c', n' >= n, all isSpace (drop n' text) -> Nothing
    _ -> fence

  where

    text = dropWhile isSpace (lineText l)
    run = case text of
      c : _ | c == '`' || c == '~' -> Just (c, length (takeWhile (== c) text))
      _ -> Nothing

isIdentifier :: String -> Bool
isIdentifier (c : cs) = (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x == '_') cs
isIdentifier [] = False

-- | A variant as @V@ or as @E::V@.
isVariantName :: String -> Bool
isVariantName s = case breakOn s of
  (v, "") -> isIdentifier v
  (e, v) -> isIdentifier e && isIdentifier v

  where

    breakOn text = case text of
      ':' : ':' : rest -> ("", rest)
      c : rest -> let (a, b) = breakOn rest in (c : a, b)
      [] -> ("", "")

blank :: DocLine -> Bool
blank = all isSpace . lineText

indentation :: DocLine -> Int
indentation = length . takeWhile isSpace . lineText

-- | A line without its first characters, with its position moved past them.
dropChars :: Int -> DocLine -> DocLine
dropChars n (DocLine pos text) = DocLine (updatePosString pos (take n text)) (drop n text)

dropSpaces :: DocLine -> DocLine
dropSpaces l = dropChars (indentation l) l

trimBlank :: [DocLine] -> [DocLine]
trimBlank = dropWhileEnd blank . dropWhile blank

minimumOr :: Ord a => a -> [a] -> a
minimumOr d [] = d
minimumOr _ xs = minimum xs
