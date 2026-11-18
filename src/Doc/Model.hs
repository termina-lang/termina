-- | The documentation model of a comment: its lines without the margin, and
-- the split of those lines into the body and the tags.
module Doc.Model
  ( DocLine(..)
  , ParamMode(..)
  , TagKind(..)
  , DocTag(..)
  , DocError(..)
  , linesAt
  , commentLines
  , splitDoc
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Text.Parsec.Pos (SourcePos, updatePosString)

import Core.AST (DocForm(..))

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

-- | A tag: the position of its @\@@, its kind and the lines of its text.
data DocTag = DocTag
  { tagPos :: SourcePos
  , tagKind :: TagKind
  , tagText :: [DocLine]
  } deriving (Show, Eq)

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

-- | The body of a comment and its tags, from its lines without the margin. The
-- first line that starts with @\@@ and a letter outside a code block opens the
-- tags. A tag continues on the lines right after it, and after a blank line on
-- the indented ones, as an item of a Markdown list; the indentation of its
-- first indented line is removed from all of them. A line without indentation
-- after a blank line is text after the tags.
splitDoc :: [DocLine] -> Either (SourcePos, DocError) ([DocLine], [DocTag])
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
tagsOf :: [DocLine] -> Either (SourcePos, DocError) [DocTag]
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
      c : _ | c `elem` "`~" -> Just (c, length (takeWhile (== c) text))
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

    breakOn str = case str of
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
