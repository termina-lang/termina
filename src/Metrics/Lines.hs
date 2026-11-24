-- | Classification of the lines of a source file into code, comment and blank,
-- with the code lines that carry a comment and the comment lines of a
-- documentation comment counted apart. Comments are recognised by the same
-- delimiters as the parser, and string and character literals are skipped so
-- that a delimiter inside them is not taken as a comment.
module Metrics.Lines
  ( LineClass(..)
  , LineInfo(..)
  , LineCounts(..)
  , Lines
  , CodeStarts
  , classifyLines
  , scanLines
  , codeStarts
  , lastCodeLine
  , totalCounts
  , countRange
  , commentRatio
  , ecssCommentRatio
  ) where

import Data.Char (isSpace)
import qualified Data.Sequence as Seq

import Core.AST (DocForm(..))
import Parser.Parsing (docOpener)

-- | The class of a line. Every line belongs to exactly one.
data LineClass = Code | Comment | Blank
  deriving (Show, Eq)

-- | A classified line: its class, whether a code line also carries a comment
-- and whether a comment line belongs to a documentation comment.
data LineInfo = LineInfo
  { lineClass :: LineClass
  , lineEolComment :: Bool
  , lineDoc :: Bool
  } deriving (Show, Eq)

-- | The number of lines of each class in a range of lines.
data LineCounts = LineCounts
  { countTotal :: !Int
  , countCode :: !Int
  , countComment :: !Int
  , countBlank :: !Int
  , countEolComment :: !Int
  , countDoc :: !Int
  } deriving (Show, Eq)

instance Semigroup LineCounts where
  LineCounts a b c d e f <> LineCounts a' b' c' d' e' f' =
    LineCounts (a + a') (b + b') (c + c') (d + d') (e + e') (f + f')

instance Monoid LineCounts where
  mempty = LineCounts 0 0 0 0 0 0

-- | The cumulative counts of a file: element n holds the counts of lines 1 to
-- n, and element 0 holds zero.
newtype Lines = Lines (Seq.Seq LineCounts)

-- | Where the scanner is within a line.
data Mode
  = Normal
  | InLineComment Bool
  | InBlockComment Bool
  | InString
  | InChar

-- | What has been seen so far on the current line, and the column of the next
-- character, counted as Parsec counts it.
data Current = Current
  { started :: Bool
  , firstCode :: Maybe Int
  , hasComment :: Bool
  , hasDoc :: Bool
  , column :: Int
  }

-- | The state of a line that starts in the given mode. A line that starts
-- inside a block comment belongs to it even if it is blank.
fresh :: Mode -> Current
fresh mode =
  case mode of
    InBlockComment doc -> Current False Nothing True doc 1
    _ -> Current False Nothing False False 1

hasCode :: Current -> Bool
hasCode = maybe False (const True) . firstCode

-- | The mode in which the next line starts. Line comments and literals end at
-- the end of the line.
nextLine :: Mode -> Mode
nextLine mode =
  case mode of
    InBlockComment doc -> InBlockComment doc
    _ -> Normal

finish :: Current -> LineInfo
finish cur =
  if hasCode cur
    then LineInfo Code (hasComment cur) False
    else if hasComment cur
      then LineInfo Comment False (hasDoc cur)
      else LineInfo Blank False False

-- | Marks code at the current column.
code :: Current -> Current
code cur = cur { firstCode = Just (maybe (column cur) id (firstCode cur)) }

comment :: Bool -> Current -> Current
comment doc cur = cur { hasComment = True, hasDoc = hasDoc cur || doc }

-- | Moves the column over the next characters of the line. A tab moves it to
-- the next multiple of eight plus one, as in Parsec.
consume :: Int -> Current -> String -> (Current, String)
consume n cur input = (cur { column = foldl next (column cur) (take n input) }, drop n input)

  where

    next col c = if c == '\t' then col + 8 - ((col - 1) `mod` 8) else col + 1

-- | The classified lines of a text, each with the column of its first code
-- character. A final newline does not start a line.
scan :: String -> [(LineInfo, Maybe Int)]
scan = go Normal (fresh Normal)

  where

    go mode cur input =
      case input of
        [] -> if started cur then [(finish cur, firstCode cur)] else []
        '\n' : rest -> (finish cur, firstCode cur) : go (nextLine mode) (fresh (nextLine mode)) rest
        _ -> step mode (cur { started = True }) input

    continue mode n cur input = uncurry (go mode) (consume n cur input)

    step mode cur input =
      case mode of
        Normal -> normal cur input
        InLineComment doc -> continue mode 1 (comment doc cur) input
        InBlockComment doc ->
          case input of
            '*' : '/' : _ -> continue Normal 2 (comment doc cur) input
            _ -> continue mode 1 (comment doc cur) input
        InString -> literal '"' InString cur input
        InChar -> literal '\'' InChar cur input

    normal cur input =
      case docOpener input of
        Just (_, LineDoc, len) -> continue (InLineComment True) len (comment True cur) input
        Just (_, BlockDoc, len) -> continue (InBlockComment True) len (comment True cur) input
        Nothing ->
          case input of
            '/' : '/' : _ -> continue (InLineComment False) 2 (comment False cur) input
            '/' : '*' : _ -> continue (InBlockComment False) 2 (comment False cur) input
            '"' : _ -> continue InString 1 (code cur) input
            '\'' : _ -> continue InChar 1 (code cur) input
            c : _ -> continue Normal 1 (if isSpace c then cur else code cur) input
            [] -> go Normal cur []

    -- An escape skips the next character unless it ends the line.
    literal close mode cur input =
      case input of
        '\\' : c : _ -> if c == '\n' then continue mode 1 cur input else continue mode 2 cur input
        c : _ -> if c == close then continue Normal 1 cur input else continue mode 1 cur input
        [] -> go mode cur []

-- | The classified lines of a text. A final newline does not start a line.
classifyLines :: String -> [LineInfo]
classifyLines = map fst . scan

-- | The column of the first code character of each line of a text, element 0
-- being line 1.
newtype CodeStarts = CodeStarts (Seq.Seq (Maybe Int))

codeStarts :: String -> CodeStarts
codeStarts = CodeStarts . Seq.fromList . map snd . scan

-- | The line of the last code character before a position, given by its line
-- and column. The parser ends an element after the white space and the
-- comments that follow it, and this is the line where the element really ends.
lastCodeLine :: CodeStarts -> Int -> Int -> Int
lastCodeLine (CodeStarts starts) line col = go line

  where

    go n =
      case if n >= 1 then Seq.lookup (n - 1) starts else Nothing of
        Just (Just c) -> if n < line || c < col then n else go (n - 1)
        Just Nothing -> go (n - 1)
        Nothing -> if n > Seq.length starts then go (Seq.length starts) else line

lineCounts :: LineInfo -> LineCounts
lineCounts info =
  case lineClass info of
    Code -> LineCounts 1 1 0 0 (fromEnum (lineEolComment info)) 0
    Comment -> LineCounts 1 0 1 0 0 (fromEnum (lineDoc info))
    Blank -> LineCounts 1 0 0 1 0 0

-- | The cumulative counts of a text.
scanLines :: String -> Lines
scanLines = Lines . Seq.scanl (<>) mempty . Seq.fromList . map lineCounts . classifyLines

at :: Lines -> Int -> LineCounts
at (Lines counts) n = Seq.index counts (max 0 (min n (Seq.length counts - 1)))

minus :: LineCounts -> LineCounts -> LineCounts
minus (LineCounts a b c d e f) (LineCounts a' b' c' d' e' f') =
  LineCounts (a - a') (b - b') (c - c') (d - d') (e - e') (f - f')

-- | The counts of the whole text.
totalCounts :: Lines -> LineCounts
totalCounts ls@(Lines counts) = at ls (Seq.length counts - 1)

-- | The counts of the lines from the first to the last given, both included
-- and numbered from 1.
countRange :: Lines -> Int -> Int -> LineCounts
countRange ls from to = at ls to `minus` at ls (from - 1)

ratio :: Int -> Int -> Maybe Double
ratio num den = if den == 0 then Nothing else Just (fromIntegral num / fromIntegral den)

-- | The GNATmetric comment ratio: comment lines and code lines with a comment
-- over comment and code lines.
commentRatio :: LineCounts -> Maybe Double
commentRatio c = ratio (countComment c + countEolComment c) (countComment c + countCode c)

-- | The closest equivalent of the comment frequency of ECSS-Q-HB-80-04A:
-- comment lines other than documentation over comment and code lines.
ecssCommentRatio :: LineCounts -> Maybe Double
ecssCommentRatio c = ratio (countComment c - countDoc c) (countComment c + countCode c)
