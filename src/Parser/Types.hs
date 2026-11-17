module Parser.Types where

import Text.Parsec.Pos (SourcePos)
import Utils.Annotations

-- | Where a documentation comment goes: before an element, after it on the
-- same line, or at the start of a module.
data DocPlacement = DocBefore | DocAfter | DocModule
  deriving (Show, Eq, Enum, Bounded)

-- | The two forms of a documentation comment.
data DocForm = LineDoc | BlockDoc
  deriving (Show, Eq)

-- | A documentation comment as written: where it goes, its form, its text
-- without the delimiters, and where it is in the file.
data DocComment = DocComment
  { docPlacement :: DocPlacement
  , docForm :: DocForm
  , docText :: String
  , docLocation :: Location
  } deriving Show

-- | The annotation of the parser: where the element is in the file, and its
-- documentation comment, if it has one.
data ParserAnn = ParserAnn
  { parsedLocation :: Location
  , parsedDoc :: Maybe DocComment
  } deriving Show

instance Located ParserAnn where
  getLocation = parsedLocation
  updateLocation ann loc = ann { parsedLocation = loc }

-- | The annotation of an element between two positions of a module, with no
-- documentation comment.
located :: QualifiedName -> SourcePos -> SourcePos -> ParserAnn
located qname start end = undocumented (Position qname start end)

-- | The annotation of an element at a location, with no documentation comment.
undocumented :: Location -> ParserAnn
undocumented loc = ParserAnn loc Nothing
