module Parser.Types where

import Text.Parsec.Pos (SourcePos)
import Core.AST (DocComment(..))
import Utils.Annotations

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
