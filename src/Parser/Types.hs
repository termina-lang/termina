module Parser.Types where

import Utils.Annotations

{- | Type of the parsing annotations
This type is used to identify the annotations made on the different
elements of the AST. In this case, the annotations will only include
the position in the source file where the element is located.
-}

type ParserAnn = Location

-- | Where a documentation comment goes: before an element, after it on the
-- same line, or at the start of a module.
data DocPlacement = DocBefore | DocAfter | DocModule
  deriving (Show, Eq, Enum, Bounded)

-- | The two forms of a documentation comment.
data DocForm = LineDoc | BlockDoc
  deriving (Show, Eq)

