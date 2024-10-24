module Parser.Types where

import Utils.Annotations

{- | Type of the parsing annotations
This type is used to identify the annotations made on the different
elements of the AST. In this case, the annotations will only include
the position in the source file where the element is located.
-}

type ParserAnn = TLocation

