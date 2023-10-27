-- | Semantic Error Printing

module Semantic.Errors.PPrinting where

import Semantic.Errors.Errors

import Prettyprinter
-- import PPrinter.Common (DocStyle)
import Text.Parsec

import Parser.Parsing (Annotation(..))

-- useful parsec doc
-- https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html#g:4
fileNameLineCol :: SourcePos -> Doc a
fileNameLineCol e =
  -- interspace everything with colon (":")
  concatWith (\x y -> x<>colon<>y)
                -- Pretty the source name
                [ pretty $ sourceName e
                -- Using Show pretty the line
                , viaShow $ sourceLine e
                -- Using Show pretty the column
                , viaShow $ sourceColumn e ]

fileNLC :: Annotation -> Doc a
fileNLC (Position spos) = fileNameLineCol spos
fileNLC Internal = pretty "Internal Error!! Throw the computer out of the buliding."

-- useful prettyprinter doc
-- https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html
ppError :: SemanticErrors -> Doc a
ppError (AnnError e detectedPos) =
  case e of
    (EUsedFunName funName parsedPos) ->
        vsep
        [ pretty "Error when defining function" <+> pretty funName
        , pretty "At file" <+> fileNLC detectedPos
        , pretty "Defined" <+> fileNLC parsedPos
        ]
    _ -> vsep
         [ pretty "An Error ocurred, we are going on a beautyful printer, be patient."
         , pretty "See:" <> fileNLC detectedPos
         , pretty "Informally:" <+> viaShow e
         ]
