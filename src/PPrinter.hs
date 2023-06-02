-- | Module to pretty-print Termina Programs
module PPrinter where

import Prelude hiding (id)

import AST
-- https://hackage.haskell.org/package/prettyprinter
import Prettyprinter
import Prettyprinter.Render.Terminal

import Parsing (Annotation)

import Data.Text (Text)

import PPrinter.Common
import PPrinter.TypeDef

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppEmptyDoc :: a -> Doc ann
ppEmptyDoc = const emptyDoc

ppHeaderASTElement :: Printer AnnASTElement a
ppHeaderASTElement before after (TypeDefinition t) = ppTypeDef before after t
ppHeaderASTElement _ _ _ = pretty "vaya"

ppHeaderFile :: AnnotatedProgram Annotation -> Text
ppHeaderFile = render . vsep . map (ppHeaderASTElement ppEmptyDoc ppEmptyDoc)

-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
