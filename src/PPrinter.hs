-- | Module to pretty-print Termina Programs

module PPrinter where

import Prelude hiding (id)

import SemanAST
-- https://hackage.haskell.org/package/prettyprinter
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Text (Text)

import PPrinter.Common
import PPrinter.TypeDef
import Semantic.Monad (SemanticAnns)
import PPrinter.Function

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppEmptyDoc :: a -> Doc ann
ppEmptyDoc = const emptyDoc

ppHeaderASTElement :: AnnASTElement SemanticAnns -> DocStyle
ppHeaderASTElement (TypeDefinition t _) = ppTypeDefDeclaration t
ppHeaderASTElement func@(Function {}) = ppFunctionDeclaration func
ppHeaderASTElement _ = pretty "unsupported"

ppSourceASTElement :: AnnASTElement SemanticAnns -> DocStyle
ppSourceASTElement func@(Function {}) = ppFunction func
ppSourceASTElement _ = pretty "unsupported"

ppHeaderFile :: AnnotatedProgram SemanticAnns -> Text
ppHeaderFile = render . vsep . map ppHeaderASTElement

ppSourceFile :: AnnotatedProgram SemanticAnns -> Text
ppSourceFile = render . vsep . map ppSourceASTElement

-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
