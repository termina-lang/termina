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
import Data.Maybe
import PPrinter.Task

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppEmptyDoc :: a -> Doc ann
ppEmptyDoc = const emptyDoc

ppHeaderASTElement :: AnnASTElement SemanticAnns -> Maybe DocStyle
ppHeaderASTElement (TypeDefinition t _) = Just (ppTypeDefDeclaration t)
ppHeaderASTElement func@(Function {}) = Just (ppFunctionDeclaration func)
ppHeaderASTElement task@(Task {}) = Just (ppTaskDeclaration task)
ppHeaderASTElement _ = Nothing

ppSourceASTElement :: AnnASTElement SemanticAnns -> Maybe DocStyle
ppSourceASTElement (TypeDefinition (Struct {}) _) = Nothing
ppSourceASTElement (TypeDefinition (Enum {}) _) = Nothing
ppSourceASTElement (TypeDefinition (Class {}) _) = Nothing
ppSourceASTElement func@(Function {}) = Just (ppFunction func)
ppSourceASTElement task@(Task {}) = Just (ppTask task)
ppSourceASTElement _ = Nothing

ppHeaderFile :: AnnotatedProgram SemanticAnns -> Text
-- Print only the elements that are not nothing
ppHeaderFile = render . vsep . mapMaybe ppHeaderASTElement

ppSourceFile :: AnnotatedProgram SemanticAnns -> Text
ppSourceFile = render . vsep . mapMaybe ppSourceASTElement

-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
