-- | Module to pretty-print Termina Programs

module PPrinter where

import Prelude hiding (id)

import AST.Seman

-- https://hackage.haskell.org/package/prettyprinter
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Text (Text)

import PPrinter.Common
import PPrinter.TypeDef
import Semantic.Monad (SemanticAnns)
import PPrinter.Function
import Data.Maybe
import PPrinter.Global

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppEmptyDoc :: a -> Doc ann
ppEmptyDoc = const emptyDoc

ppHeaderASTElement :: AnnASTElement SemanticAnns -> Maybe DocStyle
ppHeaderASTElement (TypeDefinition t _) = Just (ppTypeDefDeclaration t)
ppHeaderASTElement (GlobalDeclaration obj@(Resource {})) = Just (ppGlobalDeclaration obj)
ppHeaderASTElement func@(Function {}) = Just (ppFunctionDeclaration func)
ppHeaderASTElement _ = Nothing

ppSourceASTElement :: AnnASTElement SemanticAnns -> Maybe DocStyle
ppSourceASTElement (TypeDefinition (Struct {}) _) = Nothing
ppSourceASTElement (TypeDefinition (Enum {}) _) = Nothing
ppSourceASTElement (TypeDefinition cls@(Class {}) _) = Just (ppClassDefinition cls)
ppSourceASTElement func@(Function {}) = Just (ppFunction func)
ppSourceASTElement _ = Nothing

ppHeaderFile :: AnnotatedProgram SemanticAnns -> Text
-- Print only the elements that are not nothing
ppHeaderFile = render . vsep . mapMaybe ppHeaderASTElement

ppSourceFile :: AnnotatedProgram SemanticAnns -> Text
ppSourceFile = render . vsep . mapMaybe ppSourceASTElement

-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
