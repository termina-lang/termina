-- | Module to pretty-print Termina Programs

module PPrinter where

import Prelude hiding (id)

import AST.Seman

-- https://hackage.haskell.org/package/prettyprinter
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Text (Text, pack, intercalate, replace, toUpper)

import PPrinter.Common
import PPrinter.TypeDef.Declaration
import PPrinter.TypeDef.Definition
import Semantic.Monad (SemanticAnns)
import PPrinter.Function

import PPrinter.Global

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

ppEmptyDoc :: a -> Doc ann
ppEmptyDoc = const emptyDoc

ppHeaderASTElement :: AnnASTElement SemanticAnns -> Maybe DocStyle
ppHeaderASTElement (TypeDefinition t _) = Just (ppTypeDefDeclaration t)
ppHeaderASTElement (GlobalDeclaration obj@(Resource {})) = Just (ppGlobalDeclaration obj)
ppHeaderASTElement (GlobalDeclaration obj@(Task {})) = Just (ppGlobalDeclaration obj)
ppHeaderASTElement (GlobalDeclaration obj@(Handler {})) = Just (ppGlobalDeclaration obj)
ppHeaderASTElement func@(Function {}) = Just (ppFunctionDeclaration func)
ppHeaderASTElement _ = Nothing

ppSourceASTElement :: AnnASTElement SemanticAnns -> Maybe DocStyle
ppSourceASTElement (TypeDefinition (Struct {}) _) = Nothing
ppSourceASTElement (TypeDefinition (Enum {}) _) = Nothing
ppSourceASTElement (TypeDefinition cls@(Class {}) _) = Just (ppClassDefinition cls)
ppSourceASTElement (GlobalDeclaration obj@(Resource {})) = Just (ppGlobalDefinition obj)
ppSourceASTElement (GlobalDeclaration obj@(Task {})) = Just (ppGlobalDefinition obj)
ppSourceASTElement (GlobalDeclaration obj@(Handler {})) = Just (ppGlobalDefinition obj)
ppSourceASTElement func@(Function {}) = Just (ppFunction func)
ppSourceASTElement _ = Nothing

-- | Pretty print the header file identifier macro
-- TODO: Whatever character that is not a letter or a digit should be replaced by an underscore
ppHeaderFileDefine :: [Text] -> DocStyle
ppHeaderFileDefine filePath = 
    let qualifiedPath = intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) in
    pretty "__" <> pretty qualifiedPath <> pretty "_H__"


-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
