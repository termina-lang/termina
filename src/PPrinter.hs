-- | Module to pretty-print Termina Programs

module PPrinter where

import Prelude hiding (id)

import AST.Seman

-- https://hackage.haskell.org/package/prettyprinter
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Text (Text, pack, intercalate, replace, toUpper)

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

-- | Pretty print the header file identifier macro
-- TODO: Whatever character that is not a letter or a digit should be replaced by an underscore
ppHeaderFileDefine :: [Text] -> DocStyle
ppHeaderFileDefine filePath = 
    let qualifiedPath = intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) in
    pretty "__" <> pretty qualifiedPath <> pretty "_H__"

ppHeaderFile :: [Text] -> [[Text]] -> AnnotatedProgram SemanticAnns -> Text
-- Print only the elements that are not nothing
ppHeaderFile filePath imports program = render (
    vsep $
        [
            pretty "#ifndef " <> ppHeaderFileDefine filePath,
            pretty "#define " <> ppHeaderFileDefine filePath,
            emptyDoc,
            pretty "#include <termina.h>"
        ] ++ 
        map (\i -> pretty "#include \"" <> pretty (intercalate (pack "/") i) <> pretty ".h\"") imports ++
        emptyDoc :
        mapMaybe ppHeaderASTElement program ++
        [   
            pretty "#endif // " <> ppHeaderFileDefine filePath,
            emptyDoc
        ]
    )

ppSourceFile :: [Text] -> AnnotatedProgram SemanticAnns -> Text
ppSourceFile filePath program = render (
        vsep $
        [
            emptyDoc,
            pretty "#include \"" <> pretty (intercalate (pack "/") filePath) <> pretty ".h\"",
            emptyDoc
        ] ++
        mapMaybe ppSourceASTElement program
    )

-- ppProgramDebug :: AnnotatedProgram Annotation -> Text
-- ppAnnonProgram = render . vsep . map (ppAnnAST (pretty . show))
