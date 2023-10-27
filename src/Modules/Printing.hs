-- | Module Printing Some Module stuff

module Modules.Printing where

import Modules.Modules
import Modules.Errors

import Data.List (intercalate)
import System.Path

import Data.Text hiding (group, intercalate,map)
import Prettyprinter
import Prettyprinter.Render.Terminal
import AST.Seman (AnnotatedProgram(..))
import Semantic.Monad (SemanticAnns)
import Data.Maybe (mapMaybe)

import PPrinter hiding (ppHeaderFile)
import PPrinter.Common (DocStyle)

import Semantic.Errors (ppError)

ppModuleName :: ModuleName -> Doc a
ppModuleName = pretty . toUnrootedFilePath
-- ppModuleName = pretty . intercalate "." . toUnrootedFilePath

-- Function to use |ppHeaderFileDefine|
moduleNameToText :: ModuleName -> [Text]
moduleNameToText = map (pack . toUnrootedFilePath) . splitFragments

includeMod :: (ModuleName, ModuleMode) -> Doc a
includeMod (nm,DirMod) = ppModuleName nm <> pretty "/src"
includeMod (nm,SrcFile) = ppModuleName nm

includes :: [(ModuleName, ModuleMode)] -> Doc a
includes = vsep . map (\m -> pinclude <+> dquotes(includeMod m <> pretty ".h"))
 where
   pinclude = pretty "#include"

ppHeaderFile
  -- Module Name
  :: DocStyle
  -- Import list
  -> DocStyle
  -- Typed Program
  -> AnnotatedProgram SemanticAnns -> Text
ppHeaderFile modName imports program = render $
  vsep
  [ vsep
    [ pretty "#ifndef " <> modName
    , pretty "#define " <> modName
    , line
    ]
  , pretty "#include <termina.h>"
  , line
  , imports
  , vsep (mapMaybe ppHeaderASTElement program
               ++ [ pretty "#endif // " <> modName
                  , line])
  ]

ppSourceFile
  -- ModName
  :: DocStyle
  -- Typed Termina Program
  -> AnnotatedProgram SemanticAnns
  -- Resulting PP program
  -> Text
ppSourceFile modName program = render $ vsep $
        [line,
        pretty "#include " <+> dquotes(modName <> pretty ".h"),
        line
        ] ++ mapMaybe ppSourceASTElement program

-- Error Printing

ppModError :: Errors -> Doc a
ppModError (ELiftTypeCheckError e) = ppError e
ppModError e = pretty "Other errors during module stuff" <+> viaShow e
