-- | Module Printing Some Module stuff

module Modules.Printing where

import Modules.Modules
import Modules.Errors

import System.Path

import Data.Text hiding (group, intercalate,map)
import Prettyprinter
import AST.Seman (AnnotatedProgram)
import Semantic.Monad (SemanticAnns)
import Data.Maybe (mapMaybe)

import PPrinter
import PPrinter.Common (DocStyle)

import Semantic.Errors (ppError)

ppModuleName :: ModuleName -> Doc a
ppModuleName = pretty . toUnrootedFilePath
-- ppModuleName = pretty . intercalate "." . toUnrootedFilePath

-- Function to use |ppHeaderFileDefine|
moduleNameToText :: ModuleName -> [Text]
moduleNameToText = map (pack . toUnrootedFilePath) . splitFragments

includes :: [(ModuleName, ModuleMode)] -> Doc a
includes [] = emptyDoc
includes deps = vsep $ emptyDoc : map (
  \(nm, mm) -> 
    case mm of
      DirMod -> pinclude <+> dquotes(ppModuleName (nm </> fragment "header" <.> FileExt "h"))
      SrcFile -> pinclude <+> dquotes(ppModuleName (nm <.> FileExt "h"))
  ) deps ++ [emptyDoc]
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
    [ pretty "#ifndef" <+> modName
    , pretty "#define" <+> modName
    , emptyDoc
    ]
  , pretty "#include <termina.h>"
  , imports
  , vsep (mapMaybe ppHeaderASTElement program
               ++ [ pretty "#endif //" <+> modName, emptyDoc])
  ]

ppSourceFile
  -- ModName
  :: DocStyle
  -- Typed Termina Program
  -> AnnotatedProgram SemanticAnns
  -- Resulting PP program
  -> Text
ppSourceFile modName program = render $ vsep $
        [emptyDoc,
        pretty "#include" <+> dquotes(modName <> pretty ".h"),
        emptyDoc
        ] ++ mapMaybe ppSourceASTElement program

-- Error Printing

ppModError :: Errors -> Doc a
ppModError (ELiftTypeCheckError e) = ppError e
ppModError e = pretty "Other errors during module stuff" <+> viaShow e
