-- | Module Printing Some Module stuff

module Modules.Printing where

import Modules.Modules
import Modules.Errors

import System.Path

import Data.Text hiding (group, intercalate,map)
import Prettyprinter
import AST.Seman (AnnotatedProgram)
import Semantic.Monad (SemanticAnns)
import Semantic.Option (OptionMap)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import PPrinter
import PPrinter.Common (DocStyle)

import Semantic.Errors (ppError)

ppModuleName :: ModuleName -> ModuleMode -> Doc a
ppModuleName mn DirMod = pretty $ toUnrootedFilePath (mn </> fragment "header")
ppModuleName mn SrcFile = pretty $ toUnrootedFilePath mn
-- ppModuleName = pretty . intercalate "." . toUnrootedFilePath

-- Function to use |ppHeaderFileDefine|
moduleNameToText :: ModuleName -> ModuleMode -> [Text]
moduleNameToText mn DirMod = map (pack . toUnrootedFilePath) (splitFragments (mn </> fragment "header"))
moduleNameToText mn SrcFile = map (pack . toUnrootedFilePath) (splitFragments mn)

includes :: [(ModuleName, ModuleMode)] -> DocStyle
includes [] = emptyDoc
includes deps = vsep $ emptyDoc : map (
  \(nm, mm) -> includeC <+> dquotes(headerPath nm mm)) deps ++ [emptyDoc]
 where
    includeC :: DocStyle
    includeC = pretty "#include"

    headerPath :: ModuleName -> ModuleMode -> DocStyle
    headerPath mName DirMod = pretty $ toUnrootedFilePath (mName </> fragment "header" <.> FileExt "h")
    headerPath mName SrcFile =  pretty $ toUnrootedFilePath (mName <.> FileExt "h")

ppHeaderFile
  :: Bool
  -> OptionMap
  -- Module Name
  -> DocStyle
  -- Import list
  -> DocStyle
  -- Typed Program
  -> AnnotatedProgram SemanticAnns -> Text
ppHeaderFile includeOptionH opts modName imports program = render $
  vsep $
  [
    pretty "#ifndef" <+> modName
    , pretty "#define" <+> modName
    , emptyDoc
    , pretty "#include <termina.h>"
  ] ++
  (if not includeOptionH then [] else 
    [
      emptyDoc, pretty "#include \"option.h\""
    ]) ++
  [ imports
  , vsep (mapMaybe (ppHeaderASTElement opts) program
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
