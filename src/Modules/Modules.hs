-- | Module about Modules!

module Modules.Modules where

import AST.Core
import qualified AST.Parser as PAST
import Parser.Parsing (Annotation)
import System.Path
import Extras.TopSort


-- Containers
import qualified Data.Map.Strict as M

-- /Folder1/Folder2/../ModName
type ModuleName = Path Unrooted
-- AbsProjectRoute/Folder1/Folder2/.../ModName/src.fin
type ModuleSrc = Path Absolute
type ProjectDir = Path Absolute


data ModuleData = MData
  { moduleName :: ModuleName
  , moduleSrc :: ModuleSrc
  }


-- Ways of modules
data ModuleMode = DirMod | SrcFile

isMDir :: ModuleMode -> Bool
isMDir DirMod = True
isMDir _ = False
--

isTerminaFile :: Path a -> Bool
isTerminaFile = (Just terminaExt ==)  . takeExtension

terminaExt :: FileExt
terminaExt = FileExt "fin"

-- Let [projDir] be the route to the "project" and [nm]  a module name.
-- [moduleSrcFromName projDir nm] = "projDir/nm/src.fin"
-- moduleSrcFromName :: ProjectDir -> ModuleName -> [ ModuleSrc ]
-- moduleSrcFromName dirProject mName = dirProject </> mName <.> terminaExt

-- moduleStringToPath
--   :: ProjectDir -> Module' [String] a -> Module' ModuleData a
-- moduleStringToPath pDir
--   = modulePath (\strs -> let nm = fragments strs in MData nm (moduleSrcFromName pDir nm))

buildModuleName :: [String] -> ModuleName
buildModuleName = fragments

terminaProgramImports :: PAST.TerminaProgram Annotation -> [ ModuleName ]
terminaProgramImports = map ( fragments . moduleIdentifier) . modules

loadProject
  :: Monad m
  -- ModuleName
  => (ModuleName -> m (ModuleMode,PAST.TerminaProgram Annotation))
  -> [ModuleName]
  -> m (M.Map ModuleName ([ModuleName], ModuleMode, PAST.TerminaProgram Annotation))
loadProject = loadProject' M.empty

loadProject' :: Monad m
  -- Map loading every file imported
  => M.Map ModuleName ([ModuleName], ModuleMode,PAST.TerminaProgram Annotation)
  -- Loading function
  -> (ModuleName -> m (ModuleMode,PAST.TerminaProgram Annotation))
  -- Modules to load
  -> [ModuleName]
  -> m (M.Map ModuleName ([ModuleName],ModuleMode, PAST.TerminaProgram Annotation))
loadProject' fsLoaded _loadFile [] = return fsLoaded
loadProject' fsLoaded loadFile (fs:fss) =
  if M.member fs fsLoaded
  -- Nothing to do, skip to the next one. It could be the case of a module
  -- imported from several files.
  then loadProject' fsLoaded loadFile fss
  -- Import and load it.
  else do
    (tMode, terminaProg) <- loadFile fs
    let deps = terminaProgramImports terminaProg
    loadProject'
      (M.insert fs (deps,tMode, terminaProg) fsLoaded)
      loadFile
      (fss ++ deps)

-- | Return the list of module in ascending order...
-- This may not be the correct order. We can detect a cycle before hand or just
-- try and detect it on the fly.
processProject :: M.Map (Path Absolute) (PAST.TerminaProgram Annotation)
  -> [(Path Absolute, PAST.TerminaProgram Annotation)]
processProject = M.toAscList

-- | Given a map, it returns each path with its dependencies.
-- This is what we need in case we want to detect cycles before hand.
-- Detecting them before hand is better if typing and everyting is slow.
processProjectDeps
  :: M.Map ModuleName [ModuleName]
  -> [(ModuleName, [ModuleName])]
processProjectDeps
  = M.toList

-- | Given project dir and map, returns either a detected loop or a way to load
-- them in order.
sortOrLoop
  :: M.Map ModuleName [ModuleName]
  -> Either [ModuleName] [ModuleName]
sortOrLoop = topSortFromDepList . processProjectDeps
