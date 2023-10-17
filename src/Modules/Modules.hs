-- | Module about Modules!

module Modules.Modules where

import AST.Core
import qualified AST.Parser as PAST
import Parser.Parsing (Annotation)
import System.Path
import Data.List

-- Containers
import qualified Data.Map.Strict as M

-- Imported name ++ "/src.fin"
stringToPath :: [String] -> Path Unrooted
stringToPath strs = fragments strs </> fragment "src" <.> FileExt "fin"

moduleStringToPath :: Module' [String] a -> Module' (Path Unrooted) a
moduleStringToPath = modulePath stringToPath

terminaFilePaths
  :: TerminaProgram' expr glb [String] a b
  -> [Path Unrooted]
terminaFilePaths
  = map ( fragments . moduleIdentifier ) . modules

terminaFile :: Path a -> Bool
terminaFile = isExtensionOf (FileExt "fin")

loadProject :: Monad m
  -- Project root directory
  => Path Absolute
  -- Map loading every file imported
  -> M.Map (Path Absolute) (PAST.TerminaProgram Annotation)
  -- Loading function
  -> (Path Absolute -> m (PAST.TerminaProgram Annotation))
  -- Modules to load
  -> [Path Unrooted]
  -> m (M.Map (Path Absolute) (PAST.TerminaProgram Annotation))
loadProject _dirProject fsLoaded _loadFile [] = return fsLoaded
loadProject dirProject fsLoaded loadFile (fs:fss) =
  let fsAbs = dirProject </> fs </> fragment "src" <.> FileExt "fin" in
  if M.member fsAbs fsLoaded
  -- Nothing to do, skip to the next one.
  then loadProject dirProject fsLoaded loadFile fss
  else do
    termina <- loadFile fsAbs
    loadProject dirProject (M.insert fsAbs termina fsLoaded) loadFile (fss ++ terminaFilePaths termina)

processProjectOrdered :: M.Map (Path Absolute) (PAST.TerminaProgram Annotation) -> [(Path Absolute, PAST.TerminaProgram Annotation)]
processProjectOrdered = M.toAscList
