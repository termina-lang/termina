-- | Loading of the project before its deployments are built.
module Command.Project (
    loadProject
) where

import Configuration.ExternCheck (readExternSections, checkExternSections)
import Configuration.Namespace (checkExports)
import Command.Common
import Command.Dependencies (Fetching(..), Sync(..), syncDependencies, describeChange)
import Command.Sources (LoadedProject(..), readProjectModules)
import qualified Data.Map.Lazy as ML
import System.FilePath (normalise)
import Command.Utils

import Control.Monad

-- | Load the project for the commands that build.
-- It reads the manifest and the modules of the project, and checks the extern
-- sections and the exports of a library.
-- It updates termina.lock and vendor/ from the manifest.
-- When offline is set, it does not fetch the dependencies.
loadProject :: Bool -> IO LoadedProject
loadProject offline = do
    manifest <- loadManifest >>= either dieWithFirstError return
    own <- readProjectModules "." manifest
    -- | Read the modules that the extern sections name.
    -- A module that does not parse stops with the error of the parser.
    (sections, unparsed) <- readExternSections "." own manifest
    forM_ unparsed $ \(qname, file) ->
        maybe (parseTerminaFile qname file) (parsedOrDie qname file) (ML.lookup (normalise file) own)
    unless (null (checkExternSections sections)) $ dieWithFirstError (checkExternSections sections)
    exportErrors <- checkExports "." own manifest
    unless (null exportErrors) $ dieWithFirstError exportErrors
    lock <- loadLock "." >>= either dieWithFirstError return
    place <- currentPlace
    Sync changes warnings packages locals <- syncDependencies place "termina build" (if offline then Offline else Reuse) True "." manifest lock
        >>= either dieWithFirstError return
    mapM_ (putStrLn . warnMessage) warnings
    mapM_ (putStrLn . infoMessage . ("termina.lock: " ++) . describeChange) changes
    return (LoadedProject "." manifest packages locals own)
