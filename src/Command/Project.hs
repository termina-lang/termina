-- | Loading of the project before its deployments are built.
module Command.Project (
    Dependencies(..),
    loadProject
) where

import Configuration.ExternCheck (readExternSections, checkExternSections)
import Configuration.Namespace (checkExports)
import Command.Common
import Command.Dependencies (Fetching(..), Sync(..), syncDependencies, syncFrozen, describeChange)
import Command.Sources (LoadedProject(..), readProjectModules)
import qualified Data.Map.Lazy as ML
import System.FilePath (normalise)
import Command.Utils

import Control.Monad

-- | What a command does with the dependencies of the project.
data Dependencies =
    -- | Bring termina.lock and vendor/ up to date with the manifest
    Update Fetching
    -- | Take termina.lock and vendor/ as they are, without network, and stop
    -- when they do not match the manifest or a dependency is a local folder
  | Frozen

-- | Load the project for the commands that build.
-- It reads the manifest and the modules of the project, and checks the extern
-- sections and the exports of a library.
-- It brings termina.lock and vendor/ up to date with the manifest, or checks
-- that they are.
loadProject :: Dependencies -> IO LoadedProject
loadProject dependencies = do
    manifest <- loadManifest projectRoot >>= either dieWithFirstError return
    own <- readProjectModules projectRoot manifest
    -- | Read the modules that the extern sections name.
    -- A module that does not parse stops with the error of the parser.
    (sections, unparsed) <- readExternSections projectRoot own manifest
    forM_ unparsed $ \(qname, file) ->
        maybe (parseTerminaFile qname file) (parsedOrDie qname file) (ML.lookup (normalise file) own)
    unless (null (checkExternSections sections)) $ dieWithFirstError (checkExternSections sections)
    exportErrors <- checkExports projectRoot own manifest
    unless (null exportErrors) $ dieWithFirstError exportErrors
    lock <- loadLock projectRoot >>= either dieWithFirstError return
    place <- currentPlace
    Sync _ warnings packages locals <- case dependencies of
        Update fetching -> do
            synced <- syncDependencies place fetching True projectRoot manifest lock >>= either dieWithFirstError return
            mapM_ (putStrLn . infoMessage . ("termina.lock: " ++) . describeChange) (syncChanges synced)
            return synced
        Frozen -> syncFrozen place projectRoot manifest lock >>= either dieWithFirstError return
    mapM_ (putStrLn . warnMessage) warnings
    return (LoadedProject projectRoot manifest packages locals own)
