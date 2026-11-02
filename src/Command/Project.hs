{-# LANGUAGE OverloadedStrings #-}

-- | The project of a command that builds, loaded and checked before any of
-- its deployments.
module Command.Project (
    loadProject
) where

import Configuration.Configuration
import Configuration.ExternCheck (readExternSections, checkExternSections, checkExternModules)
import Configuration.Manifest
import Configuration.Namespace (checkExports, exportedModules)
import Configuration.Platform
import Command.Common
import Command.Dependencies (Fetching(..), Sync(..), syncDependencies, describeChange)
import Command.Extern (parsedExterns)
import Command.Sources (LoadedProject(..), buildSources, readProjectModules)
import qualified Data.Map.Lazy as ML
import System.FilePath (normalise)
import Command.Utils
import Generator.Environment
import Modules.Modules
import Semantic.Environment
import Utils.Annotations

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Load the project for a command that builds. It checks the manifest, the
-- folders, the extern sections and the prefixes of a library, updates the lock
-- and vendor/ from the manifest, validates the libraries of local folders and,
-- in a library, type checks the exported modules. A copy of vendor/ that does
-- not change is checked against its hash. Offline, nothing is fetched.
loadProject :: Bool -> Bool -> IO LoadedProject
loadProject chatty offline = do
    manifest <- loadManifest >>= either dieWithFirstError return
    own <- readProjectModules manifest
    -- | The extern sections, reading only the modules that their entries
    -- name; one of them that does not parse stops with the report of the parser
    (sections, unparsed) <- readExternSections "." own manifest
    forM_ unparsed $ \(qname, file) ->
        maybe (parseTerminaFile qname file) (parsedOrDie qname file) (ML.lookup (normalise file) own)
    unless (null (checkExternSections sections)) $ dieWithFirstError (checkExternSections sections)
    exportErrors <- checkExports "." own manifest
    unless (null exportErrors) $ dieWithFirstError exportErrors
    lock <- loadLock "." >>= either dieWithFirstError return
    place <- currentPlace
    Sync changes warnings packages locals parsed <- syncDependencies place "termina build" (if offline then Offline else Reuse) True "." manifest lock
        >>= either dieWithFirstError return
    mapM_ (putStrLn . warnMessage) warnings
    mapM_ (putStrLn . infoMessage . ("termina.lock: " ++) . describeChange) changes
    let project = LoadedProject manifest packages locals (ML.union own parsed)
    verifyLibrary chatty project
    return project

-- | Type check the exported modules of a library together, with the modules
-- they import, on each platform of its tests, or on the test platform when it
-- has none. Every exported module is checked, whether a test imports it or
-- not, and their names must not clash. The modules are checked as those of a
-- test, so a module that is not exported may hold a test class.
verifyLibrary :: Bool -> LoadedProject -> IO ()
verifyLibrary chatty project =
    case manifestNature manifest of
        Application _ -> return ()
        Library _ -> do
            roots <- exportedModules "." manifest
            parsedModules <- loadModules (loadedParsed project) (buildSources project Nothing "src") "src" [ModuleDependency root Internal | root <- roots]
            -- | Each exported module with extern functions has its entry
            case checkExternModules "the project" "src" (manifestExtern manifest) Nothing (map parsedExterns (M.elems parsedModules)) of
                [] -> return ()
                errs -> dieWithFirstError errs
            orderedModules <- either
                (\files -> dieWithError $ "detected cycle between the exported modules of the library: " ++ show files)
                return
                (sortProjectDepsOrLoop (M.map importedModules parsedModules))
            forM_ libraryPlatforms $ \plt -> do
                when chatty (putStrLn . debugMessage $ "Type checking the exported modules on " ++ show plt)
                let config = (defaultConfig (T.unpack (projectName (manifestProject manifest))) plt) {
                        sysPrintOutputBufferSize = manifestPrintBufSize manifest,
                        sysReadInputBufferSize = manifestReadBufSize manifest,
                        deploymentKind = TestBuild "library"
                      }
                void (typeModules parsedModules (makeInitialGlobalEnv (Just config) plt (getPlatformInitialGlobalEnv config plt)) orderedModules)

  where

    manifest = loadedManifest project

    libraryPlatforms =
        case foldr (\plt acc -> if plt `elem` acc then acc else plt : acc) [] (concatMap testPlatforms (M.elems (manifestTests manifest))) of
            [] -> [TestPlatform]
            plts -> plts
