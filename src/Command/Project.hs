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
import Command.Sources (loadSources)
import Command.Utils
import Generator.Environment
import Modules.Modules
import Semantic.Environment
import Utils.Annotations

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Load the project for a command that builds: the manifest, the folders, the
-- extern sections, the prefixes of a library, the lock and vendor/ brought in
-- line with the manifest (each copy that does not change checked against its
-- hash; offline, without fetching anything) and, in a library, the types of
-- its exported modules.
loadProject :: Bool -> Bool -> IO Manifest
loadProject chatty offline = do
    manifest <- loadManifest >>= either dieWithFirstError return
    -- | The extern sections, reading only the modules that their entries
    -- name; one of them that does not parse stops with the report of the parser
    (sections, unparsed) <- readExternSections "." manifest
    forM_ unparsed $ uncurry parseTerminaFile
    unless (null (checkExternSections sections)) $ dieWithFirstError (checkExternSections sections)
    exportErrors <- checkExports "." manifest
    unless (null exportErrors) $ dieWithFirstError exportErrors
    lock <- loadLock "." >>= either dieWithFirstError return
    place <- currentPlace
    Sync changes warnings <- syncDependencies place "termina build" (if offline then Offline else Reuse) True "." manifest lock
        >>= either dieWithFirstError return
    mapM_ (putStrLn . warnMessage) warnings
    mapM_ (putStrLn . infoMessage . ("termina.lock: " ++) . describeChange) changes
    verifyLibrary chatty manifest
    return manifest

-- | Type check the exported modules of a library together, with the modules
-- they import, on each platform of its tests, or on the test platform when it
-- has none. Every exported module is checked, whether a test imports it or
-- not, and their names must not clash. The modules are checked as those of a
-- test, so a module that is not exported may hold a test class.
verifyLibrary :: Bool -> Manifest -> IO ()
verifyLibrary chatty manifest =
    case manifestNature manifest of
        Application _ -> return ()
        Library _ -> do
            roots <- exportedModules "." manifest
            sources <- loadSources manifest Nothing "src"
            parsedModules <- loadModules sources "src" [ModuleDependency root Internal | root <- roots]
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

    libraryPlatforms =
        case foldr (\plt acc -> if plt `elem` acc then acc else plt : acc) [] (concatMap testPlatforms (M.elems (manifestTests manifest))) of
            [] -> [TestPlatform]
            plts -> plts
