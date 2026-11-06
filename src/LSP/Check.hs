-- | The project of the language server, read and checked as the build reads
-- and checks it, with the text of the files open in the editor.
module LSP.Check (
    openProject, checkProject, reloadFile
) where

import Command.Common (libraryPlatforms, loadExportedModules, loadMainModule, orderedModules, sourceTexts, typeLibrary)
import Command.Pipeline (checkModules)
import Analysis.SMTBounds (noSMTProofs)
import Data.Functor.Identity (runIdentity)
import Command.Sources (LoadedProject(..), buildSources, dependencyModulesOf, readProject, withTexts)
import Command.Types (ParsedProject, TypedProject, SemanticData(typedAST))
import Command.Utils (CheckFailure(..), findProjectRoot, loadManifest)
import Configuration.Manifest
import LSP.Index (emptyIndex, indexModule)
import LSP.Logging (errorM)
import LSP.Modules (TerminaStoredModule(..))
import LSP.Monad
import LSP.Utils (emitDiagnostics)
import qualified Modules.Modules as Mod
import Utils.Annotations (Location(Position))

import Control.Exception (IOException, try)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server (getVirtualFiles)
import Language.LSP.VFS (vfsMap, virtualFileText)
import System.Directory (canonicalizePath, getCurrentDirectory, setCurrentDirectory)
import System.FilePath
import Text.Parsec.Pos (sourceName)

-- | Read the project of the folder where the server runs, or of the nearest
-- folder above it with a termina.yaml, without network, and keep it in the
-- state. When there is no project, or its manifest or its dependencies have
-- an error, it is shown to the user and the server checks nothing.
openProject :: HandlerM ()
openProject = do
    found <- liftIO findProjectRoot
    case found of
        Left err -> errorM (T.pack err)
        Right folder -> do
            root <- liftIO (setCurrentDirectory folder >> getCurrentDirectory >>= canonicalizePath)
            loadProjectAt root

-- | Read the project of a folder and keep it in the state.
loadProjectAt :: FilePath -> HandlerM ()
loadProjectAt root = do
    manifestRead <- liftIO (loadManifest root)
    case manifestRead of
        Left errs -> errorM (T.pack (concat (take 1 errs)))
        Right manifest -> do
            projectRead <- liftIO (readProject root manifest)
            case projectRead of
                Left errs -> errorM (T.pack (concat (take 1 errs)))
                Right project -> modify (\s -> s { loaded = Just project })

-- | Check the project with the text of the files open in the editor: the
-- application, or the exported modules of a library. The modules of the check
-- replace those of the state, and its error, if there is one, is published in
-- the file where it is. A module that cannot be loaded leaves the modules of
-- the state as they were.
checkProject :: HandlerM ()
checkProject = do
    mProject <- gets loaded
    texts <- editorTexts
    forM_ (withTexts texts <$> mProject) $ \project ->
        maybe (checkLibrary project) (checkApplication project) (appDeployment (loadedManifest project))

-- | Check the application of a project as the build does.
checkApplication :: LoadedProject -> Deployment -> HandlerM ()
checkApplication project deployment = do
    let mainFolder = normalise (loadedRoot project </> deploymentMainFolder deployment)
        mainFile = mainFolder </> deploymentMainModule deployment <.> "fin"
        sources = buildSources project Nothing mainFolder
        files = sourceTexts sources
    loadedMain <- liftIO (loadMainModule sources mainFolder (deploymentMainModule deployment))
    case loadedMain >>= \(_, parsedProject) -> (,) parsedProject <$> orderedModules parsedProject of
        Left failure -> publishDiagnostics (failureDiagnosticsIn mainFile files failure)
        Right (parsedProject, order) -> do
            -- | The SMT prover does not run in the editor
            let (typedProject, result) = runIdentity $
                    checkModules noSMTProofs (deploymentConfig deployment) (deploymentPlatform deployment) (dependencyModulesOf sources) parsedProject order
            modify (\s -> s { project_modules = keepIndexes (project_modules s) (storedModules parsedProject typedProject) })
            publishDiagnostics (either (failureDiagnosticsIn mainFile files) (const M.empty) result)

-- | Type check the exported modules of a library on the first platform on
-- which the build checks them.
checkLibrary :: LoadedProject -> HandlerM ()
checkLibrary project = do
    let manifest = loadedManifest project
        root = loadedRoot project
        manifestFile = root </> "termina.yaml"
        sources = buildSources project Nothing (normalise (root </> "src"))
        files = sourceTexts sources
    loadedLibrary <- liftIO (loadExportedModules project)
    case (loadedLibrary, libraryPlatforms manifest) of
        (Left failure, _) -> publishDiagnostics (failureDiagnosticsIn manifestFile files failure)
        (Right _, []) -> return ()
        (Right (parsedProject, order), plt : _) -> do
            let (typedProject, failure) = typeLibrary manifest plt (dependencyModulesOf sources) parsedProject order
            modify (\s -> s { project_modules = keepIndexes (project_modules s) (storedModules parsedProject typedProject) })
            publishDiagnostics (maybe M.empty (failureDiagnosticsIn manifestFile files) failure)

-- | Read again from disk a file that the editor has saved or closed, so that
-- the project keeps its text once the editor no longer holds it. A file that
-- cannot be read or is not valid UTF-8 is left as it was.
reloadFile :: FilePath -> HandlerM ()
reloadFile path = do
    bytes <- liftIO (try (BS.readFile path)) :: HandlerM (Either IOException BS.ByteString)
    case bytes of
        Left _ -> return ()
        Right content -> case TE.decodeUtf8' content of
            Left _ -> return ()
            Right text -> modify (\s -> s { loaded = withTexts (M.singleton (normalise path) text) <$> loaded s })

-- | The text of the files open in the editor, by their path.
editorTexts :: HandlerM (M.Map FilePath T.Text)
editorTexts = do
    vfs <- getVirtualFiles
    return $ M.fromList
        [ (normalise path, virtualFileText file)
        | (uri, file) <- M.toList (vfs ^. vfsMap)
        , Just path <- [LSP.uriToFilePath (LSP.fromNormalizedUri uri)] ]

-- | The diagnostics of a failure, in the file where it is, or in the main
-- module when it has no place in the source.
failureDiagnosticsIn :: FilePath -> M.Map FilePath T.Text -> CheckFailure -> M.Map FilePath [LSP.Diagnostic]
failureDiagnosticsIn mainFile files failure =
    M.singleton file (failureDiagnostics failure files)

  where

    file = case failureLocation failure of
        Position _ start _ -> sourceName start
        _ -> mainFile

-- | Publish the diagnostics of each file, and clear those of the files that
-- had diagnostics and have none now.
publishDiagnostics :: M.Map FilePath [LSP.Diagnostic] -> HandlerM ()
publishDiagnostics diags = do
    before <- gets published
    forM_ (S.toList (S.union before (M.keysSet diags))) $ \file ->
        emitDiagnostics file (M.findWithDefault [] file diags)
    modify (\s -> s { published = M.keysSet diags })

-- | The modules of a check, with those that the check has not type checked
-- keeping the types and the index of the last check that gave them, so that the
-- editor can still navigate a module while it has an error.
keepIndexes :: M.Map FilePath TerminaStoredModule -> M.Map FilePath TerminaStoredModule -> M.Map FilePath TerminaStoredModule
keepIndexes previous = M.mapWithKey keep

  where

    keep file stored =
        case (semantic stored, M.lookup file previous) of
            (Nothing, Just old) -> stored { semantic = semantic old, moduleIndex = moduleIndex old }
            _ -> stored

-- | The modules of a check, by the path of their file, each with its types and
-- its index when it has been type checked.
storedModules :: ParsedProject -> TypedProject -> M.Map FilePath TerminaStoredModule
storedModules parsedProject typedProject = M.fromList
    [ (Mod.fullPath m, TerminaStoredModule (Mod.sourcecode m)
        (Mod.metadata <$> typed) (maybe emptyIndex (indexModule . typedAST . Mod.metadata) typed))
    | m <- M.elems parsedProject
    , let typed = M.lookup (Mod.qualifiedName m) typedProject ]
