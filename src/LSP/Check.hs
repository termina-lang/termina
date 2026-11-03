-- | The project of the language server, read and checked as the build reads
-- and checks it, with the text of the files open in the editor.
module LSP.Check (
    openProject, checkProject
) where

import Command.Common (libraryPlatforms, loadExportedModules, loadMainModule, orderedModules, sourceTexts, typeLibrary)
import Command.Pipeline (checkModules)
import Command.Sources (LoadedProject(..), buildSources, readProject, withTexts)
import Command.Types (ParsedProject, TypedProject, SemanticData(typedAST))
import Command.Utils (CheckFailure(..), loadManifest)
import Configuration.Manifest
import LSP.Index (emptyIndex, indexModule)
import LSP.Logging (errorM)
import LSP.Modules (TerminaStoredModule(TerminaStoredModule))
import LSP.Monad
import LSP.Utils (emitDiagnostics)
import qualified Modules.Modules as Mod
import Utils.Annotations (Location(Position))

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server (getVirtualFiles)
import Language.LSP.VFS (vfsMap, virtualFileText)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.FilePath
import Text.Parsec.Pos (sourceName)

-- | Read the project of the folder where the server runs, without network,
-- and keep it in the state. An error of the manifest or of the dependencies is
-- shown to the user.
openProject :: HandlerM ()
openProject = do
    root <- liftIO (getCurrentDirectory >>= canonicalizePath)
    manifestRead <- liftIO loadManifest
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
            let (typedProject, result) =
                    checkModules (deploymentConfig deployment) (deploymentPlatform deployment) parsedProject order
            modify (\s -> s { project_modules = storedModules parsedProject typedProject })
            publishDiagnostics (either (failureDiagnosticsIn mainFile files) (const M.empty) result)

-- | Type check the exported modules of a library on the first platform on
-- which the build checks them.
checkLibrary :: LoadedProject -> HandlerM ()
checkLibrary project = do
    let manifest = loadedManifest project
        root = loadedRoot project
        manifestFile = root </> "termina.yaml"
        files = sourceTexts (buildSources project Nothing (normalise (root </> "src")))
    loadedLibrary <- liftIO (loadExportedModules project)
    case (loadedLibrary, libraryPlatforms manifest) of
        (Left failure, _) -> publishDiagnostics (failureDiagnosticsIn manifestFile files failure)
        (Right _, []) -> return ()
        (Right (parsedProject, order), plt : _) -> do
            let (typedProject, failure) = typeLibrary manifest plt parsedProject order
            modify (\s -> s { project_modules = storedModules parsedProject typedProject })
            publishDiagnostics (maybe M.empty (failureDiagnosticsIn manifestFile files) failure)

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

-- | The modules of a check, by the path of their file, each with its types and
-- its index when it has been type checked.
storedModules :: ParsedProject -> TypedProject -> M.Map FilePath TerminaStoredModule
storedModules parsedProject typedProject = M.fromList
    [ (Mod.fullPath m, TerminaStoredModule (Mod.fullPath m) (Mod.importedModules m) (Mod.sourcecode m) []
        (Just (Mod.metadata m)) (Mod.metadata <$> typed) (maybe emptyIndex (indexModule . typedAST . Mod.metadata) typed))
    | m <- M.elems parsedProject
    , let typed = M.lookup (Mod.qualifiedName m) typedProject ]
