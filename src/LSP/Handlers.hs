{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Handlers where

import Control.Lens ((^.))
import Language.LSP.Server
import Language.LSP.Protocol.Message
import LSP.Monad
import Language.LSP.Protocol.Types
import Control.Monad.State

import qualified Language.LSP.Protocol.Lens as J
import Configuration.Platform
import Configuration.Configuration
import LSP.Logging
import System.Directory
import qualified Data.Text as T
import Command.Utils
import LSP.Utils
import qualified Data.Map as M
import System.FilePath
import LSP.Modules
import Generator.Environment (getPlatformInitialGlobalEnv)
import Data.Functor (void)
import Semantic.Environment

initializeHandler :: TMessage Method_Initialize -> HandlerM ()
initializeHandler _req = do
    infoM "Loading termina.yaml..."
    ecfg <- loadConfig
    case ecfg of
      Left err -> 
        errorM $ "Error when parsing termina.yaml: " <> T.pack (show err)
      Right cfg -> do
        -- We have loaded the configuration file. Then we must check that the platform is OK
        -- Decode the selected platform field
        infoM "Loaded termina.yaml"
        case checkPlatform (platform cfg) of
          Nothing ->
            errorM $ "Unsupported platform: \"" <> T.pack (show (platform cfg)) <> "\""
          Just plt -> do
            put $ ServerState (Just cfg) mempty 
            -- The platform is OK
            -- Then we have to check the folder's structure
            existSourceFolder <- liftIO $ doesDirectoryExist (sourceModulesFolder cfg)
            existAppFolder <- liftIO $ doesDirectoryExist (appFolder cfg)
            if not existSourceFolder then
              errorM ("Source folder \"" <> T.pack (sourceModulesFolder cfg) <> "\" does not exist")
            else if not existAppFolder then
              errorM ("Application folder \"" <> T.pack (appFolder cfg) <> "\" does not exist")
            else do
              -- At this point, no files have been loaded into the VFS, so we must read all
              -- the files directly from the file system.
              absPath <- liftIO $ canonicalizePath (appFolder cfg)
              let fullP = absPath </> appFilename cfg <.> "fin"
              mappModule <- loadTerminaModule fullP (Just (sourceModulesFolder cfg))
              case mappModule of
                Nothing -> return ()
                Just appModule -> do
                  -- Load the modules of the project
                  loadModules (importedModules appModule) (sourceModulesFolder cfg)
                  -- | Once all files have been loaded, we may proceed to type check them
                  -- We need to first obtain the order in which the modules must be type checked
                  parsedProject <- gets project_modules
                  let projectDependencies = fmap importedModules parsedProject
                  either
                    (\_loop ->
                      -- TODO: Generate diagnostics
                      return ())
                    (\orderedDependencies -> 
                      let initialGlobalEnv = makeInitialGlobalEnv (Just cfg) (getPlatformInitialGlobalEnv cfg plt) in
                      void $ typeModules (sourceModulesFolder cfg) M.empty initialGlobalEnv orderedDependencies)
                    $ sortProjectDepsOrLoop projectDependencies 
              return ()

typeProject :: HandlerM ()
typeProject = do
  parsedProject <- gets project_modules
  cfg <- gets config
  let projectDependencies = M.map importedModules parsedProject
  either
    (\_loop ->
      -- TODO: Generate diagnostics
      return ())
    (\orderedDependencies -> do
      let pltInitialGlbEnv = case cfg of
            Nothing -> []
            Just cfg' -> case checkPlatform (platform cfg') of 
              Nothing -> []
              Just plt -> getPlatformInitialGlobalEnv cfg' plt
      let initialGlobalEnv = makeInitialGlobalEnv cfg pltInitialGlbEnv
      case cfg of
        Nothing -> void $ typeModules "" M.empty initialGlobalEnv orderedDependencies
        Just cfg' -> void $ typeModules (sourceModulesFolder cfg') M.empty initialGlobalEnv orderedDependencies)
    $ sortProjectDepsOrLoop projectDependencies
  diags <- gets project_modules
  mapM_ (uncurry emitDiagnostics) (M.toList (diagnostics <$> diags)) 

documentChange :: Handlers HandlerM
documentChange  = notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
  let fileURI = msg ^. J.params . J.textDocument . J.uri
  case uriToFilePath fileURI of 
    Nothing -> 
      sendNotification SMethod_WindowShowMessage
        (ShowMessageParams MessageType_Error $ T.pack ("Internal error: unknown file: " ++ show (uriToFilePath fileURI)))
    Just filePath -> do
      cfg <- gets config
      _ <- loadTerminaModule filePath (sourceModulesFolder <$> cfg)
      typeProject

initialized :: Handlers HandlerM
initialized = notificationHandler SMethod_Initialized $ \_msg -> do
  diags <- gets project_modules
  mapM_ (uncurry emitDiagnostics) (M.toList (diagnostics <$> diags))

didOpen :: Handlers HandlerM
didOpen = notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
    let fileURI = msg ^. J.params . J.textDocument . J.uri
    case uriToFilePath fileURI of 
      Nothing -> 
        sendNotification SMethod_WindowShowMessage
          (ShowMessageParams MessageType_Error $ T.pack ("Internal error: unknown file: " ++ show (uriToFilePath fileURI)))
      Just filePath -> do
        -- TODO: See if we can improve this. Now we load the module in all cases, so that we
        -- ensure that, even if the module was not previously loaded, it is now present as part
        -- of the project. Modules are loaded at the beginning, but only those that are
        -- referenced (either directly or indirectly, by the main app module). 
        cfg <- gets config
        _ <- loadTerminaModule filePath (sourceModulesFolder <$> cfg)
        typeProject

handlers :: Handlers HandlerM
handlers =
  mconcat
    [ 
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not ->
        return ()
      , initialized
      , documentChange
    ]


