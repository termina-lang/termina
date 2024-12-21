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
import Configuration.Platform (checkPlatform)
import Configuration.Configuration (TerminaConfig(..))
import LSP.Logging
import System.Directory (doesDirectoryExist)
import qualified Data.Text as T
import Command.Utils (loadConfig)
import LSP.Utils
import qualified Data.Map as M
import System.FilePath
import LSP.Modules

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
          Just _plt -> do
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
              let fullP = appFolder cfg </> appFilename cfg <.> "fin"
              loadTerminaModule fullP (sourceModulesFolder cfg)

documentChange :: Handlers HandlerM
documentChange  = notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
  let fileURI = msg ^. J.params . J.textDocument . J.uri
  sendNotification SMethod_WindowShowMessage
    (ShowMessageParams MessageType_Info $ T.pack ("File changed: " ++ show (uriToFilePath fileURI)))

initialized :: Handlers HandlerM
initialized = notificationHandler SMethod_Initialized $ \_msg -> do
  diags <- gets project_modules
  mapM_ (uncurry emitDiagnostics) (M.toList (diagnostic <$> diags))

handlers :: Handlers HandlerM
handlers =
  mconcat
    [ 
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not ->
        return ()
      , initialized
      , documentChange
    ]


