{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LSP.Server where

import LSP.Monad
import Control.Monad.IO.Class
import Language.LSP.Server

import Control.Monad
import Language.LSP.Protocol.Types
import qualified Control.Concurrent.MVar as MVar
import LSP.Handlers
import Colog.Core (LogAction (..), WithSeverity (..))
import System.IO (stdin, stdout)


lspRunServer :: IO ()
lspRunServer = do

  st <- MVar.newMVar (ServerState Nothing mempty)

  void $ runServerWithHandles logger logger stdin stdout $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "termina.languageServer"
      , doInitialize = \env req -> runLSM (initializeHandler req) st env >> return (Right env)
      , staticHandlers = const handlers
      , interpretHandler = \environment -> Iso (\handler -> runLSM handler st environment) liftIO
      , options = defaultOptions
        { optTextDocumentSync = Just syncOptions
        }
      }
  
  where

    logger :: Monad m => LogAction m (WithSeverity LspServerLog)
    logger = LogAction $ const $ return ()

    syncOptions :: TextDocumentSyncOptions
    syncOptions =
      TextDocumentSyncOptions
        { _openClose = Just True
        , _change = Just TextDocumentSyncKind_Incremental
        , _willSave = Just False
        , _willSaveWaitUntil = Just False
        , _save = Just $ InR $ SaveOptions $ Just False
        }