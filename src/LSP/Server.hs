{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LSP.Server where

import LSP.Monad
import Control.Monad.IO.Class
import Language.LSP.Server

import Control.Monad
import Language.LSP.Protocol.Types
import qualified Control.Concurrent.MVar as MVar
import LSP.Handlers

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True
    , _change = Just TextDocumentSyncKind_Incremental
    , _willSave = Just False
    , _willSaveWaitUntil = Just False
    , _save = Just $ InR $ SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions
    }

lspRunServer :: IO ()
lspRunServer = do

  st <- MVar.newMVar (ServerState Nothing)

  let interpretHandler environment = Iso{..}

        where
          forward :: HandlerM a -> IO a
          forward handler = runLSM handler st environment
          backward = liftIO


  void $ runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "termina.languageServer"
      , doInitialize = \env req -> runLSM (initializeHandler req) st env >> return (Right env)
      , staticHandlers = const handlers
      , interpretHandler = interpretHandler
      , options = lspOptions
      }