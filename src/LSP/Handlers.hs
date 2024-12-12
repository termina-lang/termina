{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Handlers where

import Control.Lens ((^.))
import Language.LSP.Server
import Language.LSP.Protocol.Message
import LSP.Monad
import LSP.Config
import Language.LSP.Protocol.Types
import qualified Data.Text as T
import Control.Monad.State

import qualified Language.LSP.Protocol.Lens as J

initializeHandler :: TMessage Method_Initialize -> HandlerM ()
initializeHandler _req = do
    cfg <- loadConfig
    case cfg of
      Left err -> 
        liftLSP $ 
          sendNotification SMethod_WindowShowMessage 
            (ShowMessageParams MessageType_Error $ "Error when parsing termina.yaml: " <> T.pack (show err))
      Right config -> do
        liftLSP $
          sendNotification SMethod_WindowShowMessage
            (ShowMessageParams MessageType_Info "Loaded termina.yaml")
        put (ServerState (Just config))
        return ()

handlers :: Handlers HandlerM
handlers =
  mconcat
    [ 
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not ->
        return ()
      , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let fileURI = msg ^. J.params . J.textDocument . J.uri
        liftLSP $
              sendNotification SMethod_WindowShowMessage
                (ShowMessageParams MessageType_Info $ T.pack ("File changed: " ++ show (uriToFilePath fileURI)))
    , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        responder (Right $ InL rsp)
    ]


