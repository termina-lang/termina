{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LSP.Monad where
    
import Language.LSP.Server ( LspT, LanguageContextEnv, runLspT )
import Configuration.Configuration
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Monad.Reader

newtype ServerState = 
  ServerState (Maybe TerminaConfig)

type HandlerM = LspT () (ReaderT (MVar ServerState) IO)

instance MonadState ServerState HandlerM where
    state f = do
      stVar <- lift ask
      liftIO $ modifyMVar stVar $ \s -> 
        let (a, s') = f s in
        return (s', a)

-- | Runs the language server's state monad.
runLSM :: HandlerM a -> MVar ServerState -> LanguageContextEnv () -> IO a
runLSM lsm stVar cfg = runReaderT (runLspT cfg lsm) stVar