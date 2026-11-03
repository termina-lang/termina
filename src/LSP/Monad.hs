{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LSP.Monad where
    
import Language.LSP.Server ( LspT, LanguageContextEnv, runLspT )
import Command.Sources (LoadedProject)
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import LSP.Modules

data ServerState =
  ServerState {
    -- | The project read from disk when the server starts
    loaded :: Maybe LoadedProject,
    -- | The modules of the last check, by the absolute path of their file
    project_modules :: M.Map FilePath TerminaStoredModule,
    -- | The files whose diagnostics the server has published and not cleared
    published :: S.Set FilePath
  }

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