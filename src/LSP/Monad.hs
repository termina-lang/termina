{-# LANGUAGE BlockArguments #-}
module LSP.Monad where
    
import Language.LSP.Server ( LspT, LanguageContextEnv, runLspT )
import Configuration.Configuration
import Control.Monad.State
import Control.Concurrent.MVar

newtype ServerState = 
  ServerState (Maybe TerminaConfig)

type HandlerM =
    StateT ServerState (LspT () IO)

liftLSP :: LspT () IO a -> HandlerM a
liftLSP = lift

-- | Runs the language server's state monad.
runLSM :: HandlerM a -> MVar ServerState -> LanguageContextEnv () -> IO a
runLSM handler st environment = modifyMVar st \oldSt -> do
    runLspT environment do
        (result, newState) <- runStateT handler oldSt
        return (newState, result)