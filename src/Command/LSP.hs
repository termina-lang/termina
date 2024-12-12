module Command.LSP where

import Options.Applicative
import Control.Monad
import Command.Utils (debugMessage)
import LSP.Server

-- | Data type for the "lsp" command arguments
newtype LSPCmdArgs =
    LSPCmdArgs
        Bool -- ^ Verbose mode
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
lspCmdArgsParser :: Parser LSPCmdArgs
lspCmdArgsParser = LSPCmdArgs
    <$> switch (long "verbose"
        <> short 'v'
        <> help "Enable verbose mode")

-- | Command handler for the "lsp" command
lspCommand :: LSPCmdArgs -> IO ()
lspCommand (LSPCmdArgs chatty) = do
  when chatty (putStrLn . debugMessage $ "Reading project configuration from \"termina.yaml\"")
  lspRunServer