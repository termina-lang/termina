module Command.New (
    newCmdArgsParser, newCommand, NewCmdArgs
) where

import Options.Applicative
import Control.Monad

-- | Data type for the "new" command arguments
data NewCmdArgs =
    NewCmdArgs
        String -- ^ Name of the new project
        Bool -- ^ Verbose mode
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
newCmdArgsParser :: Parser NewCmdArgs
newCmdArgsParser = NewCmdArgs
    <$> argument str (metavar "PROJECT"
        <> help "Name of the new project")
    <*> switch (long "verbose"
        <> short 'v'
        <> help "Enable verbose mode")

-- | Command handler for the "new" command
newCommand :: NewCmdArgs -> IO ()
newCommand (NewCmdArgs project chatty) = do
    when chatty (putStrLn $ "Creating new project: " ++ project)
