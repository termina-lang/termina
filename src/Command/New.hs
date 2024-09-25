module Command.New (
    newCmdArgsParser, newCommand, NewCmdArgs
) where

import Options.Applicative
import Control.Monad
import Command.Utils
import Generator.Platform
import System.Exit
import Data.Text as T

-- | Data type for the "new" command arguments
data NewCmdArgs =
    NewCmdArgs
        String -- ^ Name of the new project
        String -- ^ Target platform
        Bool -- ^ Verbose mode
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
newCmdArgsParser :: Parser NewCmdArgs
newCmdArgsParser = NewCmdArgs
    <$> argument str (metavar "PROJECT"
        <> help "Name of the new project")
    <*> argument str (metavar "PLATFORM"
        <> help "Target platform for the new project")
    <*> switch (long "verbose"
        <> short 'v'
        <> help "Enable verbose mode")

showSupportedPlatforms :: IO ()
showSupportedPlatforms = 
    putStrLn "These are the currently supported platforms:" >>
    mapM_ (\(plt, desc) ->
        putStr (show plt) >> putStr ": " >> putStrLn desc) supportedPlatforms

-- | Command handler for the "new" command
newCommand :: NewCmdArgs -> IO ()
newCommand (NewCmdArgs project pltName chatty) = do
    let platform = T.pack pltName
    plt <- maybe (
            putStrLn (errorMessage $ "Unsupported platform: " ++ show platform) >> putStr "\n" >>
            showSupportedPlatforms >> exitFailure
        ) return $ checkPlatform platform
    when chatty (putStrLn . debugMessage $ "Selected platform: \"" ++ show plt ++ "\"")
    when chatty (putStrLn . debugMessage $ "Creating new project: " ++ project)

