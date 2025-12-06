module Command.New (
    newCmdArgsParser, newCommand, NewCmdArgs
) where

import Options.Applicative
import Control.Monad
import Configuration.Configuration
import Command.Utils
import System.FilePath
import System.Exit
import System.Directory
import qualified Data.Text as T
import Data.Char
import Configuration.Platform

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
    <*> option str (long "platform" <> short 'p'
        <> value "posix-gcc"
        <> help "Target platform for the new project")
    <*> switch (long "verbose"
        <> short 'v'
        <> help "Enable verbose mode")

showSupportedPlatforms :: IO ()
showSupportedPlatforms = 
    putStrLn "These are the currently supported platforms:" >>
    mapM_ (\(plt, desc) ->
        putStr (show plt) >> putStr ": " >> putStrLn desc) supportedPlatforms

validateProjectName :: String -> IO ()
validateProjectName project =
    unless (all (\x -> isAlphaNum x || x == '_') project) (die . errorMessage $ "Project name must be alphanumeric")

emptyAppModuleContent :: String -> String
emptyAppModuleContent projectName = unlines [
        "// Main application module of project " ++ projectName,
        ""
    ]

-- | Command handler for the "new" command
newCommand :: NewCmdArgs -> IO ()
newCommand (NewCmdArgs project pltName chatty) = do
    validateProjectName project
    let pname = T.pack pltName
    plt <- maybe (
            putStrLn (errorMessage $ "Unsupported platform: " ++ show pname) >>
            showSupportedPlatforms >> exitFailure
        ) return $ checkPlatform (T.unpack pname)
    when chatty (putStrLn . debugMessage $ "Selected platform: \"" ++ show plt ++ "\"")
    when chatty (putStrLn . debugMessage $ "Creating new project: " ++ project)
    -- Check if the directory already exists
    exists <- doesPathExist project
    when exists (die . errorMessage $ "Path already exists: " ++ project)
    -- | Create the project directory
    when chatty (putStrLn . debugMessage $ "Creating project directory: " ++ project)
    createDirectory project
    -- | Create project default structure
    let config = defaultConfig project plt
    let configFile = project </> "termina" <.> "yaml"
    let appFolderPath = project </> appFolder config
    let appModulePath = appFolderPath </> appFilename config <.> "fin"
    let sourceModulesFolderPath = project </> sourceModulesFolder config
    let outputFolderPath = project </> outputFolder config
    when chatty (putStrLn . debugMessage $ "Creating project configuration file: " ++ configFile)
    serializeConfig project config
    when chatty (putStrLn . debugMessage $ "Creating project source modules directory: " ++ sourceModulesFolderPath)
    createDirectory sourceModulesFolderPath
    when chatty (putStrLn . debugMessage $ "Creating project application directory: " ++ appFolderPath)
    createDirectory appFolderPath
    when chatty (putStrLn . debugMessage $ "Creating project output directory: " ++ outputFolderPath)
    createDirectory outputFolderPath
    when chatty (putStrLn . debugMessage $ "Creating empty app module")
    writeFile appModulePath (emptyAppModuleContent project)
    when chatty (putStrLn . debugMessage $ "Project created successfully")

