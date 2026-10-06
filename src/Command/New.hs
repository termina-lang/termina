module Command.New (
    newCmdArgsParser, newCommand, NewCmdArgs
) where

import Options.Applicative
import Control.Exception (IOException, try)
import Control.Monad
import Configuration.Configuration
import Command.Utils
import System.FilePath
import System.Exit
import System.Directory
import System.Process (readProcessWithExitCode)
import qualified Data.Text as T
import Data.Char
import Data.List (intercalate)
import Data.Version (versionBranch)
import Configuration.Platform
import Paths_termina (version)

-- | Version control system of a new project
data Vcs = VcsGit | VcsNone
    deriving (Show,Eq)

-- | Data type for the "new" command arguments
data NewCmdArgs =
    NewCmdArgs
        String -- ^ Name of the new project
        String -- ^ Target platform
        Vcs -- ^ Version control system
        Bool -- ^ Generate the VS Code configuration
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
    <*> option (eitherReader readVcs) (long "vcs"
        <> value VcsGit
        <> metavar "git|none"
        <> help "Version control of the new project (default: git)")
    <*> switch (long "vscode"
        <> help "Generate the VS Code configuration (.vscode and .devcontainer)")
    <*> switch (long "verbose"
        <> short 'v'
        <> help "Enable verbose mode")

readVcs :: String -> Either String Vcs
readVcs "git" = Right VcsGit
readVcs "none" = Right VcsNone
readVcs other = Left $ "Unknown version control system: " ++ other ++ " (use git or none)"

showSupportedPlatforms :: IO ()
showSupportedPlatforms =
    putStrLn "These are the currently supported platforms:" >>
    mapM_ (\(plt, desc) ->
        putStr (show plt) >> putStr ": " >> putStrLn desc) supportedPlatforms

validateProjectName :: String -> IO ()
validateProjectName project =
    unless (all (\x -> isAlphaNum x || x == '_') project) (die . errorMessage $ "Project name must be alphanumeric")

emptyAppModuleContent :: String -> String
emptyAppModuleContent projectName = unlines [
        "// Main application module of project " ++ projectName,
        ""
    ]

-- | Content of the .gitignore of a new project: the binaries the build leaves
-- in the output folder. The generated C code is kept under version control.
gitignoreContent :: FilePath -> String
gitignoreContent output = unlines [
        "/" ++ output ++ "/bin/",
        ".DS_Store"
    ]

-- | Tag of the development image that matches this transpiler. The image has a
-- version of its own, out of the lockstep, so the tag is the MAJOR.MINOR of the
-- transpiler, which every image of that minor version is compatible with.
imageTag :: String
imageTag = intercalate "." (map show (take 2 (versionBranch version)))

devcontainerContent :: String -> String
devcontainerContent project = unlines [
        "{",
        "    \"name\": \"" ++ project ++ "\",",
        "    \"image\": \"ghcr.io/termina-lang/docker-termina:" ++ imageTag ++ "\",",
        "    \"runArgs\": [",
        "        \"--cap-add=SYS_PTRACE\",",
        "        \"--security-opt\", \"seccomp=unconfined\",",
        "        \"--add-host=host.docker.internal:host-gateway\"",
        "    ],",
        "    \"customizations\": {",
        "        \"vscode\": {",
        "            \"extensions\": [\"termina-lang.termina\", \"ms-vscode.cpptools\"]",
        "        }",
        "    },",
        "    \"remoteUser\": \"vscode\"",
        "}"
    ]

-- | Default address of the GDB server of a target platform, outside the
-- container: GRMON for the LEON3 board, the ST-LINK_gdbserver of STM32CubeCLT
-- for the Nucleo board, and hw_server, whose first core listens on port 3000,
-- for the PYNQ-Z2 board.
gdbServerAddress :: Platform -> Maybe String
gdbServerAddress RTEMS5LEON3NEXYSA7 = Just "host.docker.internal:2222"
gdbServerAddress FreeRTOS10STM32L432NUCLEOL432KC = Just "host.docker.internal:61234"
gdbServerAddress RTEMS6ZYNQ7000PYNQZ2 = Just "host.docker.internal:3000"
gdbServerAddress _ = Nothing

launchContent :: String -> FilePath -> Platform -> String
launchContent project output plt =
    case gdbServerAddress plt of
        Nothing -> unlines [
                "{",
                "    \"version\": \"0.2.0\",",
                "    \"configurations\": [",
                "        {",
                "            \"name\": \"Debug (" ++ show plt ++ ")\",",
                "            \"type\": \"cppdbg\",",
                "            \"request\": \"launch\",",
                "            \"program\": \"${workspaceFolder}/" ++ output ++ "/bin/" ++ project ++ "\",",
                "            \"cwd\": \"${workspaceFolder}/" ++ output ++ "\",",
                "            \"MIMode\": \"gdb\",",
                "            \"preLaunchTask\": \"termina: build\",",
                "            \"setupCommands\": [",
                "                { \"text\": \"set startup-with-shell off\" }",
                "            ]",
                "        }",
                "    ]",
                "}"
            ]
        Just address -> unlines [
                "{",
                "    \"version\": \"0.2.0\",",
                "    \"configurations\": [",
                "        {",
                "            \"name\": \"Debug (" ++ show plt ++ ")\",",
                "            \"type\": \"cppdbg\",",
                "            \"request\": \"launch\",",
                "            \"program\": \"${workspaceFolder}/" ++ output ++ "/bin/" ++ project ++ "\",",
                "            \"cwd\": \"${workspaceFolder}/" ++ output ++ "\",",
                "            \"MIMode\": \"gdb\",",
                "            \"miDebuggerPath\": \"gdb-multiarch\",",
                "            \"miDebuggerServerAddress\": \"${input:gdbServer}\",",
                "            \"postRemoteConnectCommands\": [",
                "                { \"text\": \"load\" }",
                "            ],",
                "            \"preLaunchTask\": \"termina: build\"",
                "        }",
                "    ],",
                "    \"inputs\": [",
                "        {",
                "            \"id\": \"gdbServer\",",
                "            \"type\": \"promptString\",",
                "            \"description\": \"GDB server of the board or the simulator (host:port)\",",
                "            \"default\": \"" ++ address ++ "\"",
                "        }",
                "    ]",
                "}"
            ]

tasksContent :: FilePath -> String
tasksContent output = unlines [
        "{",
        "    \"version\": \"2.0.0\",",
        "    \"tasks\": [",
        "        {",
        "            \"label\": \"termina: build\",",
        "            \"type\": \"shell\",",
        "            \"command\": \"termina build && make -C " ++ output ++ "\",",
        "            \"options\": { \"cwd\": \"${workspaceFolder}\" },",
        "            \"group\": { \"kind\": \"build\", \"isDefault\": true },",
        "            \"problemMatcher\": []",
        "        }",
        "    ]",
        "}"
    ]

-- | Runs git with the given arguments, or reports why it could not.
runGit :: [String] -> IO (Either String String)
runGit args = do
    result <- try (readProcessWithExitCode "git" args "")
    return $ case result of
        Left e -> Left (show (e :: IOException))
        Right (ExitSuccess, out, _) -> Right out
        Right (ExitFailure _, _, err) -> Left err

-- | Initializes a git repository in the project, unless the project lies
-- inside the working tree of another one. A failure is a warning: the project
-- is created anyway.
initRepository :: Bool -> FilePath -> IO ()
initRepository chatty project = do
    parent <- takeDirectory <$> makeAbsolute project
    inside <- runGit ["-C", parent, "rev-parse", "--is-inside-work-tree"]
    case inside of
        Right out | takeWhile (not . isSpace) out == "true" ->
            when chatty (putStrLn . debugMessage $
                "The project is inside a git repository: no repository created")
        _ -> do
            when chatty (putStrLn . debugMessage $ "Initializing git repository")
            initResult <- runGit ["init", "-q", project]
            case initResult of
                Right _ -> return ()
                Left err -> putStrLn . warnMessage $
                    "The git repository could not be created: " ++ takeWhile (/= '\n') err

-- | Command handler for the "new" command
newCommand :: NewCmdArgs -> IO ()
newCommand (NewCmdArgs project pltName vcs vscode chatty) = do
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
    -- | Create the project directory
    when chatty (putStrLn . debugMessage $ "Creating project directory: " ++ project)
    createDirectory project
    -- | Create project default structure
    let config = defaultConfig project plt
    let configFile = project </> "termina" <.> "yaml"
    let appFolderPath = project </> appFolder
    let appModulePath = appFolderPath </> appFilename <.> "fin"
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
    when chatty (putStrLn . debugMessage $ "Creating .gitignore")
    writeFile (project </> ".gitignore") (gitignoreContent (outputFolder config))
    when vscode $ do
        when chatty (putStrLn . debugMessage $ "Creating the VS Code configuration")
        createDirectory (project </> ".devcontainer")
        writeFile (project </> ".devcontainer" </> "devcontainer.json") (devcontainerContent project)
        createDirectory (project </> ".vscode")
        writeFile (project </> ".vscode" </> "launch.json") (launchContent project (outputFolder config) plt)
        writeFile (project </> ".vscode" </> "tasks.json") (tasksContent (outputFolder config))
    when (vcs == VcsGit) (initRepository chatty project)
    when chatty (putStrLn . debugMessage $ "Project created successfully")
