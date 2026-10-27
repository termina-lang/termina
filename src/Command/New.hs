module Command.New (
    newCmdArgsParser, newCommand, NewCmdArgs
) where

import Options.Applicative
import Control.Exception (IOException, try)
import Control.Monad
import Configuration.Configuration
import Configuration.Manifest (Manifest(..), newApplicationManifest, newLibraryManifest,
    defaultTestPlatforms, addTestEntry, isSnakeCase)
import qualified Data.Map.Strict as M
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

-- | What a new project is
data NewKind = NewApp | NewLib
    deriving (Show,Eq)

-- | Data type for the "new" command arguments
data NewCmdArgs =
    NewProject
        String -- ^ Name of the new project
        (Maybe String) -- ^ Target platform
        NewKind
        (Maybe String) -- ^ Prefix exported by a new library
        Vcs -- ^ Version control system
        Bool -- ^ Generate the VS Code configuration
        Bool -- ^ Verbose mode
    | NewTest
        String -- ^ Name of the new test
        Bool -- ^ Verbose mode
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
newCmdArgsParser :: Parser NewCmdArgs
newCmdArgsParser = newTestParser <|> newProjectParser

  where

    newTestParser = NewTest
        <$> strOption (long "test"
            <> metavar "NAME"
            <> help "Add a test to the project of the current folder")
        <*> verboseSwitch

    newProjectParser = NewProject
        <$> argument str (metavar "PROJECT"
            <> help "Name of the new project")
        <*> optional (option str (long "platform" <> short 'p'
            <> help "Target platform of the application (default: posix-gcc)"))
        <*> (flag' NewLib (long "lib" <> help "Create a library")
            <|> flag NewApp NewApp (long "app" <> help "Create an application (default)"))
        <*> optional (strOption (long "exports"
            <> metavar "PREFIX"
            <> help "Module prefix exported by a new library (default: the name of the project)"))
        <*> option (eitherReader readVcs) (long "vcs"
            <> value VcsGit
            <> metavar "git|none"
            <> help "Version control of the new project (default: git)")
        <*> switch (long "vscode"
            <> help "Generate the VS Code configuration (.vscode and .devcontainer)")
        <*> verboseSwitch

    verboseSwitch = switch (long "verbose"
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
    unless (not (null project) && all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '_' || x == '-') project)
        (die . errorMessage $ "Project name must be one or more ASCII letters, digits, '_' and '-'")

emptyAppModuleContent :: String -> String
emptyAppModuleContent projectName = unlines [
        "// Main application module of project " ++ projectName,
        ""
    ]

manifestFile :: FilePath
manifestFile = "termina" <.> "yaml"

-- | Content of the .gitignore of a new project: the whole output, which the
-- transpiler generates again from the project and the deliverable keeps, and
-- the clones that the user leaves in .termina/fetch/ for "termina add".
gitignoreContent :: String
gitignoreContent = unlines [
        "/" ++ outputFolderName ++ "/",
        "/.termina/",
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
gdbServerAddress RTEMS5LEON3SFNEXYSA7 = Just "host.docker.internal:2222"
gdbServerAddress FreeRTOS10STM32L432NUCLEOL432KC = Just "host.docker.internal:61234"
gdbServerAddress RTEMS6ZYNQ7000PYNQZ2 = Just "host.docker.internal:3000"
gdbServerAddress _ = Nothing

-- | The debugger runs from the root of the project: the @#line@ directives of
-- the generated C name the Termina modules relative to it.
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
                "            \"cwd\": \"${workspaceFolder}\",",
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
                "            \"cwd\": \"${workspaceFolder}\",",
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

tasksContent :: String -> String
tasksContent buildCommandLine = unlines [
        "{",
        "    \"version\": \"2.0.0\",",
        "    \"tasks\": [",
        "        {",
        "            \"label\": \"termina: build\",",
        "            \"type\": \"shell\",",
        "            \"command\": \"" ++ buildCommandLine ++ "\",",
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

-- | Initializes a git repository in the project, with main as the initial
-- branch independently of init.defaultBranch, unless the project lies
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
            initResult <- runGit ["init", "-q", "-b", "main", project]
            case initResult of
                Right _ -> return ()
                Left err -> putStrLn . warnMessage $
                    "The git repository could not be created: " ++ takeWhile (/= '\n') err

-- | Command handler for the "new" command
newCommand :: NewCmdArgs -> IO ()
newCommand (NewTest testName chatty) = newTestCommand testName chatty
newCommand (NewProject project mPltName kind mExports vcs vscode chatty) = do
    validateProjectName project
    plt <- case (kind, mPltName) of
        (NewLib, Just _) -> die . errorMessage $ "A library has no platform: its tests choose theirs"
        (_, Nothing) -> return POSIXGCC
        (NewApp, Just pltName) -> maybe (
                putStrLn (errorMessage $ "Unsupported platform: " ++ show pltName) >>
                showSupportedPlatforms >> exitFailure
            ) return $ checkPlatform pltName
    -- | The prefix that a new library exports, by default named after the project
    prefix <- case (kind, mExports) of
        (NewApp, Just _) -> die . errorMessage $ "An application exports nothing: --exports is only for a library"
        (_, Just given) -> do
            unless (all (isSnakeCase . T.unpack) (T.splitOn (T.pack ".") (T.pack given))) $
                die . errorMessage $ "The prefix \"" ++ given ++ "\" is not a module prefix (lowercase snake_case segments separated by dots)"
            return given
        (_, Nothing) -> do
            let named = map (\c -> if c == '-' then '_' else toLower c) project
            when (kind == NewLib && not (isSnakeCase named)) $
                die . errorMessage $ "The name of a library has to make a module prefix: \"" ++ named ++ "\" is not lowercase snake_case (choose one with --exports)"
            return named
    when chatty (putStrLn . debugMessage $ "Creating new project: " ++ project)
    -- Check if the directory already exists
    exists <- doesPathExist project
    when exists (die . errorMessage $ "Path already exists: " ++ project)
    -- | Create the project directory
    when chatty (putStrLn . debugMessage $ "Creating project directory: " ++ project)
    createDirectory project
    -- | Create project default structure
    let config = defaultConfig project plt
    let configFile = project </> manifestFile
    let sourceModulesFolderPath = project </> sourceModulesFolder config
    let outputFolderPath = project </> outputFolder config
    when chatty (putStrLn . debugMessage $ "Creating project configuration file: " ++ configFile)
    writeFile configFile $ case kind of
        NewApp -> newApplicationManifest project plt
        NewLib -> newLibraryManifest project prefix
    when chatty (putStrLn . debugMessage $ "Creating project source modules directory: " ++ sourceModulesFolderPath)
    createDirectory sourceModulesFolderPath
    when (kind == NewLib) $
        createDirectoryIfMissing True (sourceModulesFolderPath </> joinPath (map T.unpack (T.splitOn (T.pack ".") (T.pack prefix))))
    when (kind == NewApp) $ do
        let appFolderPath = project </> appFolder
        when chatty (putStrLn . debugMessage $ "Creating project application directory: " ++ appFolderPath)
        createDirectory appFolderPath
        when chatty (putStrLn . debugMessage $ "Creating empty app module")
        writeFile (appFolderPath </> appFilename <.> "fin") (emptyAppModuleContent project)
    when chatty (putStrLn . debugMessage $ "Creating project output directory: " ++ outputFolderPath)
    createDirectory outputFolderPath
    when chatty (putStrLn . debugMessage $ "Creating .gitignore")
    writeFile (project </> ".gitignore") gitignoreContent
    when vscode $ do
        when chatty (putStrLn . debugMessage $ "Creating the VS Code configuration")
        createDirectory (project </> ".devcontainer")
        writeFile (project </> ".devcontainer" </> "devcontainer.json") (devcontainerContent project)
        createDirectory (project </> ".vscode")
        case kind of
            NewApp -> do
                writeFile (project </> ".vscode" </> "launch.json") (launchContent project (outputFolder config </> "app") plt)
                writeFile (project </> ".vscode" </> "tasks.json") (tasksContent "termina build")
            NewLib ->
                writeFile (project </> ".vscode" </> "tasks.json") (tasksContent "termina build --tests")
    when (vcs == VcsGit) (initRepository chatty project)
    when chatty (putStrLn . debugMessage $ "Project created successfully")

-- | Add a test to the project of the current folder: its folder, with an
-- empty "test.fin", and its entry in the manifest. If the manifest does not
-- read back, both are undone.
newTestCommand :: String -> Bool -> IO ()
newTestCommand testName chatty = do
    manifest <- loadManifest >>= either dieWithErrors return
    unless (isSnakeCase testName) $
        die . errorMessage $ "The name of a test is lowercase snake_case: \"" ++ testName ++ "\""
    when (T.pack testName `M.member` manifestTests manifest) $
        die . errorMessage $ "The manifest already declares the test " ++ testName
    let testFolder = "tests" </> testName
        testFile = testFolder </> "test" <.> "fin"
    exists <- doesPathExist testFolder
    when exists (die . errorMessage $ "Path already exists: " ++ testFolder)
    original <- readFileStrict manifestFile
    updated <- either (die . errorMessage) return $
        addTestEntry original testName (defaultTestPlatforms manifest) 10
    when chatty (putStrLn . debugMessage $ "Creating " ++ testFile)
    createDirectoryIfMissing True testFolder
    writeFile testFile (emptyTestModuleContent testName)
    when chatty (putStrLn . debugMessage $ "Adding the test to " ++ manifestFile)
    writeFile manifestFile updated
    reloaded <- loadManifest
    case reloaded of
        Right _ -> when chatty (putStrLn . debugMessage $ "Test created successfully")
        Left err -> do
            writeFile manifestFile original
            removeFile testFile
            removeDirectory testFolder
            dieWithErrors err

  where

    -- | The whole file, read before it is written again
    readFileStrict path = do
        content <- readFile path
        length content `seq` return content

emptyTestModuleContent :: String -> String
emptyTestModuleContent testName = unlines [
        "// Test " ++ testName,
        ""
    ]
