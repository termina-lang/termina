-- | termina doctor: the OSAL, the tools and the toolchain of each platform
-- that this machine has, and, inside a project, those that the project
-- needs and the image of its devcontainer.json.
module Command.Doctor (
    doctorCmdArgsParser, doctorCommand, DoctorCmdArgs,
    Toolchain(..), devcontainerImageTag, minorVersion, toolchainLine
) where

import Command.Deliver (toolchainVariables)
import Command.Provenance (readOsalVersion, terminaVersion)
import Command.Utils (errorMessage, findProjectRoot, loadManifest)
import Configuration.Manifest
import Configuration.Platform

import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, nub, stripPrefix, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Text as T
import qualified Options.Applicative as O
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Process (proc, readCreateProcessWithExitCode)

-- | termina doctor takes no options.
data DoctorCmdArgs = DoctorCmdArgs
    deriving Show

doctorCmdArgsParser :: O.Parser DoctorCmdArgs
doctorCmdArgsParser = pure DoctorCmdArgs

-- | What make reads from the platform.mk of a platform in the OSAL.
data Toolchain = Toolchain {
    toolchainCC :: !String,
    toolchainObjcopy :: !String,
    -- | The definition of CROSS, unexpanded, which names the variable of the
    -- folder of the toolchain
    toolchainCross :: !String
  }

doctorCommand :: DoctorCmdArgs -> IO ()
doctorCommand DoctorCmdArgs = do
    putStrLn $ "termina " ++ terminaVersion
    fromEnv <- lookupEnv "TERMINA_OSAL_DIR"
    let osalDir = fromMaybe "/opt/termina/osal" fromEnv
        osalSource = maybe "the default" (const "TERMINA_OSAL_DIR") fromEnv
    osalFound <- doesDirectoryExist (osalDir </> "platform")
    osalVersion <- if osalFound then readOsalVersion osalDir else return Nothing
    realDir <- if osalFound then canonicalizePath osalDir else return osalDir
    putStrLn $ field "OSAL" $
        if osalFound
          then fromMaybe "no VERSION" osalVersion ++ " in " ++ osalDir
            ++ (if realDir /= osalDir then " (" ++ realDir ++ ")" else "") ++ ", " ++ osalSource
          else "not found in " ++ osalDir ++ ", " ++ osalSource
    case osalVersion of
        Just v | minorVersion v /= minorVersion terminaVersion ->
            putStrLn $ field "" ("the OSAL " ++ v ++ " is not of the version " ++ minor terminaVersion ++ " of termina")
        _ -> return ()
    makeVersion <- firstLineOf "make" ["--version"]
    gitVersion <- firstLineOf "git" ["--version"]
    gdbVersion <- firstLineOf "gdb-multiarch" ["--version"]
    putStrLn $ field "make" (fromMaybe "not found" makeVersion)
    putStrLn $ field "git" (fromMaybe "not found" gitVersion)
    putStrLn $ field "gdb-multiarch" (fromMaybe "not found" gdbVersion)
    -- | The project of the current folder, if any, and its platforms
    project <- findProjectRoot >>= either (const (return Nothing)) (\root -> fmap ((,) root) <$> projectManifest root)
    let used = maybe [] (projectPlatforms . snd) project
    putStrLn "Platforms:"
    missing <- fmap concat . forM (map fst supportedPlatforms) $ \plt -> do
        line <- if osalFound && makeVersion /= Nothing
            then platformLine osalDir plt
            else return (Left "needs make and the OSAL to be read")
        putStrLn $ "  " ++ pad 36 (show plt) ++ either id id line ++ (if plt `elem` used then " [used by the project]" else "")
        return [plt | plt `elem` used, either (const True) (const False) line]
    case project of
        Nothing -> return ()
        Just (root, _) -> do
            putStrLn $ "Project in " ++ root ++ ":"
            image <- devcontainerLine root
            putStrLn $ "  " ++ field "image" image
    let problems = ["the OSAL is not found" | not osalFound] ++ ["make is not found" | isNothing makeVersion]
            ++ ["the compiler of " ++ show plt ++ ", which the project uses, is not found" | plt <- missing]
    unless (null problems) $ do
        hPutStrLn stderr (errorMessage (intercalate "; " problems))
        exitFailure

  where

    field name value = pad 15 name ++ value

    pad n s = s ++ replicate (max 1 (n - length s)) ' '

    minor v = fromMaybe v (minorVersion v)

    projectManifest root = either (const Nothing) Just <$> loadManifest root

-- | The platforms of the application and of the tests of a project.
projectPlatforms :: Manifest -> [Platform]
projectPlatforms manifest = nub $
    [appPlatform app | Application app <- [manifestNature manifest]]
    ++ concatMap testPlatforms (M.elems (manifestTests manifest))

-- | The compiler and the objcopy of a platform, found or not: Right when the
-- compiler is found.
platformLine :: FilePath -> Platform -> IO (Either String String)
platformLine osalDir plt = do
    toolchain <- readToolchain osalDir plt
    case toolchain of
        Nothing -> return (Left "the OSAL has no platform.mk for it")
        Just tc -> do
            cc <- toolVersion (toolchainCC tc)
            objcopy <- findTool (toolchainObjcopy tc)
            return (toolchainLine tc cc (objcopy /= Nothing))

-- | The line of a platform, from its compiler, the first line of the
-- --version of the compiler when it is found, and whether objcopy is found.
toolchainLine :: Toolchain -> Maybe String -> Bool -> Either String String
toolchainLine tc cc objcopy =
    case cc of
        Nothing -> Left $ toolchainCC tc ++ " not found" ++ hint
        Just v -> Right $ v ++ (if objcopy then "" else "; " ++ toolchainObjcopy tc ++ " not found, so there are no hashes and no deliver")

  where

    hint = case toolchainVariables (M.fromList [("CROSS", toolchainCross tc)]) of
        [] -> ""
        vars -> " (install it there or set " ++ intercalate " or " vars ++ ")"

-- | The toolchain that the platform.mk of a platform defines, as make reads
-- it with the variables of the environment.
readToolchain :: FilePath -> Platform -> IO (Maybe Toolchain)
readToolchain osalDir plt = do
    let mk = osalDir </> "platform" </> show plt </> "platform.mk"
    exists <- doesFileExist mk
    if not exists then return Nothing else do
      let script = unlines [
              "TERMINA_OSAL_DIR:=" ++ osalDir,
              "PLATFORM:=" ++ show plt,
              "include " ++ mk,
              "doctor-info:",
              "\t$(info CC $(CC))$(info OBJCOPY $(OBJCOPY))$(info CROSS $(value CROSS))@:"
            ]
      result <- try (readCreateProcessWithExitCode (proc "make" ["-s", "--no-print-directory", "-f", "-", "doctor-info"]) script)
      return $ case result :: Either IOException (ExitCode, String, String) of
          Right (ExitSuccess, out, _) ->
              let values = M.fromList [ (k, dropWhile (== ' ') v) | l <- lines out, let (k, v) = break (== ' ') l ]
                  value k = M.findWithDefault "" k values
              in if null (value "CC") then Nothing else Just (Toolchain (value "CC") (value "OBJCOPY") (value "CROSS"))
          _ -> Nothing

-- | The first line of --version of a tool, when it is found.
toolVersion :: String -> IO (Maybe String)
toolVersion tool = do
    found <- findTool tool
    case found of
        Nothing -> return Nothing
        Just path -> do
            v <- firstLineOf path ["--version"]
            return (Just (fromMaybe path v))

-- | The path of a tool given by its path or by its name in PATH.
findTool :: String -> IO (Maybe FilePath)
findTool tool
    | null tool = return Nothing
    | isAbsolute tool = do
        exists <- doesFileExist tool
        return (if exists then Just tool else Nothing)
    | otherwise = findExecutable tool

-- | The first line that a command writes, when it runs and succeeds.
firstLineOf :: FilePath -> [String] -> IO (Maybe String)
firstLineOf cmd args = do
    result <- try (readCreateProcessWithExitCode (proc cmd args) "")
    return $ case result :: Either IOException (ExitCode, String, String) of
        Right (ExitSuccess, out, err) -> listToMaybe (filter (not . all isSpace) (lines out ++ lines err))
        _ -> Nothing

-- | The image of the devcontainer.json of a project against the version of
-- termina.
devcontainerLine :: FilePath -> IO String
devcontainerLine root = do
    let file = root </> ".devcontainer" </> "devcontainer.json"
    exists <- doesFileExist file
    if not exists then return "no .devcontainer/devcontainer.json" else do
      content <- readFile file
      return $ case devcontainerImageTag content of
          Nothing -> "no docker-termina image in .devcontainer/devcontainer.json"
          Just tag -> "docker-termina:" ++ tag ++ " in .devcontainer/devcontainer.json, " ++
              case minorVersion tag of
                  Nothing -> "whose tag has no version to compare"
                  Just v
                    | Just v == minorVersion terminaVersion -> "which matches termina " ++ terminaVersion
                    | otherwise -> "which does not match termina " ++ terminaVersion

-- | The tag of the docker-termina image that a devcontainer.json names. The
-- file may have comments, so it is searched as text.
devcontainerImageTag :: String -> Maybe String
devcontainerImageTag content =
    listToMaybe [ takeWhile (\c -> not (isSpace c) && c /= '"' && c /= '\'') rest
                | t <- tails content, Just rest <- [stripPrefix "docker-termina:" t] ]

-- | The MAJOR.MINOR of a version or of a tag of the image, such as "0.7",
-- "0.7.1" or "v0.3.0".
minorVersion :: String -> Maybe String
minorVersion v =
    let s = fromMaybe v (stripPrefix "v" v)
        parts = T.splitOn (T.pack ".") (T.pack s)
    in case parts of
        (major : minor' : _) | all isNumber [major, minor'] -> Just (T.unpack major ++ "." ++ T.unpack minor')
        _ -> Nothing

  where

    isNumber p = not (T.null p) && T.all isDigit p
