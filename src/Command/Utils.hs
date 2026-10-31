module Command.Utils  where

import Command.Types

import System.FilePath

import Lowering

import Modules.Modules

import ControlFlow.BoxUsage (runBoxUsageCheck)
import ControlFlow.VarUsage (runVarUsageCheck)
import ControlFlow.VarScope (runVarScopeCheck)
import ControlFlow.DequeUsage (runDequeUsageCheck)
import ControlFlow.DequeUsage.Errors (DequeUsageError)
import ControlFlow.SideEffects (runSideEffectCheck)
import ControlFlow.SideEffects.Errors (SideEffectsError)
import Configuration.Platform (Platform)
import ControlFlow.ExitPaths
import Configuration.Configuration
import Configuration.Lock (Lock, decodeLockFile)
import Configuration.Manifest
import System.Directory
import Utils.Annotations
import ControlFlow.ExitPaths.Errors (PathsCheckError)
import ControlFlow.BoxUsage.Errors (BoxUsageError)
import ControlFlow.VarUsage.Errors (VarUsageError)
import ControlFlow.VarScope.Errors (VarScopeError)
import Lowering.Errors (LoweringError)
import Elaboration (ElaborationReport, provers, elaborateProgram)
import Elaboration.Prover.Value (valueProver)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Semantic.Environment (DeclaredEnv, declaredNames)
import Utils.Graph (TopSortError(..), topSortFromDepList)
import Utils.Errors (ErrorMessage(toText, errorIdent))
import Control.Monad (forM_, unless, when)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..), die, exitFailure)
import System.IO (stderr)
import System.Environment (getEnvironment)
import Control.Exception (IOException, try)
import System.Process (readCreateProcessWithExitCode)
import Configuration.Fetch (gitProcess)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Text.Printf (printf)

-- | Error message formatter
-- Prints error messages in the form "[error] <message>"
errorMessage :: String -> String
errorMessage msg = "\x1b[31m[error]\x1b[0m: " ++ msg

-- | Debug message formatter
-- Prints debug messages in the form "[debug] <message>"
debugMessage :: String -> String
debugMessage msg = "\x1b[32m[debug]\x1b[0m " ++ msg

-- | Warning message formatter
-- Prints warning messages in the form "[warning] <message>"
warnMessage :: String -> String
warnMessage msg = "\x1b[33m[warning]\x1b[0m " ++ msg

-- | Info message formatter
-- Prints info messages in the form "[info] <message>"
infoMessage :: String -> String
infoMessage msg = "\x1b[34m[info]\x1b[0m " ++ msg

-- | Where termina runs, which decides what git can reach and what the
-- messages suggest.
data Place = Computer | VSCodeContainer | CommandLineContainer
  deriving (Eq, Show)

-- | The image sets TERMINA_CONTAINER, and VS Code sets REMOTE_CONTAINERS in
-- the container it opens.
currentPlace :: IO Place
currentPlace = do
    env <- getEnvironment
    return $ case (lookup "TERMINA_CONTAINER" env, lookup "REMOTE_CONTAINERS" env) of
        (Nothing, _) -> Computer
        (Just _, Just _) -> VSCodeContainer
        (Just _, Nothing) -> CommandLineContainer

getVisibleModules ::  M.Map QualifiedName [QualifiedName] -> [ModuleDependency] -> [QualifiedName]
getVisibleModules prevModsMap importedMods =
  let moduleDependencies = (\(ModuleDependency qname _) -> qname) <$> importedMods
      moduleDependencies' = concatMap (\m ->
        case M.lookup m prevModsMap of
          Nothing -> error $ "Module not found: " ++ m ++ " in project: " ++ show (M.keys prevModsMap)
          Just prevMods -> prevMods) moduleDependencies
  in
  moduleDependencies ++ moduleDependencies'

-- | Identifiers that the whole project declares at its top level, each with the
-- location of its declaration. The type checker needs them before it types the
-- first module, so that a local object that reuses one of these names is
-- rejected whatever the order in which the modules are typed.
projectDeclaredNames :: ParsedProject -> DeclaredEnv
projectDeclaredNames = M.unions . fmap (declaredNames . parsedAST . metadata) . M.elems

-- | Write a generated file only when its content changes, so that make does
-- not compile again what is the same.
writeIfChanged :: FilePath -> T.Text -> IO ()
writeIfChanged path content = do
  exists <- doesFileExist path
  same <- if exists then (== content) <$> TIO.readFile path else return False
  unless same $ do
    createDirectoryIfMissing True (takeDirectory path)
    TIO.writeFile path content

-- | Remove the generated files of a folder that the deployment no longer
-- generates, such as the C of a module that has been deleted.
removeStaleFiles :: FilePath -> [String] -> [FilePath] -> IO ()
removeStaleFiles folder extensions expected = do
  exists <- doesDirectoryExist folder
  when exists $ do
    files <- listFilesRecursive folder
    forM_ files $ \f ->
      when (takeExtension f `elem` extensions && f `notElem` expected) (removeFile f)

  where

    listFilesRecursive dir = do
      entries <- map (dir </>) <$> listDirectory dir
      concat <$> mapM (\e -> do
        isDir <- doesDirectoryExist e
        if isDir then listFilesRecursive e else return [e]) entries

boxUsageCheckModules :: LoweredProject -> Maybe BoxUsageError
boxUsageCheckModules = check . M.elems

    where

        check [] = Nothing
        check [x] = boxUsageCheckModule x
        check (x:xs) =
            case boxUsageCheckModule x of
                Nothing -> check xs
                Just err -> Just err

boxUsageCheckModule :: BasicBlocksModule -> Maybe BoxUsageError
boxUsageCheckModule =
    runBoxUsageCheck . loweredAST . metadata

-- | The modules are checked in dependency order, threading the functions found
-- to carry an effect from one module to the next, so that a call resolves
-- against the function it calls even when that one lives in a module this one
-- imports.
sideEffectCheckModules :: Platform -> ElaboratedProject -> Maybe SideEffectsError
sideEffectCheckModules plt project =
    case sortProjectDepsOrLoop (M.map importedModules project) of
        -- | The build pipeline orders the modules, and reports a cycle, before
        -- reaching this point.
        Left _ -> Nothing
        Right orderedDependencies -> check M.empty orderedDependencies

    where

        check _ [] = Nothing
        check functions (m:ms) =
            case runSideEffectCheck plt functions (elaboratedAST . metadata $ project M.! m) of
                (Nothing, functions') -> check functions' ms
                (Just err, _) -> Just err

varUsageCheckModules :: LoweredProject -> Maybe VarUsageError
varUsageCheckModules = check . M.elems

    where

        check [] = Nothing
        check [x] = varUsageCheckModule x
        check (x:xs) =
            case varUsageCheckModule x of
                Nothing -> check xs
                Just err -> Just err

varUsageCheckModule :: BasicBlocksModule -> Maybe VarUsageError
varUsageCheckModule =
    runVarUsageCheck . loweredAST . metadata

dequeUsageCheckModules :: LoweredProject -> Maybe DequeUsageError
dequeUsageCheckModules = listToMaybe . mapMaybe (runDequeUsageCheck . loweredAST . metadata) . M.elems

varScopeCheckModules :: LoweredProject -> Maybe VarScopeError
varScopeCheckModules = listToMaybe . mapMaybe varScopeCheckModule . M.elems

varScopeCheckModule :: BasicBlocksModule -> Maybe VarScopeError
varScopeCheckModule =
    runVarScopeCheck . loweredAST . metadata

lowerProject :: TypedProject -> Either LoweringError LoweredProject
lowerProject = mapM lowerTypedModule

lowerTypedModule :: TypedModule -> Either LoweringError BasicBlocksModule
lowerTypedModule typedModule = do
    let result = runLowerModule . typedAST . metadata $ typedModule
    case result of
        Left err -> Left err
        Right bbAST -> pure $ TerminaModuleData
            (qualifiedName typedModule)
            (fullPath typedModule)
            (modificationTime typedModule)
            (importedModules typedModule)
            (visibleModules typedModule)
            (sourcecode typedModule)
            (BasicBlockData bbAST)

-- | Elaborates each module of a project, deciding the run-time checks of its
-- operations, with the report of what the provers discharged in each one. The
-- value prover of a module reads what the value analysis showed of it.
elaborateProject :: ProjectValueEvidence -> LoweredProject -> (ElaboratedProject, M.Map QualifiedName ElaborationReport)
elaborateProject evidence loweredProject = (fst <$> elaborated, snd <$> elaborated)

  where

    elaborated = M.mapWithKey elaborateModule loweredProject

    elaborateModule moduleName bbModule =
      let moduleProvers = provers ++ [valueProver (M.findWithDefault M.empty moduleName evidence)]
          (eAST, report) = elaborateProgram moduleProvers (loweredAST . metadata $ bbModule) in
      (bbModule { metadata = ElaboratedData eAST }, report)

basicBlockPathsCheckModules :: LoweredProject -> Maybe PathsCheckError
basicBlockPathsCheckModules = check . M.elems

    where

        check [] = Nothing
        check [x] = basicBlockPathsCheckModule x
        check (x:xs) =
            case basicBlockPathsCheckModule x of
                Nothing -> check xs
                Just err -> Just err

basicBlockPathsCheckModule :: BasicBlocksModule -> Maybe PathsCheckError
basicBlockPathsCheckModule bbModule = do
    let result = runCheckExitPaths . loweredAST . metadata $ bbModule
    case result of
        Left err -> Just err
        Right _ -> Nothing

-- | What a check reports when it fails. Every check raises errors of its own
-- type, and what is asked of all of them is the same, so a failure carries the
-- answers instead of the error itself.
data CheckFailure = CheckFailure
  {
    -- | The @XX-NNN@ code of the error.
    failureCode :: T.Text
    -- | The error as it is shown, which is all an internal error has, since it
    -- carries no position in the source.
  , failureShown :: String
    -- | The message, given the source of every module of the project.
  , failureMessage :: M.Map FilePath T.Text -> T.Text
  }

checkFailure :: (ErrorMessage e, Show e) => e -> CheckFailure
checkFailure err = CheckFailure (errorIdent err) (show err) (toText err)

-- | A check over the basic-block AST of a whole project, with the message that
-- announces it while it runs.
data Check = Check
  {
    checkMessage :: String
  , runCheck :: Platform -> LoweredProject -> Maybe CheckFailure
  }

-- | The checks the basic-block AST goes through, in the order they run. Reading
-- an object that no path has assigned is a more basic mistake than assigning a
-- value that nobody reads, so the usage check goes before the linearity one.
loweredChecks :: [Check]
loweredChecks =
  [
    Check "Checking basic block paths"
      (const (fmap checkFailure . basicBlockPathsCheckModules))
  , Check "Definite assignment checking project modules"
      (const (fmap checkFailure . varUsageCheckModules))
  , Check "Usage checking project modules"
      (const (fmap checkFailure . boxUsageCheckModules))
    -- | The deque check relies on the boxes it sees moved being well formed.
  , Check "Deque checking project modules"
      (const (fmap checkFailure . dequeUsageCheckModules))
    -- | The scope check relies on definite assignment having passed, and a
    -- declaration in too wide a block is the least of the mistakes, so it goes
    -- last.
  , Check "Scope checking project modules"
      (const (fmap checkFailure . varScopeCheckModules))
  ]

-- | The source of each module of a project, which is what the error printer
-- quotes from.
projectSourceFiles :: M.Map QualifiedName (TerminaModuleData a) -> M.Map FilePath T.Text
projectSourceFiles =
  M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) M.empty

-- | Runs every check over the basic-block AST, stopping at the first error.
runLoweredChecks :: Bool -> Platform -> LoweredProject -> IO ()
runLoweredChecks chatty plt loweredProject = mapM_ runOne loweredChecks

  where

    sourceFilesMap = projectSourceFiles loweredProject

    runOne :: Check -> IO ()
    runOne check = do
      when chatty (putStrLn . debugMessage $ checkMessage check)
      case runCheck check plt loweredProject of
        Nothing -> return ()
        Just failure -> dieWithReport (failureMessage failure sourceFilesMap)

-- | Load "termina.yaml" and check the folders of the project against it,
-- giving every problem found.
loadManifest :: (MonadIO m) => m (Either [String] Manifest)
loadManifest = liftIO $ do
    result <- decodeManifestFile "termina.yaml"
    case result of
        Left err -> return (Left ["termina.yaml: " ++ err])
        Right manifest -> do
            layoutErrors <- checkLayout "." manifest
            return $ if null layoutErrors then Right manifest else Left layoutErrors

-- | Load the "termina.lock" of a project, which is empty when there is none.
loadLock :: FilePath -> IO (Either [String] Lock)
loadLock root = either (Left . pure . ("termina.lock: " ++)) Right <$> decodeLockFile (root </> "termina.lock")

-- | Print an error, which may take several lines, and stop.
dieWithError :: String -> IO a
dieWithError = die . errorMessage

-- | Print a report of the transpiler, which carries its own tag, on the
-- error output and stop.
dieWithReport :: T.Text -> IO a
dieWithReport report = TIO.hPutStrLn stderr report >> exitFailure

-- | Print the first error and stop. The errors are reported one at a time,
-- as the type checker does.
dieWithFirstError :: [String] -> IO a
dieWithFirstError = dieWithError . concat . take 1

-- | The configuration of the application of the project.
loadConfig :: (MonadIO m) => m (Either [String] TerminaConfig)
loadConfig = do
    result <- loadManifest
    return $ result >>= maybe (Left ["the project is a library and has no application"]) Right . appConfig

sortProjectDepsOrLoop
  :: ProjectDependencies
  -> Either [ModuleDependency] [QualifiedName]
sortProjectDepsOrLoop = topErrorInternal . M.toList
  where
    topErrorInternal projectDependencies =
      either
        (
          \case {
            ELoop xs -> Left xs;
            e -> error . errorMessage $ "Internal sorting Error: " ++ show e
          }
        )
        Right $ topSortFromDepList projectDependencies

-- | Runs git with the given arguments, in the C locale, or reports why it
-- could not.
runGit :: [String] -> IO (Either String String)
runGit args = do
    p <- gitProcess args
    result <- try (readCreateProcessWithExitCode p "")
    return $ case result of
        Left e -> Left (show (e :: IOException))
        Right (ExitSuccess, out, _) -> Right out
        Right (ExitFailure _, _, err) -> Left err

sha256File :: FilePath -> IO String
sha256File file = concatMap (printf "%02x") . BS.unpack . SHA256.hashlazy <$> BL.readFile file
