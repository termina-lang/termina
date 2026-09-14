module Command.Utils  where

import Command.Types

import System.FilePath

import qualified Parser.AST as PAST
import Core.AST

import Parser.Types

import ControlFlow.BasicBlocks

import Modules.Modules

import ControlFlow.BoxUsage (runBoxUsageCheck)
import ControlFlow.VarUsage (runVarUsageCheck)
import ControlFlow.SideEffects (runSideEffectCheck)
import ControlFlow.SideEffects.Errors (SideEffectsError)
import Configuration.Platform (Platform)
import ControlFlow.BasicBlocks.Checks.ExitPaths
import Configuration.Configuration
import Data.Yaml
import System.Directory
import Utils.Annotations
import ControlFlow.BasicBlocks.Checks.ExitPaths.Errors (PathsCheckError)
import ControlFlow.BoxUsage.Errors (BoxUsageError)
import ControlFlow.VarUsage.Errors (VarUsageError)
import ControlFlow.BasicBlocks.Errors (BBGeneratorError)
import Parser.Errors
import Control.Monad.IO.Class
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Utils.Graph (TopSortError(..), topSortFromDepList)
import Utils.Errors (ErrorMessage(toText, errorIdent))
import Modules.Utils
import Data.Time (UTCTime)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

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

getVisibleModules ::  M.Map QualifiedName [QualifiedName] -> [ModuleDependency] -> [QualifiedName]
getVisibleModules prevModsMap importedMods =
  let moduleDependencies = (\(ModuleDependency qname _) -> qname) <$> importedMods
      moduleDependencies' = concatMap (\m ->
        case M.lookup m prevModsMap of
          Nothing -> error $ "Module not found: " ++ m ++ " in project: " ++ show (M.keys prevModsMap)
          Just prevMods -> prevMods) moduleDependencies
  in
  moduleDependencies ++ moduleDependencies'

changedDependendencies :: BasicBlocksProject -> UTCTime -> [QualifiedName] -> IO Bool
changedDependendencies _ _ [] = return False
changedDependendencies bbProject t (x:xs) = do
  let dep = bbProject M.! x
      depModTime = modificationTime dep
  if depModTime > t then 
    return True
  else
    changedDependendencies bbProject t xs

getModuleImports :: Maybe FilePath -> PAST.TerminaModule ParserAnn -> IO (Either ParsingErrors [ModuleDependency])
getModuleImports (Just srcPath) m =
    mapM buildAndTest (modules m) <&> sequence
    where

        buildAndTest :: PAST.ModuleImport ParserAnn -> IO (Either ParsingErrors ModuleDependency)
        buildAndTest (ModuleImport modName ann) = do
            let mname = buildModuleName ann modName
            case mname of
                Left err -> return $ Left err
                Right qname -> do
                    let importedPath = srcPath </> qname <.> "fin"
                    exists <- doesFileExist importedPath
                    if exists
                        then return $ Right (ModuleDependency qname ann)
                        else
                            return $ Left (annotateError ann (EImportedFileNotFound importedPath))
getModuleImports Nothing m =
    case modules m of
        [] -> return $ Right []
        ((ModuleImport modName ann):_) -> do
            let mname = buildModuleName ann modName
            case mname of
                Left err -> return $ Left err
                Right qname -> do
                    return $ Left (annotateError ann (EImportedFileNotFound (qname <.> "fin")))

boxUsageCheckModules :: BasicBlocksProject -> Maybe BoxUsageError
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
    runBoxUsageCheck . basicBlocksAST . metadata

sideEffectCheckModules :: Platform -> BasicBlocksProject -> Maybe SideEffectsError
sideEffectCheckModules plt = check . M.elems

    where

        check [] = Nothing
        check [x] = sideEffectCheckModule plt x
        check (x:xs) =
            case sideEffectCheckModule plt x of
                Nothing -> check xs
                Just err -> Just err

sideEffectCheckModule :: Platform -> BasicBlocksModule -> Maybe SideEffectsError
sideEffectCheckModule plt =
    runSideEffectCheck plt . basicBlocksAST . metadata

varUsageCheckModules :: BasicBlocksProject -> Maybe VarUsageError
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
    runVarUsageCheck . basicBlocksAST . metadata

genBasicBlocks :: TypedProject -> Either BBGeneratorError BasicBlocksProject
genBasicBlocks = mapM genBasicBlocksModule

genBasicBlocksModule :: TypedModule -> Either BBGeneratorError BasicBlocksModule
genBasicBlocksModule typedModule = do
    let result = runGenBBModule . typedAST . metadata $ typedModule
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

basicBlockPathsCheckModules :: BasicBlocksProject -> Maybe PathsCheckError
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
    let result = runCheckExitPaths . basicBlocksAST . metadata $ bbModule
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
  , runCheck :: Platform -> BasicBlocksProject -> Maybe CheckFailure
  }

-- | The checks the basic-block AST goes through, in the order they run. Reading
-- an object that no path has assigned is a more basic mistake than assigning a
-- value that nobody reads, so the usage check goes before the linearity one.
basicBlockChecks :: [Check]
basicBlockChecks =
  [
    Check "Checking basic block paths"
      (const (fmap checkFailure . basicBlockPathsCheckModules))
  , Check "Definite assignment checking project modules"
      (const (fmap checkFailure . varUsageCheckModules))
  , Check "Usage checking project modules"
      (const (fmap checkFailure . boxUsageCheckModules))
  , Check "Side-effect checking project modules"
      (\plt -> fmap checkFailure . sideEffectCheckModules plt)
  ]

-- | The source of each module of a project, which is what the error printer
-- quotes from.
projectSourceFiles :: BasicBlocksProject -> M.Map FilePath T.Text
projectSourceFiles =
  M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) M.empty

-- | Runs every check over the basic-block AST, stopping at the first error.
runBasicBlockChecks :: Bool -> Platform -> BasicBlocksProject -> IO ()
runBasicBlockChecks chatty plt bbProject = mapM_ runOne basicBlockChecks

  where

    sourceFilesMap = projectSourceFiles bbProject

    runOne :: Check -> IO ()
    runOne check = do
      when chatty (putStrLn . debugMessage $ checkMessage check)
      case runCheck check plt bbProject of
        Nothing -> return ()
        Just failure -> TIO.putStrLn (failureMessage failure sourceFilesMap) >> exitFailure

-- | Load "termina.yaml" configuration file
loadConfig :: (MonadIO m) => m (Either ParseException TerminaConfig)
loadConfig =
    liftIO $ decodeFileEither "termina.yaml"

serializeConfig :: (MonadIO m) => FilePath -> TerminaConfig -> m ()
serializeConfig filePath config = do
    liftIO $ encodeFile (filePath </> "termina" <.> "yaml") config

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