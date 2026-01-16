module Command.Utils  where

import Command.Types

import System.FilePath

import qualified Parser.AST as PAST
import Core.AST

import Parser.Types

import ControlFlow.BasicBlocks

import Modules.Modules

import ControlFlow.VarUsage (runUDAnnotatedProgram)
import ControlFlow.BasicBlocks.Checks.ExitPaths
import Configuration.Configuration
import Data.Yaml
import System.Directory
import Utils.Annotations
import ControlFlow.BasicBlocks.Checks.ExitPaths.Errors (PathsCheckError)
import ControlFlow.VarUsage.Errors (VarUsageError)
import ControlFlow.BasicBlocks.Errors (BBGeneratorError)
import Parser.Errors
import Control.Monad.IO.Class
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Extras.Graph (TopSortError(..), topSortFromDepList)
import Modules.Utils
import Data.Time (UTCTime)

-- | Error message formatter
-- Prints error messages in the form "[error] <message>"
errorMessage :: String -> String
errorMessage msg = "\x1b[31m[error]\x1b[0m: " ++ msg

-- | Debug message formatter
-- Prints debug messages in the form "[debug] <message>"
debugMessage :: String -> String
debugMessage msg = "\x1b[32m[debug]\x1b[0m " ++ msg

-- | Warning message formatter
-- Prints warning messages in the form "[warning] <message>"
warnMessage :: String -> String
warnMessage msg = "\x1b[33m[warning]\x1b[0m " ++ msg

-- | Info message formatter
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

useDefCheckModules :: BasicBlocksProject -> Maybe VarUsageError
useDefCheckModules = check . M.elems

    where

        check [] = Nothing
        check [x] = useDefCheckModule x
        check (x:xs) =
            case useDefCheckModule x of
                Nothing -> check xs
                Just err -> Just err

useDefCheckModule :: BasicBlocksModule -> Maybe VarUsageError
useDefCheckModule =
    runUDAnnotatedProgram . basicBlocksAST . metadata

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