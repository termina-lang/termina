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
import qualified Data.Map as M

-- | Error message formatter
-- Prints error messages in the form "[error] <message>"
errorMessage :: String -> String
errorMessage msg = "\x1b[31m[error]\x1b[0m: " ++ msg

-- | Debug message formatter
-- Prints debug messages in the form "[debug] <message>"
debugMessage :: String -> String
debugMessage msg = "\x1b[32m[debug]\x1b[0m " ++ msg

warnMessage :: String -> String
warnMessage msg = "\x1b[33m[warning]\x1b[0m " ++ msg

getModuleImports :: (MonadIO m) => FilePath -> PAST.TerminaModule ParserAnn -> m (Either ParsingErrors [FilePath])
getModuleImports srcPath m =
    mapM buildAndTest (modules m) <&> sequence
    where

        buildAndTest :: (MonadIO m) => PAST.ModuleImport ParserAnn -> m (Either ParsingErrors FilePath)
        buildAndTest (ModuleImport modName ann) = do
            let mname = buildModuleName ann modName
            case mname of
                Left err -> return $ Left err
                Right qname -> do
                    let importedPath = srcPath </> qname <.> "fin"
                    exists <- liftIO $ doesFileExist importedPath
                    if exists
                        then return $ Right importedPath
                        else
                            return $ Left (annotateError ann (EImportedFileNotFound importedPath))

buildModuleName :: Location -> [String] -> Either ParsingErrors QualifiedName
buildModuleName loc [] = Left $ annotateError loc EEmptyModuleName
buildModuleName loc [x] = Left $ annotateError loc (EInvalidModuleName x)
buildModuleName loc fs = buildModuleName' fs

  where

    buildModuleName' :: [String] -> Either ParsingErrors QualifiedName
    buildModuleName' [] = Left $ annotateError loc EEmptyModuleName
    buildModuleName' [x] = Right x
    buildModuleName' (x:xs) = (x </>) <$> buildModuleName' xs

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
            (importedModules typedModule)
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