module Command.Utils  where

import Command.Types

import System.FilePath
import System.Exit

import qualified Data.Map.Strict as M

import qualified Parser.AST as PAST
import Core.AST

import Parser.Types

import ControlFlow.BasicBlocks

import Modules.Modules

import ControlFlow.VarUsage (runUDAnnotatedProgram)
import ControlFlow.BasicBlocks.Checks.ExitPaths
import qualified ControlFlow.BasicBlocks.Checks.ExitPaths.PPrinting as EPErrors
import qualified ControlFlow.VarUsage.Errors.PPrinting as VUErrors
import Configuration.Configuration
import Data.Yaml
import System.Directory
import qualified Command.Errors.PPrinting as CE
import Command.Errors.Errors (Error(EImportedFileNotFound))
import Utils.Annotations
import qualified Data.Text.Lazy as TL

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

getModuleImports :: FilePath -> FilePath -> TL.Text -> PAST.TerminaModule ParserAnn -> IO [FilePath]
getModuleImports filePath srcPath src_code m = 
    mapM buildAndTest (modules m)

    where

        modulePath = filePath <.> "fin"

        buildAndTest :: PAST.ModuleImport ParserAnn -> IO FilePath
        buildAndTest (ModuleImport modName ann) = do
            qname <- buildModuleName modName
            let importedPath = srcPath </> qname <.> "fin"
            exists <- doesFileExist importedPath
            if exists
                then return qname
                else
                    let sourceFilesMap = M.fromList [(modulePath, src_code)] in
                    CE.ppError sourceFilesMap (annotateError ann (EImportedFileNotFound importedPath)) >> exitFailure

buildModuleName :: [String] -> IO QualifiedName
buildModuleName [] = die . errorMessage $ "Internal parsing error: empty module name"
buildModuleName [x] = die . errorMessage $ "Inalid module name \"" ++ show x ++ "\": modules cannot be at the root of the source folder"
buildModuleName fs = buildModuleName' fs

  where

    buildModuleName' :: [String] -> IO QualifiedName
    buildModuleName' [] = error . errorMessage $ "Internal parsing error: empty module name"
    buildModuleName' [x] = pure x
    buildModuleName' (x:xs) = (x </>) <$> buildModuleName' xs

useDefCheckModule :: BasicBlocksModule -> IO ()
useDefCheckModule bbModule =
    let result = runUDAnnotatedProgram . basicBlocksAST . metadata $ bbModule in
    case result of
    Nothing -> return ()
    Just err -> 
        let sourceFilesMap = M.fromList [(fullPath bbModule, sourcecode bbModule)] in
        VUErrors.ppError sourceFilesMap err >> exitFailure

genBasicBlocksModule :: TypedModule -> IO BasicBlocksModule
genBasicBlocksModule typedModule = do
    let result = runGenBBModule . typedAST . metadata $ typedModule
    case result of
        Left err -> die . errorMessage $ show err
        Right bbAST -> pure $ TerminaModuleData
            (qualifiedName typedModule)
            (fullPath typedModule)
            (importedModules typedModule)
            (sourcecode typedModule)
            (BasicBlockData bbAST)

checkBasicBlocksPaths :: BasicBlocksModule -> IO ()
checkBasicBlocksPaths bbModule = do
    let result = runCheckExitPaths . basicBlocksAST . metadata $ bbModule
    case result of
        Left err -> 
            let sourceFilesMap = M.fromList [(fullPath bbModule, sourcecode bbModule)] in
            EPErrors.ppError sourceFilesMap err >> exitFailure
        Right _ -> return ()

-- | Load "termina.yaml" configuration file
loadConfig :: IO TerminaConfig
loadConfig = do
    config <- decodeFileEither "termina.yaml"
    case config of
        Left (InvalidYaml (Just (YamlException err))) -> die . errorMessage $ err
        Left err -> die . errorMessage $ show err
        Right c -> return c

serializeConfig :: FilePath -> TerminaConfig -> IO ()
serializeConfig filePath config = do
    encodeFile (filePath </> "termina" <.> "yaml") config