{-# LANGUAGE OverloadedStrings #-}

module Command.Sched (
    schedCmdArgsParser, schedCommand, SchedCmdArgs
) where

import Configuration.Configuration
import Command.Utils
import Command.Types

import qualified Options.Applicative as O
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.FilePath
import System.Exit
import System.Directory
import Parser.Errors
import Text.Parsec (runParser)
import qualified Data.Map as M
import Modules.Modules
import Generator.Environment
import Configuration.Platform
import Utils.Errors
import Utils.Annotations
import Text.Parsec.Error
import Semantic.Environment
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Command.Common
import EFP.Schedulability.TransPath.Parsing
import EFP.Schedulability.TransPath.TypeChecking
import qualified Semantic.Types as STYPES
import qualified EFP.Schedulability.TransPath.Types as TTYPES
import ControlFlow.Architecture.Types

-- | Data type for the "sched" command arguments
newtype SchedCmdArgs =
    SchedCmdArgs
        Bool -- ^ Verbose mode
    deriving (Show,Eq)


-- | Parser for the "sched" command arguments
schedCmdArgsParser :: O.Parser SchedCmdArgs
schedCmdArgsParser = SchedCmdArgs
    <$> O.switch (O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable verbose mode")


-- | Load the files containing the transactional worst-case execution paths
loadPathModules
  :: BasicBlocksProject
    -> FilePath
  -> IO TransPathProject
loadPathModules bbProject efpPath = do
  foldM loadPathModules' M.empty (M.elems bbProject)

  where

  loadPathModules' :: TransPathProject
    -- Module whose transactional worst-case execution paths are being loaded
    -> BasicBlocksModule
    -> IO TransPathProject
  loadPathModules' fsLoaded bbModule = do
    let fullP = efpPath </> (qualifiedName bbModule <.> "twcep")
        filePath = qualifiedName bbModule
    sourceFileExists <- doesFileExist fullP
    -- If the file containing the transactional worst-case execution paths
    -- for the module does not exist, skip it
    if not sourceFileExists
      then return fsLoaded
      else do
        loadedModule <- loadPathModule fullP filePath
        pure $ M.insert (qualifiedName loadedModule) loadedModule fsLoaded

  -- | Load file containing transactional worst-case execution paths
  loadPathModule ::
    FilePath
    -> FilePath
    -> IO TransPathModule
  loadPathModule fullP filePath = do
    -- read it
    src_code <- TE.decodeUtf8 <$> BS.readFile fullP
    mod_time <- getModificationTime fullP
    -- parse it
    case runParser terminaTransPathsParser filePath fullP (T.unpack src_code) of
      Left err ->
        let pErr = annotateError (Position filePath (errorPos err) (errorPos err)) (EParseError err)
            fileMap = M.singleton fullP src_code
        in
        TIO.putStrLn (toText pErr fileMap) >> exitFailure
      Right term -> return $ TerminaModuleData filePath fullP mod_time [] [] src_code (TransPathData term)

-- | Type check the transactional worst-case execution paths of the project modules
typePathModules :: TerminaProgArch STYPES.SemanticAnn 
  -> BasicBlocksProject
  -> TransPathProject 
  -> IO (TransPathMap TTYPES.SemanticAnn)
typePathModules arch bbProject pathProject =
  foldM typePathModules' M.empty (M.elems pathProject)

  where

    typePathModules' :: TransPathMap TTYPES.SemanticAnn -> TransPathModule -> IO (TransPathMap TTYPES.SemanticAnn)
    typePathModules' transPathMap transPathModule = do
      let result = runTransPathTypeChecking arch transPathMap (transPathAST . metadata $ transPathModule)
      case result of
        (Left err) ->
          -- | Create the source files map. This map will be used to obtain the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap =
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                    M.empty bbProject
              pathFilesMap =
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                    sourceFilesMap pathProject in
          TIO.putStrLn (toText err pathFilesMap) >> exitFailure
        (Right newMap) -> return newMap

-- | Command handler for the "sched" command
schedCommand :: SchedCmdArgs -> IO ()
schedCommand (SchedCmdArgs chatty) = do
    when chatty (putStrLn . debugMessage $ "Reading project configuration from \"termina.yaml\"")
    -- | Read the termina.yaml file
    config <- loadConfig >>= either (
            \err -> TIO.putStrLn (T.pack $ show err) >> exitFailure
          ) return
    -- | Decode the selected platform field
    plt <- maybe (die . errorMessage $ "Unsupported platform: \"" ++ show (platform config) ++ "\"") return $ checkPlatform (platform config)
    when chatty (putStrLn . debugMessage $ "Selected platform: \"" ++ show plt ++ "\"")
    -- | Check that the files are in place
    existSourceFolder <- doesDirectoryExist (sourceModulesFolder config)
    unless existSourceFolder (die . errorMessage $ "Source folder \"" ++ sourceModulesFolder config ++ "\" does not exist")
    existAppFolder <- doesDirectoryExist (appFolder config)
    unless existAppFolder (die . errorMessage $ "Application folder \"" ++ appFolder config ++ "\" does not exist")
    -- | Create output header and source folder if it does not exist
    let outputSrcFolder = outputFolder config </> "src"
    let outputIncludeFolder = outputFolder config </> "include"
    when chatty (putStrLn . debugMessage $ "Creating output source folder (if missing): \"" ++ outputSrcFolder ++ "\"")
    createDirectoryIfMissing True outputSrcFolder
    when chatty (putStrLn . debugMessage $ "Creating output include folder (if missing): \"" ++ outputIncludeFolder ++ "\"")
    createDirectoryIfMissing True outputIncludeFolder
    -- | Load the main application module
    when chatty (putStrLn . debugMessage $ "Loading application's main module: \"" ++ appFolder config </> appFilename config <.> "fin" ++ "\"")
    appModule <- loadTerminaModule (appFilename config) (appFolder config) (sourceModulesFolder config)
    -- | Load the project
    when chatty (putStrLn . debugMessage $ "Loading project modules")
    parsedModules <- loadModules (importedModules appModule) (sourceModulesFolder config)
    let parsedProject = M.insert (qualifiedName appModule) appModule parsedModules
    -- | Detect any possible loops in the project
    when chatty (putStrLn . debugMessage $ "Ordering project modules")
    let projectDependencies = M.map importedModules parsedProject
    orderedDependencies <-
      either
        (\files ->
          let msg = "\x1b[31m[error]\x1b[0m: Detected cycle between project source files: " <> T.intercalate " -> " (map (T.pack . show) files) in
          TIO.putStrLn msg >> exitFailure)
        return
        $ sortProjectDepsOrLoop projectDependencies
    when chatty (putStrLn. debugMessage $ "Type checking project modules")
    -- | Create the initial global environment
    let initialGlobalEnv = makeInitialGlobalEnv (Just config) (getPlatformInitialGlobalEnv config plt)
    (typedProject, _finalGlobalEnv) <- typeModules parsedProject initialGlobalEnv orderedDependencies
    -- | Obtain the basic blocks AST of the program
    when chatty (putStrLn . debugMessage $ "Obtaining the basic blocks")
    rawBBProject <-
      either
        (\err ->
          TIO.putStrLn (toText err M.empty) >> exitFailure)
        return
        $ genBasicBlocks typedProject
    when chatty (putStrLn . debugMessage $ "Checking basic blocks paths")
    case basicBlockPathsCheckModules rawBBProject of
      Nothing -> return ()
      Just err ->
        let sourceFilesMap =
              M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
              M.empty rawBBProject in
        TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    -- | Usage checking
    when chatty (putStrLn . debugMessage $ "Usage checking project modules")
    case useDefCheckModules rawBBProject of
      Nothing -> return ()
      Just err ->
        let sourceFilesMap =
              M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
              M.empty rawBBProject in
        TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    when chatty (putStrLn . debugMessage $ "Performing constant simplification")
    simplBBProject <- constSimpl rawBBProject
    -- | Obtain the architectural description of the program
    when chatty (putStrLn . debugMessage $ "Checking the architecture of the program")
    programArchitecture <- genArchitecture simplBBProject (getPlatformInitialProgram config plt) orderedDependencies
    checkEmitterConnections simplBBProject programArchitecture
    checkChannelConnections simplBBProject programArchitecture
    checkResourceUsage simplBBProject programArchitecture
    checkPoolUsage simplBBProject programArchitecture
    checkProjectBoxSources simplBBProject programArchitecture
    when chatty (putStrLn . debugMessage $ "Performing depth constant folding")
    bbProject <- constFolding simplBBProject programArchitecture
    -- | Load the transactional worst-case execution paths
    when chatty (putStrLn . debugMessage $ "Loading transactional worst-case execution paths")
    transPathProject <- loadPathModules bbProject (efpFolder config)
    transPathMap <- typePathModules programArchitecture bbProject transPathProject
    when chatty (putStrLn . debugMessage $ "Transactional worst-case execution paths type checked successfully")
    when chatty (putStrLn . debugMessage $ "Scheduling models generated successfully")
