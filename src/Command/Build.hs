{-# LANGUAGE OverloadedStrings #-}

module Command.Build (
    buildCmdArgsParser, buildCommand, BuildCmdArgs
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

import qualified Data.Map as M
import Modules.Modules
import Generator.Monadic
import Data.List (foldl')
import Generator.CodeGen.Module (runGenSourceFile, runGenHeaderFile)
import Generator.LanguageC.Printer (runCPrinter)
import Generator.CodeGen.Application.Initialization (runGenInitFile)
import Generator.CodeGen.Application.Option (runGenOptionHeaderFile)
import Core.AST
    ( TerminaType'(TEnum, TStruct) )
import Generator.Environment
import Generator.CodeGen.Platform
import Configuration.Platform
import Utils.Errors
import Utils.Annotations
import Semantic.Environment
import qualified Data.Set as S
import Generator.CodeGen.Application.Status (runGenStatusHeaderFile)
import Generator.CodeGen.Application.Result (runGenResultHeaderFile)
import System.Environment
import EFP.Schedulability.WCEPath.Generator (genTransactionalWCEPS)
import EFP.Schedulability.WCEPath.Printer
import Command.Common

-- | Data type for the "new" command arguments
data BuildCmdArgs =
    BuildCmdArgs
        Bool -- ^ Verbose mode
        Bool -- ^ Generate transactional worst-case execution paths
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
buildCmdArgsParser :: O.Parser BuildCmdArgs
buildCmdArgsParser = BuildCmdArgs
    <$> O.switch (O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable verbose mode")
    <*> O.switch (O.long "gen-transactional-wceps"
        <> O.help "Generate transactional worst-case execution paths")  

monadicTypesMapModules :: TypedProject -> MonadicTypes
monadicTypesMapModules = foldl' monadicTypesMapModule emptyMonadicTypes  . M.elems

  where
    monadicTypesMapModule :: MonadicTypes -> TypedModule -> MonadicTypes
    monadicTypesMapModule prevMap typedModule = runMapMonadicTypesAnnotatedProgram prevMap (typedAST . metadata $ typedModule)
  
genTWCEPS ::
  TerminaConfig
  -> BasicBlocksProject
  -> IO ()
genTWCEPS params bbProject = do
  mapM_ printWCEPModule (M.elems bbProject) 

  where

    printWCEPModule :: BasicBlocksModule -> IO ()
    printWCEPModule bbModule = do
      let destinationPath = efpFolder params
          twcepFile = destinationPath </> qualifiedName bbModule <.> "twcep"
          bbAST = basicBlocksAST . metadata $ bbModule
          wceps = genTransactionalWCEPS bbAST
      createDirectoryIfMissing True (takeDirectory twcepFile)
      TIO.writeFile twcepFile $ runWCEPathPrinter wceps



genModules ::
  TerminaConfig
  -> Platform
  -- | The map with the option types to generate from defined types 
  -> MonadicTypes
  -- | The project to generate the code from
  -> BasicBlocksProject -> IO ()
genModules params plt initialMonadicTypes bbProject =
  foldM_ printModule initialMonadicTypes (M.elems bbProject)

  where

    printModule :: MonadicTypes -> BasicBlocksModule -> IO MonadicTypes
    printModule currentMonadicTypes bbModule = do
      let destinationPath = outputFolder params
          sourceFile = destinationPath </> "src" </> qualifiedName bbModule <.> "c"
          headerFile = destinationPath </> "include" </> qualifiedName bbModule <.> "h"
      sourceFileExists <- doesFileExist sourceFile
      -- | Get the modification time of the transpiler itself
      exePath <- getExecutablePath
      exeModificationTime <- getModificationTime exePath
      terminaYamlModificationTime <- getModificationTime "termina.yaml"
      if sourceFileExists
        then do
          sourceFileTime <- getModificationTime sourceFile
          depChanged <- changedDependendencies bbProject sourceFileTime (visibleModules bbModule)
          if sourceFileTime > modificationTime bbModule &&
             sourceFileTime > exeModificationTime &&
             sourceFileTime > terminaYamlModificationTime &&
             not depChanged
            then return ()
          else do
            printSource bbModule
      else do
        printSource bbModule
      headerFileExists <- doesFileExist headerFile
      if headerFileExists
        then do
          headerFileTime <- getModificationTime headerFile
          depChanged <- changedDependendencies bbProject headerFileTime (visibleModules bbModule)
          if headerFileTime > modificationTime bbModule &&
             headerFileTime > exeModificationTime &&
             headerFileTime > terminaYamlModificationTime &&
             not depChanged
            then return currentMonadicTypes
          else do
            printHeader currentMonadicTypes bbModule
        else do
          printHeader currentMonadicTypes bbModule

    printSource :: BasicBlocksModule -> IO ()
    printSource bbModule = do
      let destinationPath = outputFolder params
          sourceFile = destinationPath </> "src" </> qualifiedName bbModule <.> "c"
          tAST = basicBlocksAST . metadata $ bbModule
      case runGenSourceFile params (getPlatformInterruptMap plt) (qualifiedName bbModule) tAST of
        Left err -> die. errorMessage $ show err
        Right cSourceFile -> do
          createDirectoryIfMissing True (takeDirectory sourceFile)
          TIO.writeFile sourceFile $ runCPrinter (profile params == Debug) cSourceFile

    printHeader :: MonadicTypes -> BasicBlocksModule -> IO MonadicTypes
    printHeader currentMonadicTypes bbModule = do
      let destinationPath = outputFolder params
          tAST = basicBlocksAST . metadata $ bbModule
          moduleDeps = (\(ModuleDependency qname _) -> qname) <$> importedModules bbModule
      case runGenHeaderFile params (getPlatformInterruptMap plt) (qualifiedName bbModule) moduleDeps tAST currentMonadicTypes of
        Left err -> die . errorMessage $ show err
        Right (cHeaderFile, newMonadicTypes) -> do
          let headerFile = destinationPath </> "include" </> qualifiedName bbModule <.> "h"
          createDirectoryIfMissing True (takeDirectory headerFile)
          TIO.writeFile headerFile $ runCPrinter (profile params == Debug) cHeaderFile
          return newMonadicTypes

genInitFile :: TerminaConfig -> Platform -> BasicBlocksProject -> QualifiedName -> IO ()
genInitFile params plt bbProject appModName = do
  let appModule = bbProject M.! appModName
  initFileExists <- doesFileExist initFile
  if initFileExists then do
    initFileTime <- getModificationTime initFile
    exePath <- getExecutablePath
    exeModificationTime <- getModificationTime exePath
    terminaYamlModificationTime <- getModificationTime "termina.yaml"
    depChanged <- changedDependendencies bbProject initFileTime (visibleModules appModule)
    unless (initFileTime > modificationTime appModule &&
      initFileTime > exeModificationTime &&
      initFileTime > terminaYamlModificationTime &&
      not depChanged) runGenInitFile'
  else
    runGenInitFile'

  where

    destinationPath = outputFolder params

    initFile = destinationPath </> "init" <.> "c"

    runGenInitFile' :: IO ()
    runGenInitFile' = do
      let projectModules = M.toList $ basicBlocksAST . metadata <$> bbProject
      case runGenInitFile params (getPlatformInterruptMap plt) initFile projectModules of
        Left err -> die . errorMessage $ show err
        Right cInitFile -> do
          createDirectoryIfMissing True (takeDirectory initFile)
          TIO.writeFile initFile $ runCPrinter False cInitFile


genOptionHeaderFile :: TerminaConfig -> Platform -> MonadicTypes -> BasicBlocksProject -> QualifiedName -> IO ()
genOptionHeaderFile params plt monadicTypes bbProject appModName = do
  let appModule = bbProject M.! appModName
  optionFileExists <- doesFileExist optionFile
  if optionFileExists then do
    optionFileTime <- getModificationTime optionFile
    exePath <- getExecutablePath
    exeModificationTime <- getModificationTime exePath
    terminaYamlModificationTime <- getModificationTime "termina.yaml"
    depChanged <- changedDependendencies bbProject optionFileTime (visibleModules appModule)
    unless (optionFileTime > modificationTime appModule &&
      optionFileTime > exeModificationTime &&
      optionFileTime > terminaYamlModificationTime &&
      not depChanged) runGenOptionHeaderFile'
  else
    runGenOptionHeaderFile'

  where

    destinationPath = outputFolder params

    optionFile = destinationPath </> "include" </> "option" <.> "h"

    runGenOptionHeaderFile' :: IO ()
    runGenOptionHeaderFile' = do
      case runGenOptionHeaderFile params (getPlatformInterruptMap plt) optionFile monadicTypes of
        Left err -> die . errorMessage $ show err
        Right cOptionsFile -> TIO.writeFile optionFile $ runCPrinter (profile params == Debug) cOptionsFile

genStatusHeaderFile :: TerminaConfig -> Platform -> MonadicTypes -> BasicBlocksProject -> QualifiedName -> IO ()
genStatusHeaderFile params plt monadicTypes bbProject appModName = do
  let appModule = bbProject M.! appModName
  statusFileExists <- doesFileExist statusFile
  if statusFileExists then do
    statusFileTime <- getModificationTime statusFile
    exePath <- getExecutablePath
    exeModificationTime <- getModificationTime exePath
    terminaYamlModificationTime <- getModificationTime "termina.yaml"
    depChanged <- changedDependendencies bbProject statusFileTime (visibleModules appModule)
    unless (statusFileTime > modificationTime appModule &&
      statusFileTime > exeModificationTime &&
      statusFileTime > terminaYamlModificationTime &&
      not depChanged) runGenStatusHeaderFile'
  else
    runGenStatusHeaderFile'

  where

    destinationPath = outputFolder params

    statusFile = destinationPath </> "include" </> "status" <.> "h"

    runGenStatusHeaderFile' :: IO ()
    runGenStatusHeaderFile' = case runGenStatusHeaderFile params (getPlatformInterruptMap plt) statusFile monadicTypes of
      Left err -> die . errorMessage $ show err
      Right cOptionsFile -> TIO.writeFile statusFile $ runCPrinter (profile params == Debug) cOptionsFile

genResultHeaderFile :: TerminaConfig -> Platform -> MonadicTypes -> BasicBlocksProject -> QualifiedName -> IO ()
genResultHeaderFile params plt monadicTypes bbProject appModName = do
  let appModule = bbProject M.! appModName
  resultFileExists <- doesFileExist resultFile
  if resultFileExists then do
    resultFileTime <- getModificationTime resultFile
    exePath <- getExecutablePath
    exeModificationTime <- getModificationTime exePath
    terminaYamlModificationTime <- getModificationTime "termina.yaml"
    depChanged <- changedDependendencies bbProject resultFileTime (visibleModules appModule)
    unless (resultFileTime > modificationTime appModule &&
      resultFileTime > exeModificationTime &&
      resultFileTime > terminaYamlModificationTime &&
      not depChanged) runGenResultHeaderFile'
  else
    runGenResultHeaderFile'

  where

    destinationPath = outputFolder params

    resultFile = destinationPath </> "include" </> "result" <.> "h"

    runGenResultHeaderFile' :: IO ()
    runGenResultHeaderFile' =
      case runGenResultHeaderFile params (getPlatformInterruptMap plt) resultFile monadicTypes of
        Left err -> die . errorMessage $ show err
        Right cOptionsFile -> TIO.writeFile resultFile $ runCPrinter (profile params == Debug) cOptionsFile

-- | Command handler for the "build" command
buildCommand :: BuildCmdArgs -> IO ()
buildCommand (BuildCmdArgs chatty genTransactionalWCEPs) = do
    when chatty (putStrLn . debugMessage $ "Reading project configuration from \"termina.yaml\"")
    -- | Read the termina.yaml file
    config <- loadConfig >>= either (
            \err -> TIO.putStrLn (T.pack $ show err) >> exitFailure
          ) return
    -- | Decode the selected platform field
    plt <- maybe (die . errorMessage $ "Unsupported platform: \"" ++ show (platform config) ++ "\"") return $ checkPlatform (T.unpack (platform config))
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
    -- | Obtain the set of option types
    when chatty (putStrLn . debugMessage $ "Searching for option types")
    let monadicTypes = monadicTypesMapModules typedProject
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
    -- | Generate the code
    when chatty (putStrLn . debugMessage $ "Generating code")
    genModules config plt monadicTypes bbProject
    genInitFile config plt bbProject (qualifiedName appModule)
    genPlatformCode config plt bbProject (qualifiedName appModule) programArchitecture
    unless (S.null (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . optionTypes $ monadicTypes)) $ genOptionHeaderFile config plt monadicTypes bbProject (qualifiedName appModule)
    unless (S.null (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . statusTypes $ monadicTypes)) $ genStatusHeaderFile config plt monadicTypes bbProject (qualifiedName appModule)
    unless (S.null (S.unions . M.elems . M.filterWithKey (\k _ -> case k of {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . resultTypes $ monadicTypes)) $ genResultHeaderFile config plt monadicTypes bbProject (qualifiedName appModule)
    when genTransactionalWCEPs $ 
      when chatty (putStrLn . debugMessage $ "Generating transactional worst-case execution paths") >>
      genTWCEPS config bbProject
    when chatty (putStrLn . debugMessage $ "Build completed successfully")
