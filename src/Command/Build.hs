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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Semantic.AST as SAST

import System.FilePath
import System.Exit
import System.Directory
import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import qualified Data.Map.Strict as M
import Extras.TopSort
import Semantic.Types (SemanticAnn)
import Semantic.Monad (Environment, makeInitialGlobalEnv)
import Modules.Modules
import Generator.Option (OptionMap, runMapOptionsAnnotatedProgram)
import Data.List (foldl')
import Generator.CodeGen.Module (runGenSourceFile, runGenHeaderFile)
import Generator.LanguageC.Printer (runCPrinter)
import Generator.CodeGen.Application.Initialization (runGenInitFile)
import Generator.CodeGen.Application.Option (runGenOptionHeaderFile)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import qualified Semantic.Errors.PPrinting as SE
import qualified ControlFlow.Architecture.Errors.PPrinting as AE
import ControlFlow.Architecture
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Checks
import Core.AST
import Generator.Environment
import Generator.Platform
import Configuration.Platform

-- | Data type for the "new" command arguments
newtype BuildCmdArgs =
    BuildCmdArgs
        Bool -- ^ Verbose mode
    deriving (Show,Eq)

-- | Parser for the "new" command arguments
buildCmdArgsParser :: O.Parser BuildCmdArgs
buildCmdArgsParser = BuildCmdArgs
    <$> O.switch (O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable verbose mode")

-- | Load Termina file 
loadTerminaModule ::
  FilePath
  -- | Path of the file to load
  -> FilePath
  -- | Path of the source folder that stores the imported modules
  -> FilePath
  -> IO ParsedModule
loadTerminaModule root filePath srcPath = do
  let fullP = root </> filePath <.> "fin"
  -- read it
  src_code <- TLIO.readFile fullP
  -- parse it
  case runParser terminaModuleParser () fullP (TL.unpack src_code) of
    Left err -> die . errorMessage $ "Parsing error: " ++ show err
    Right term -> do
      imports <- getModuleImports filePath srcPath src_code term
      return $ TerminaModuleData filePath fullP imports src_code (ParsingData . frags $ term)

-- | Load the modules of the project
loadModules
  :: [QualifiedName]
  -> FilePath
  -> IO ParsedProject
loadModules imported srcPath = do
  -- | Load and parse the project.
  -- The main application module has been already loaded. We need to load the
  -- rest of the modules.
  loadModules' M.empty imported

  where

  loadModules' :: ParsedProject
    -- Modules to load
    -> [FilePath]
    -> IO ParsedProject
  loadModules' fsLoaded [] = pure fsLoaded
  loadModules' fsLoaded (fs:fss) =
    if M.member fs fsLoaded
    -- Nothing to do, skip to the next one. It could be the case of a module
    -- imported from several files.
    then loadModules' fsLoaded fss
    -- Import and load it.
    else do
      loadedModule <- loadTerminaModule srcPath fs srcPath
      let deps = importedModules loadedModule
      loadModules'
        (M.insert fs loadedModule fsLoaded)
        (fss ++ deps)

sortProjectDepsOrLoop
  :: ProjectDependencies
  -> Either [QualifiedName] [QualifiedName]
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

typeModules :: ParsedProject -> Environment -> [QualifiedName] -> IO (TypedProject, Environment)
typeModules parsedProject =
  typeModules' M.empty

  where

    typeModules' :: TypedProject -> Environment -> [QualifiedName] -> IO (TypedProject, Environment)
    typeModules' typedProject finalState [] = pure (typedProject, finalState)
    typeModules' typedProject prevState (m:ms) = do
      let parsedModule = parsedProject M.! m
      let result = runTypeChecking prevState (typeTerminaModule . parsedAST . metadata $ parsedModule)
      case result of
        (Left err) ->
          -- | Create the source files map. This map will be used to obtain the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap = 
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                    M.empty parsedProject in
          SE.ppError sourceFilesMap err >> exitFailure
        (Right (typedProgram, newState)) -> do
          let typedModule =
                TerminaModuleData
                  (qualifiedName parsedModule)
                  (fullPath parsedModule)
                  (importedModules parsedModule)
                  (sourcecode parsedModule)
                  (SemanticData typedProgram)
          typeModules' (M.insert m typedModule typedProject) newState ms

useDefCheckModules :: BasicBlocksProject -> IO ()
useDefCheckModules = mapM_ useDefCheckModule . M.elems

optionMapModules :: TypedProject -> (OptionMap, OptionMap)
optionMapModules = M.partitionWithKey
                (\k _ -> case k of
                  SAST.TStruct {} -> False
                  SAST.TEnum {} -> False
                  _ -> True) . foldl' optionMapModule M.empty . M.elems

  where
    optionMapModule :: OptionMap -> TypedModule -> OptionMap
    optionMapModule prevMap typedModule = runMapOptionsAnnotatedProgram prevMap (typedAST . metadata $ typedModule)

genModules ::
  TerminaConfig
  -- | Whether to include the option.h file or not
  -> Bool
  -- | The map with the option types to generate from defined types 
  -> OptionMap
  -- | The project to generate the code from
  -> BasicBlocksProject -> IO ()
genModules params includeOptionH definedTypesOptionMap =
  mapM_ printModule . M.elems

  where

    printModule :: BasicBlocksModule -> IO ()
    printModule bbModule = do
      let destinationPath = outputFolder params
          sourceFile = destinationPath </> "src" </> qualifiedName bbModule <.> "c"
          tAST = basicBlocksAST . metadata $ bbModule
      case runGenSourceFile params (qualifiedName bbModule) tAST of
        Left err -> die. errorMessage $ show err
        Right cSourceFile -> do
          createDirectoryIfMissing True (takeDirectory sourceFile)
          TIO.writeFile sourceFile $ runCPrinter (profile params == Debug) cSourceFile
      case runGenHeaderFile params includeOptionH (qualifiedName bbModule) (importedModules bbModule) tAST definedTypesOptionMap of
        Left err -> die . errorMessage $ show err
        Right cHeaderFile -> do
          let headerFile = destinationPath </> "include" </> qualifiedName bbModule <.> "h"
          createDirectoryIfMissing True (takeDirectory headerFile)
          TIO.writeFile headerFile $ runCPrinter (profile params == Debug) cHeaderFile

genInitFile :: TerminaConfig -> BasicBlocksProject -> IO ()
genInitFile params bbProject = do
  let destinationPath = outputFolder params
      initFilePath = destinationPath </> "init" <.> "c"
      projectModules = M.toList $ basicBlocksAST . metadata <$> bbProject
  case runGenInitFile params initFilePath projectModules of
    Left err -> die . errorMessage $ show err
    Right cInitFile -> TIO.writeFile initFilePath $ runCPrinter (profile params == Debug) cInitFile

genOptionHeaderFile :: TerminaConfig -> OptionMap -> IO ()
genOptionHeaderFile params basicTypesOptionMap = do
  let destinationPath = outputFolder params
      optionsFilePath = destinationPath </> "include" </> "options" <.> "h"
  case runGenOptionHeaderFile params basicTypesOptionMap of
    Left err -> die . errorMessage $ show err
    Right cOptionsFile -> TIO.writeFile optionsFilePath $ runCPrinter (profile params == Debug) cOptionsFile

genArchitecture :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> [QualifiedName] -> IO (TerminaProgArch SemanticAnn)
genArchitecture bbProject initialTerminaProgram orderedDependencies = do
  genArchitecture' initialTerminaProgram orderedDependencies

  where

    genArchitecture' :: TerminaProgArch SemanticAnn -> [QualifiedName] -> IO (TerminaProgArch SemanticAnn)
    genArchitecture' tp [] = pure tp
    genArchitecture' tp (m:ms) = do
      let typedModule = basicBlocksAST . metadata $ bbProject M.! m
      let result = runGenArchitecture tp m typedModule
      case result of
        Left err -> 
          -- | Create the source files map. This map will be used to obtainn the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap = 
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                    M.empty bbProject in
          AE.ppError sourceFilesMap err >> exitFailure
        Right tp' -> genArchitecture' tp' ms

checkEmitterConnections :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkEmitterConnections bbProject progArchitecture = 
  let result = runCheckEmitterConnections progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      AE.ppError sourceFilesMap err >> exitFailure
    Right _ -> return ()

checkChannelConnections :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkChannelConnections bbProject progArchitecture = 
  let result = runCheckChannelConnections progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      AE.ppError sourceFilesMap err >> exitFailure
    Right _ -> return ()

checkResourceUsage :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkResourceUsage bbProject progArchitecture = 
  let result = runCheckResourceUsage progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      AE.ppError sourceFilesMap err >> exitFailure
    Right _ -> return ()

checkPoolUsage :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkPoolUsage bbProject progArchitecture = 
  let result = runCheckPoolUsage progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      AE.ppError sourceFilesMap err >> exitFailure
    Right _ -> return ()

genBasicBlocks :: TypedProject -> IO BasicBlocksProject
genBasicBlocks = mapM genBasicBlocksModule

checkProjectBasicBlocksPaths :: BasicBlocksProject -> IO ()
checkProjectBasicBlocksPaths = mapM_ checkBasicBlocksPaths . M.elems

checkProjectBoxSources :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkProjectBoxSources bbProject progArchitecture = 
  let result = runCheckBoxSources progArchitecture in
  case result of
    Left err -> 
      -- | Create the source files map. This map will be used to obtainn the source files that
      -- will be feed to the error pretty printer. The source files map must use as key the
      -- path of the source file and as element the text of the source file.
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      AE.ppError sourceFilesMap err >> exitFailure
    Right _ -> return ()

-- | Command handler for the "build" command
buildCommand :: BuildCmdArgs -> IO ()
buildCommand (BuildCmdArgs chatty) = do
    when chatty (putStrLn . debugMessage $ "Reading project configuration from \"termina.yaml\"")
    -- | Read the termina.yaml file
    config <- loadConfig
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
          let msg = "\x1b[31m[error]\x1b[0m: Detected cycle between project source files: " <> T.intercalate " -> " (map T.pack files) in
          TIO.putStrLn msg >> exitFailure)
        return
        $ sortProjectDepsOrLoop projectDependencies
    when chatty (putStrLn. debugMessage $ "Type checking project modules")
    -- | Create the initial global environment
    let initialGlobalEnv = makeInitialGlobalEnv config (getPlatformInitialGlobalEnv config plt)
    (typedProject, _finalGlobalEnv) <- typeModules parsedProject initialGlobalEnv orderedDependencies
    -- | Obtain the set of option types
    when chatty (putStrLn . debugMessage $ "Searching for option types")
    let (basicTypesOptionMap, definedTypesOptionMap) = optionMapModules typedProject
    -- | Obtain the basic blocks AST of the program
    when chatty (putStrLn . debugMessage $ "Obtaining the basic blocks")
    bbProject <- genBasicBlocks typedProject
    when chatty (putStrLn . debugMessage $ "Checking basic blocks paths")
    checkProjectBasicBlocksPaths bbProject
    -- | Usage checking
    when chatty (putStrLn . debugMessage $ "Usage checking project modules")
    useDefCheckModules bbProject
    -- | Obtain the architectural description of the program
    when chatty (putStrLn . debugMessage $ "Checking the architecture of the program")
    programArchitecture <- genArchitecture bbProject (getPlatformInitialProgram config plt) orderedDependencies 
    checkEmitterConnections bbProject programArchitecture
    checkChannelConnections bbProject programArchitecture
    checkResourceUsage bbProject programArchitecture
    checkPoolUsage bbProject programArchitecture
    checkProjectBoxSources bbProject programArchitecture
    -- | Generate the code
    when chatty (putStrLn . debugMessage $ "Generating code")
    genModules config (not (M.null basicTypesOptionMap)) definedTypesOptionMap bbProject
    genInitFile config bbProject
    genPlatformCode plt config bbProject programArchitecture
    genOptionHeaderFile config basicTypesOptionMap
    when chatty (putStrLn . debugMessage $ "Build completed successfully")
