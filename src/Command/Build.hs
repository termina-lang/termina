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
import Parser.Parsing (terminaModuleParser)
import Parser.Errors
import Text.Parsec (runParser)
import qualified Data.Map as M
import Semantic.Types (SemanticAnn)
import Modules.Modules
import Generator.Monadic
import Data.List (foldl')
import Generator.CodeGen.Module (runGenSourceFile, runGenHeaderFile)
import Generator.LanguageC.Printer (runCPrinter)
import Generator.CodeGen.Application.Initialization (runGenInitFile)
import Generator.CodeGen.Application.Option (runGenOptionHeaderFile)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import ControlFlow.Architecture
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Checks
import Core.AST
import Generator.Environment
import Generator.Platform
import Configuration.Platform
import Utils.Errors
import Utils.Annotations
import Text.Parsec.Error
import Semantic.Environment
import ControlFlow.ConstFolding (runConstFolding)
import qualified Data.Set as S
import Generator.CodeGen.Application.Status (runGenStatusHeaderFile)
import Generator.CodeGen.Application.Result (runGenResultHeaderFile)

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
  src_code <- TIO.readFile fullP
  mod_time <- getModificationTime fullP
  -- parse it
  case runParser terminaModuleParser filePath fullP (T.unpack src_code) of
    Left err -> 
      let pErr = annotateError (Position filePath (errorPos err) (errorPos err)) (EParseError err)
          fileMap = M.singleton fullP src_code
      in
      TIO.putStrLn (toText pErr fileMap) >> exitFailure
    Right term -> do
      mimports <- getModuleImports (Just srcPath) term
      case mimports of
        Left err -> 
          let fileMap = M.singleton fullP src_code in
          TIO.putStrLn (toText err fileMap) >> exitFailure
        Right imports ->
          return $ TerminaModuleData filePath fullP mod_time imports src_code (ParsingData . frags $ term)

-- | Load the modules of the project
loadModules
  :: [ModuleDependency]
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
    -> [ModuleDependency]
    -> IO ParsedProject
  loadModules' fsLoaded [] = pure fsLoaded
  loadModules' fsLoaded ((ModuleDependency qname _):fss) =
    if M.member qname fsLoaded
    -- Nothing to do, skip to the next one. It could be the case of a module
    -- imported from several files.
    then loadModules' fsLoaded fss
    -- Import and load it.
    else do
      loadedModule <- loadTerminaModule srcPath qname srcPath
      let deps = importedModules loadedModule
      loadModules'
        (M.insert qname loadedModule fsLoaded)
        (fss ++ deps)

typeModules :: ParsedProject -> Environment -> [QualifiedName] -> IO (TypedProject, Environment)
typeModules parsedProject =
  typeModules' M.empty

  where


    typeModules' :: TypedProject -> Environment -> [QualifiedName] -> IO (TypedProject, Environment)
    typeModules' typedProject finalState [] = pure (typedProject, finalState)
    typeModules' typedProject prevState (m:ms) = do
      let parsedModule = parsedProject M.! m
      let moduleDependencies = S.fromList $ getModuleDependencyList (importedModules <$> typedProject) (importedModules parsedModule)
      let result = runTypeChecking prevState (typeTerminaModule (S.insert  m moduleDependencies) . parsedAST . metadata $ parsedModule)
      case result of
        (Left err) ->
          -- | Create the source files map. This map will be used to obtain the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap = 
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                    M.empty parsedProject in
          TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
        (Right (typedProgram, newState)) -> do
          let typedModule =
                TerminaModuleData
                  (qualifiedName parsedModule)
                  (fullPath parsedModule)
                  (modificationTime parsedModule)
                  (importedModules parsedModule)
                  (sourcecode parsedModule)
                  (SemanticData typedProgram)
          typeModules' (M.insert m typedModule typedProject) newState ms

monadicTypesMapModules :: TypedProject -> MonadicTypes
monadicTypesMapModules = foldl' monadicTypesMapModule emptyMonadicTypes  . M.elems

  where
    monadicTypesMapModule :: MonadicTypes -> TypedModule -> MonadicTypes
    monadicTypesMapModule prevMap typedModule = runMapMonadicTypesAnnotatedProgram prevMap (typedAST . metadata $ typedModule)

genModules ::
  TerminaConfig
  -> Platform
  -- | The map with the option types to generate from defined types 
  -> MonadicTypes
  -- | The project to generate the code from
  -> BasicBlocksProject -> IO ()
genModules params plt initialMonadicTypes =
  foldM_ printModule initialMonadicTypes . M.elems

  where

    printModule :: MonadicTypes -> BasicBlocksModule -> IO MonadicTypes
    printModule currentMonadicTypes bbModule = do
      let destinationPath = outputFolder params
          sourceFile = destinationPath </> "src" </> qualifiedName bbModule <.> "c"
          headerFile = destinationPath </> "include" </> qualifiedName bbModule <.> "h"
      sourceFileExists <- doesFileExist sourceFile
      if sourceFileExists
        then do
          sourceFileTime <- getModificationTime sourceFile
          if sourceFileTime > modificationTime bbModule
            then return () 
          else do
            printSource bbModule
      else do
        printSource bbModule
      headerFileExists <- doesFileExist headerFile
      if headerFileExists 
        then do
          headerFileTime <- getModificationTime headerFile
          if headerFileTime > modificationTime bbModule
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

genInitFile :: TerminaConfig -> Platform -> BasicBlocksProject -> IO ()
genInitFile params plt bbProject = do
  let destinationPath = outputFolder params
      initFilePath = destinationPath </> "init" <.> "c"
      projectModules = M.toList $ basicBlocksAST . metadata <$> bbProject
  case runGenInitFile params (getPlatformInterruptMap plt) initFilePath projectModules of
    Left err -> die . errorMessage $ show err
    Right cInitFile -> TIO.writeFile initFilePath $ runCPrinter (profile params == Debug) cInitFile

genOptionHeaderFile :: TerminaConfig -> Platform -> MonadicTypes -> IO ()
genOptionHeaderFile params plt monadicTypes = do
  let destinationPath = outputFolder params
      optionFilePath = destinationPath </> "include" </> "option" <.> "h"
  case runGenOptionHeaderFile params (getPlatformInterruptMap plt) optionFilePath monadicTypes of
    Left err -> die . errorMessage $ show err
    Right cOptionsFile -> TIO.writeFile optionFilePath $ runCPrinter (profile params == Debug) cOptionsFile

genStatusHeaderFile :: TerminaConfig -> Platform -> MonadicTypes -> IO ()
genStatusHeaderFile params plt monadicTypes = do
  let destinationPath = outputFolder params
      statusFilePath = destinationPath </> "include" </> "status" <.> "h"
  case runGenStatusHeaderFile params (getPlatformInterruptMap plt) statusFilePath monadicTypes of
    Left err -> die . errorMessage $ show err
    Right cOptionsFile -> TIO.writeFile statusFilePath $ runCPrinter (profile params == Debug) cOptionsFile

genResultHeaderFile :: TerminaConfig -> Platform -> MonadicTypes -> IO ()
genResultHeaderFile params plt monadicTypes = do
  let destinationPath = outputFolder params
      resultFilePath = destinationPath </> "include" </> "result" <.> "h"
  case runGenResultHeaderFile params (getPlatformInterruptMap plt) resultFilePath monadicTypes of
    Left err -> die . errorMessage $ show err
    Right cOptionsFile -> TIO.writeFile resultFilePath $ runCPrinter (profile params == Debug) cOptionsFile

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
          TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
        Right tp' -> genArchitecture' tp' ms

checkEmitterConnections :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkEmitterConnections bbProject progArchitecture = 
  let result = runCheckEmitterConnections progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkChannelConnections :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkChannelConnections bbProject progArchitecture = 
  let result = runCheckChannelConnections progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkResourceUsage :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkResourceUsage bbProject progArchitecture = 
  let result = runCheckResourceUsage progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkPoolUsage :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
checkPoolUsage bbProject progArchitecture = 
  let result = runCheckPoolUsage progArchitecture in
  case result of
    Left err -> 
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

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
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

constFolding :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO BasicBlocksProject
constFolding bbProject progArchitecture = 
  case runConstFolding bbProject progArchitecture of
    Left err -> 
      -- | Create the source files map. This map will be used to obtainn the source files that
      -- will be feed to the error pretty printer. The source files map must use as key the
      -- path of the source file and as element the text of the source file.
      let sourceFilesMap = 
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right bbProject' -> return bbProject'

-- | Command handler for the "build" command
buildCommand :: BuildCmdArgs -> IO ()
buildCommand (BuildCmdArgs chatty) = do
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
    -- | Obtain the architectural description of the program
    when chatty (putStrLn . debugMessage $ "Checking the architecture of the program")
    programArchitecture <- genArchitecture rawBBProject (getPlatformInitialProgram config plt) orderedDependencies 
    checkEmitterConnections rawBBProject programArchitecture
    checkChannelConnections rawBBProject programArchitecture
    checkResourceUsage rawBBProject programArchitecture
    checkPoolUsage rawBBProject programArchitecture
    checkProjectBoxSources rawBBProject programArchitecture
    when chatty (putStrLn . debugMessage $ "Performing constant folding")
    bbProject <- constFolding rawBBProject programArchitecture
    -- | Generate the code
    when chatty (putStrLn . debugMessage $ "Generating code")
    genModules config plt monadicTypes bbProject
    genInitFile config plt bbProject
    genPlatformCode config plt bbProject programArchitecture
    unless (S.null (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . optionTypes $ monadicTypes)) $ genOptionHeaderFile config plt monadicTypes
    unless (S.null (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . statusTypes $ monadicTypes)) $ genStatusHeaderFile config plt monadicTypes
    unless (S.null (S.unions . M.elems . M.filterWithKey (\k _ -> case k of {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . resultTypes $ monadicTypes)) $ genResultHeaderFile config plt monadicTypes
    when chatty (putStrLn . debugMessage $ "Build completed successfully")
