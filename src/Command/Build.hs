{-# LANGUAGE OverloadedStrings #-}

module Command.Build (
    buildCmdArgsParser, buildCommand, BuildCmdArgs
) where

import qualified Options.Applicative as O
import Control.Monad
import Data.Yaml
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Parser.Types as Parser
import qualified AST.Seman as SAST

import Generator.Platform ( checkPlatform, getPlatformInitialGlobalEnv, getPlatformInitialProgram )
import System.FilePath
import System.Exit
import System.Directory
import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import qualified Data.Map.Strict as M
import Extras.TopSort
import Semantic.Monad (SemanticAnn, Environment, makeInitialGlobalEnv)
import Modules.Modules
import DataFlow.VarUsage (runUDAnnotatedProgram)
import Generator.Option (OptionMap, runMapOptionsAnnotatedProgram)
import Data.List (foldl')
import Generator.CodeGen.Module (runGenSourceFile, runGenHeaderFile)
import Generator.LanguageC.Printer (runCPrinter)
import Generator.CodeGen.Application.Initialization (runGenInitFile)
import Generator.CodeGen.Application.Platform.RTEMS5NoelSpike (runGenMainFile)
import Generator.CodeGen.Application.Option (runGenOptionHeaderFile)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Errors.PPrinting (ppError)
import DataFlow.Architecture
import DataFlow.Architecture.Types
import DataFlow.Architecture.Checks
import AST.Core

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

-- | Data type for the "termina.yaml" configuration file
data TerminaYaml =
  TerminaYaml {
    name :: T.Text,
    platform :: T.Text,
    appFolder :: FilePath,
    appFilename :: FilePath,
    sourceModulesFolder :: FilePath,
    outputFolder :: FilePath
  } deriving (Eq, Show)

-- | Instance for parsing the "termina.yaml" configuration file
instance FromJSON TerminaYaml where
  parseJSON (Object v) =
    TerminaYaml <$>
    v .:   "name"           <*>
    v .:   "platform"       <*>
    v .:   "app-folder"     <*>
    v .:   "app-file"       <*>
    v .:   "source-modules" <*>
    v .:   "output-folder"
  parseJSON _ = fail "Expeected configuration object"

-- | Error message formatter
-- Prints error messages in the form "error: <message>"
errorMessage :: String -> String
errorMessage msg = "\x1b[31merror\x1b[0m: " ++ msg

-- | Debug message formatter
-- Prints debug messages in the form "debug: <message>"
debugMessage :: String -> String
debugMessage msg = "\x1b[32mdebug\x1b[0m: " ++ msg

warnMessage :: String -> String
warnMessage msg = "\x1b[33mwarning\x1b[0m: " ++ msg

-- | Load "termina.yaml" configuration file
loadConfig :: IO TerminaYaml
loadConfig = do
    config <- decodeFileEither "termina.yaml"
    case config of
        Left (InvalidYaml (Just (YamlException err))) -> die . errorMessage $ err
        Left err -> die . errorMessage $ show err
        Right c -> return c

newtype ParsingData = ParsingData {
  parsedAST :: Parser.AnnotatedProgram Parser.ParserAnn
} deriving (Show)

newtype SemanticData = SemanticData {
  typedAST :: SAST.AnnotatedProgram SemanticAnn
} deriving (Show)

type ParsedModule = TerminaModuleData ParsingData
type TypedModule = TerminaModuleData SemanticData
type ParsedProject = M.Map QualifiedName ParsedModule
type TypedProject = M.Map QualifiedName TypedModule
type ProjectDependencies = M.Map QualifiedName [QualifiedName]

buildModuleName :: [String] -> IO QualifiedName
buildModuleName [] = error . errorMessage $ "Internal parsing error: empty module name"
buildModuleName [x] = die . errorMessage $ "Inalid module name \"" ++ show x ++ "\": modules cannot be at the root of the source folder"
buildModuleName fs = buildModuleName' fs

  where

    buildModuleName' :: [String] -> IO QualifiedName
    buildModuleName' [] = error . errorMessage $ "Internal parsing error: empty module name"
    buildModuleName' [x] = pure x
    buildModuleName' (x:xs) = (x </>) <$> buildModuleName' xs

getModuleImports :: Parser.TerminaModule Parser.ParserAnn -> IO [FilePath]
getModuleImports = mapM (buildModuleName . moduleIdentifier) . modules

-- | Load Termina file 
loadTerminaModule ::
  -- | Path of the file to load
  FilePath
  -- | Path of the source folder that stores the imported modules
  -> FilePath
  -> IO ParsedModule
loadTerminaModule filePath root = do
  let fullP = root </> filePath <.> "fin"
  -- read it
  src_code <- TLIO.readFile fullP
  -- parse it
  case runParser terminaModuleParser () fullP (TL.unpack src_code) of
    Left err -> die . errorMessage $ "Parsing error: " ++ show err
    Right term -> do
      imports <- getModuleImports term
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
      loadedModule <- loadTerminaModule fs srcPath
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
          -- | Create the source files map. This map will be used to obtainn the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap = 
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                    M.empty parsedProject in
          ppError sourceFilesMap err >> exitFailure
        (Right (typedProgram, newState)) -> do
          let typedModule =
                TerminaModuleData
                  (qualifiedName parsedModule)
                  (fullPath parsedModule)
                  (importedModules parsedModule)
                  (sourcecode parsedModule)
                  (SemanticData typedProgram)
          typeModules' (M.insert m typedModule typedProject) newState ms

useDefCheckModules :: TypedProject -> IO ()
useDefCheckModules = mapM_ useDefCheckModule . M.elems

  where
    useDefCheckModule :: TypedModule -> IO ()
    useDefCheckModule typedModule =
      let result = runUDAnnotatedProgram . typedAST . metadata $ typedModule in
      case result of
        Nothing -> return ()
        Just err -> die . errorMessage $ show err

optionMapModules :: TypedProject -> (OptionMap, OptionMap)
optionMapModules = M.partitionWithKey
                (\k _ -> case k of
                  SAST.DefinedType {} -> False
                  _ -> True) . foldl' optionMapModule M.empty . M.elems

  where
    optionMapModule :: OptionMap -> TypedModule -> OptionMap
    optionMapModule prevMap typedModule = runMapOptionsAnnotatedProgram prevMap (typedAST . metadata $ typedModule)

printModules ::
  -- | Whether to include the option.h file or not
  Bool
  -- | The map with the option types to generate from defined types 
  -> OptionMap
  -- | The output folder 
  -> FilePath
  -- | The project to generate the code from
  -> TypedProject -> IO ()
printModules includeOptionH definedTypesOptionMap destinationPath =
  mapM_ printModule . M.elems

  where

    printModule :: TypedModule -> IO ()
    printModule typedModule = do
      let sourceFile = destinationPath </> "src" </> qualifiedName typedModule <.> "c"
          tAST = typedAST . metadata $ typedModule
      case runGenSourceFile (qualifiedName typedModule) tAST of
        Left err -> die. errorMessage $ show err
        Right cSourceFile -> TIO.writeFile sourceFile $ runCPrinter cSourceFile
      case runGenHeaderFile includeOptionH (qualifiedName typedModule) (importedModules typedModule) tAST definedTypesOptionMap of
        Left err -> die . errorMessage $ show err
        Right cHeaderFile -> TIO.writeFile (destinationPath </> "include" </> qualifiedName typedModule <.> "h") $ runCPrinter cHeaderFile

printInitFile :: FilePath -> TypedProject -> IO ()
printInitFile destinationPath typedProject = do
  let initFilePath = destinationPath </> "init" <.> "c"
      projectModules = M.toList $ typedAST . metadata <$> typedProject
  case runGenInitFile initFilePath projectModules of
    Left err -> die . errorMessage $ show err
    Right cInitFile -> TIO.writeFile initFilePath $ runCPrinter cInitFile

printMainFile :: FilePath -> TypedProject -> IO ()
printMainFile destinationPath typedProject = do
  let mainFilePath = destinationPath </> "main" <.> "c"
      projectModules = M.toList $ typedAST . metadata <$> typedProject
  case runGenMainFile mainFilePath projectModules of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> TIO.writeFile mainFilePath $ runCPrinter cMainFile

printOptionHeaderFile :: FilePath -> OptionMap -> IO ()
printOptionHeaderFile destinationPath basicTypesOptionMap = do
  let optionsFilePath = destinationPath </> "include" </> "options" <.> "h"
  case runGenOptionHeaderFile basicTypesOptionMap of
    Left err -> die . errorMessage $ show err
    Right cOptionsFile -> TIO.writeFile optionsFilePath $ runCPrinter cOptionsFile

genArchitecture :: TypedProject -> TerminaProgArch SemanticAnn -> [QualifiedName] -> IO (TerminaProgArch SemanticAnn)
genArchitecture typedProject initialTerminaProgram orderedDependencies = do
  genArchitecture' initialTerminaProgram orderedDependencies

  where

    genArchitecture' :: TerminaProgArch SemanticAnn -> [QualifiedName] -> IO (TerminaProgArch SemanticAnn)
    genArchitecture' tp [] = pure tp
    genArchitecture' tp (m:ms) = do
      let typedModule = typedAST . metadata $ typedProject M.! m
      let result = runGenArchitecture tp typedModule
      case result of
        Left err -> die . errorMessage $ show err
        Right tp' -> genArchitecture' tp' ms

warnDisconnectedEmitters :: TerminaProgArch SemanticAnn -> IO ()
warnDisconnectedEmitters tp =
    let disconnectedEmitters = getDisconnectedEmitters tp in
    unless (null disconnectedEmitters) $
      putStrLn . warnMessage $ "The following emitters are not connected to any task or handler: " ++ show disconnectedEmitters

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
    appModule <- loadTerminaModule (appFilename config) (appFolder config)
    -- | Load the project
    when chatty (putStrLn . debugMessage $ "Loading project modules")
    parsedModules <- loadModules (importedModules appModule) (sourceModulesFolder config)
    let parsedProject = M.insert (qualifiedName appModule) appModule parsedModules
    -- | Detect any possible loops in the project
    when chatty (putStrLn . debugMessage $ "Ordering project modules")
    let projectDependencies = M.map importedModules parsedProject
    orderedDependencies <-
      either
        (die . errorMessage . ("Detecte cycle between project source files: " ++) . show)
        return
        $ sortProjectDepsOrLoop projectDependencies
    when chatty (putStrLn. debugMessage $ "Type checking project modules")
    -- | Create the initial global environment
    let initialGlobalEnv = makeInitialGlobalEnv (getPlatformInitialGlobalEnv plt)
    (typedProject, _finalGlobalEnv) <- typeModules parsedProject initialGlobalEnv orderedDependencies
    -- | Usage checking
    when chatty (putStrLn . debugMessage $ "Usage checking project modules")
    useDefCheckModules typedProject
    when chatty (putStrLn . debugMessage $ "Searching for option types")
    let (basicTypesOptionMap, definedTypesOptionMap) = optionMapModules typedProject
    when chatty (putStrLn . debugMessage $ "Checking the architecture of the program")
    programArchitecture <- genArchitecture typedProject (getPlatformInitialProgram plt) orderedDependencies 
    warnDisconnectedEmitters programArchitecture
    -- | Generate the code
    when chatty (putStrLn . debugMessage $ "Generating code")
    printModules (not (M.null basicTypesOptionMap)) definedTypesOptionMap (outputFolder config) typedProject
    printInitFile (outputFolder config) typedProject
    printMainFile (outputFolder config) typedProject
    printOptionHeaderFile (outputFolder config) basicTypesOptionMap
    when chatty (putStrLn . debugMessage $ "Build completed successfully")
