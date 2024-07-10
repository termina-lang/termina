{-# LANGUAGE OverloadedStrings #-}

module Command.Build (
    buildCmdArgsParser, buildCommand, BuildCmdArgs
) where

import qualified Options.Applicative as O
import Control.Monad
import Data.Yaml
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as IOL
import qualified AST.Parser as PAST
import qualified AST.Seman as SAST

import Generator.Platform ( checkPlatform )
import System.FilePath
import System.Exit
import System.Directory
import Parser.Parsing (Annotation, terminaModuleParser)
import Text.Parsec (runParser)
import qualified Data.Map.Strict as M
import Extras.TopSort
import Semantic.Monad (SemanticAnns, SAnns, ExpressionState, runTypeChecking, SemanticMonad, initialExpressionSt)
import Semantic.Types (GEntry)
import Modules.Modules (ModuleName)
import Semantic.TypeChecking (programSeman, programAdd)
import AST.Core (Identifier)
import Semantic.Errors (ppError)
import DataFlow.DF (runUDAnnotatedProgram)
import Generator.Option (OptionMap, runMapOptionsAnnotatedProgram)
import Data.List (foldl')

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

-- | Load "termina.yaml" configuration file
loadConfig :: IO TerminaYaml
loadConfig = do
    config <- decodeFileEither "termina.yaml"
    case config of
        Left (InvalidYaml (Just (YamlException err))) -> die . errorMessage $ err
        Left err -> die . errorMessage $ show err
        Right c -> return c

type QualifiedName = FilePath

-- | Data type used to represented a loaded module
-- | It contains the module's name, the list of imported modules, the source code
-- | and the module's metadata.
data TerminaModuleData a = TerminaModuleData {
  -- | Module's qualified name
  qualifiedName :: !QualifiedName,
  -- | Root of the module
  -- This is the root path where the module is
  rootPath :: !FilePath,
  -- | List of imported modules
  importedModules :: ![QualifiedName],
  -- | Source code
  sourcecode :: TL.Text,
  -- | Module meta-data (e.g. parsed AST)
  metadata :: a
} deriving (Show)

newtype ParsingData = ParsingData {
  parsedAST :: PAST.AnnotatedProgram Annotation
} deriving (Show)

data SemanticData = SemanticData {
  typedAST :: SAST.AnnotatedProgram SemanticAnns,
  moduleDefs :: M.Map SAST.Identifier (SAnns (GEntry SemanticAnns))
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

getModuleImports :: PAST.TerminaModule Annotation -> IO [FilePath]
getModuleImports = mapM (buildModuleName . PAST.moduleIdentifier) . PAST.modules

-- | Load Termina file 
loadTerminaModule :: 
  -- | Path of the file to load
  FilePath 
  -- | Path of the source folder that stores the imported modules
  -> FilePath 
  -> IO ParsedModule
loadTerminaModule filePath root = do
  -- read it
  src_code <- IOL.readFile $ root </> filePath <.> "fin"
  -- parse it
  case runParser terminaModuleParser () filePath (TL.unpack src_code) of
    Left err -> ioError $ userError $ "Parser Error ::\n" ++ show err
    Right term -> do
      imports <- getModuleImports term
      return $ TerminaModuleData filePath root imports src_code (ParsingData . PAST.frags $ term)

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

typeTerminaModule :: 
  PAST.AnnotatedProgram Annotation 
  -> SemanticMonad (SAST.AnnotatedProgram SemanticAnns, M.Map Identifier (SAnns (GEntry SemanticAnns)))
typeTerminaModule = foldM (\(elems, glbs) t -> do
  element <- programSeman t
  (ident, ann) <- programAdd element
  return (element : elems, M.insert ident ann glbs)) ([], M.empty)

typeModules :: ParsedProject -> [ModuleName] -> IO TypedProject
typeModules parsedProject =
  typeModules' M.empty initialExpressionSt

  where
  
    typeModules' :: TypedProject -> ExpressionState -> [ModuleName] -> IO TypedProject
    typeModules' typedProject _ [] = pure typedProject
    typeModules' typedProject prevState (m:ms) = do
      let parsedModule = parsedProject M.! m
      let result = runTypeChecking prevState (typeTerminaModule . parsedAST . metadata $ parsedModule)
      case result of
        (Left err, _) -> 
          let sourceFilesMap = sourcecode <$> parsedProject in
          ppError sourceFilesMap err >> exitFailure
        (Right (typedProgram, globals), newState) -> do
          let typedModule = 
                TerminaModuleData 
                  (qualifiedName parsedModule) 
                  (rootPath parsedModule) 
                  (importedModules parsedModule) 
                  (sourcecode parsedModule) 
                  (SemanticData typedProgram globals)
          typeModules' (M.insert m typedModule typedProject) newState ms

useDefCheckModules :: TypedProject -> IO ()
useDefCheckModules = mapM_ useDefCheckModule . M.elems

  where
    useDefCheckModule :: TypedModule -> IO ()
    useDefCheckModule typedModule =
      let result = runUDAnnotatedProgram . typedAST . metadata $ typedModule in
      case result of
        Nothing -> return ()
        Just err -> die . errorMessage $ "Unsupported platform: \"" ++ show err

optionMapModules :: TypedProject -> OptionMap
optionMapModules = foldl' optionMapModule M.empty . M.elems

  where
    optionMapModule :: OptionMap -> TypedModule -> OptionMap
    optionMapModule prevMap typedModule = runMapOptionsAnnotatedProgram prevMap (typedAST . metadata $ typedModule)

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
    when chatty (putStrLn . debugMessage $ "Creating output source folder: \"" ++ outputSrcFolder ++ "\"")
    createDirectoryIfMissing True outputSrcFolder
    when chatty (putStrLn . debugMessage $ "Creating output include folder: \"" ++ outputIncludeFolder ++ "\"")
    createDirectoryIfMissing True outputIncludeFolder
    -- | Load the main application module
    when chatty (putStrLn . debugMessage $ "Loading application main module: \"" ++ appFolder config </> appFilename config ++ "\"")
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
    typedProject <- typeModules parsedProject orderedDependencies
    -- | Usage checking
    when chatty (putStrLn . debugMessage $ "Usage checking project modules")
    useDefCheckModules typedProject
    when chatty (putStrLn . debugMessage $ "Searching for option types")
    let optionMap = optionMapModules typedProject
    -- | Generate the code
    when chatty (putStrLn . debugMessage $ "Generating code")
