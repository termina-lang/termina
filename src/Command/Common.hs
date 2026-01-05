module Command.Common where

import Command.Utils
import Command.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.FilePath
import System.Exit
import System.Directory
import Parser.Parsing (terminaModuleParser)
import Parser.Errors
import Text.Parsec (runParser)
import qualified Data.Map.Strict as M
import Semantic.Types (SemanticAnn)
import Modules.Modules
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import ControlFlow.Architecture
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Checks
import Core.AST
    ( TerminaModule'(frags) )
import Utils.Errors
import Utils.Annotations
import Text.Parsec.Error
import Semantic.Environment
import ControlFlow.ConstFolding (runTransFolding, runConstSimpl)
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE


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
  src_code <- TE.decodeUtf8 <$> BS.readFile fullP
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
          return $ TerminaModuleData filePath fullP mod_time imports [] src_code (ParsingData . frags $ term)

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
      let prevModsMap = M.map visibleModules typedProject
      let vmods = S.fromList $ getVisibleModules prevModsMap (importedModules parsedModule)
      let result = runTypeChecking prevState (typeTerminaModule (S.insert m vmods) . parsedAST . metadata $ parsedModule)
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
                  (S.toList vmods)
                  (sourcecode parsedModule)
                  (SemanticData typedProgram)
          typeModules' (M.insert m typedModule typedProject) newState ms

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

constSimpl :: BasicBlocksProject -> IO BasicBlocksProject
constSimpl bbProject = do
  case runConstSimpl bbProject of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right bbProject' -> return bbProject'

constFolding :: BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO BasicBlocksProject
constFolding bbProject progArchitecture =
  case runTransFolding bbProject progArchitecture of
    Left err ->
      -- | Create the source files map. This map will be used to obtainn the source files that
      -- will be feed to the error pretty printer. The source files map must use as key the
      -- path of the source file and as element the text of the source file.
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty bbProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right bbProject' -> return bbProject'
