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
import Configuration.Configuration (TerminaConfig(..))
import Configuration.Platform (Platform)
import Utils.Errors
import Utils.Annotations
import Text.Parsec.Error
import Semantic.Environment
import ControlFlow.ConstFolding (runConstFolding, constFoldModule)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ValueAnalysis (runValueAnalysisCheck)
import qualified Data.Set as S
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE


-- | Load Termina file 
loadTerminaModule ::
  FilePath
  -- | Path of the file to load
  -> FilePath
  -- | Path of the source folder that stores the imported modules
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
  -- | Load and parse the project.
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
typeModules parsedProject initialState =
  typeModules' M.empty (addDeclaredNames (projectDeclaredNames parsedProject) initialState)

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
          -- | Create the source files map. This map will be used to obtain the source files that
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

genArchitecture :: LoweredProject -> TerminaProgArch SemanticAnn -> [QualifiedName] -> IO (TerminaProgArch SemanticAnn)
genArchitecture loweredProject initialTerminaProgram orderedDependencies = do
  genArchitecture' initialTerminaProgram orderedDependencies

  where

    genArchitecture' :: TerminaProgArch SemanticAnn -> [QualifiedName] -> IO (TerminaProgArch SemanticAnn)
    genArchitecture' tp [] = pure tp
    genArchitecture' tp (m:ms) = do
      let typedModule = loweredAST . metadata $ loweredProject M.! m
      let result = runGenArchitecture tp m typedModule
      case result of
        Left err ->
          -- | Create the source files map. This map will be used to obtainn the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap =
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                    M.empty loweredProject in
          TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
        Right tp' -> genArchitecture' tp' ms

checkEmitterConnections :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkEmitterConnections loweredProject progArchitecture =
  let result = runCheckEmitterConnections progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkChannelConnections :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkChannelConnections loweredProject progArchitecture =
  let result = runCheckChannelConnections progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkResourceUsage :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkResourceUsage loweredProject progArchitecture =
  let result = runCheckResourceUsage progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkTaskPriorities :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkTaskPriorities loweredProject progArchitecture =
  let result = runCheckTaskPriorities progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkPoolUsage :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkPoolUsage loweredProject progArchitecture =
  let result = runCheckPoolUsage progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

checkProjectBoxSources :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkProjectBoxSources loweredProject progArchitecture =
  let result = runCheckBoxSources progArchitecture in
  case result of
    Left err ->
      -- | Create the source files map. This map will be used to obtainn the source files that
      -- will be feed to the error pretty printer. The source files map must use as key the
      -- path of the source file and as element the text of the source file.
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
    Right _ -> return ()

constFolding :: TerminaConfig -> Platform -> LoweredProject -> IO (LoweredProject, ProjectConstEnvs)
constFolding config plt loweredProject =
  -- | Fold the modules in dependency order, threading the constant environment
  -- from one module to the next so that a module can resolve the constants
  -- defined by the modules it imports.
  case sortProjectDepsOrLoop (M.map importedModules loweredProject) of
    -- | The build pipeline orders the modules (and reports dependency cycles)
    -- before reaching this point, so a cycle here would be an internal error.
    Left _ -> die . errorMessage $ "Dependency cycle detected during constant folding"
    Right orderedDependencies ->
      foldModules (ConstFoldEnv M.empty plt (microsecondsPerTick config)) M.empty M.empty orderedDependencies

  where

    foldModules :: ConstFoldEnv -> LoweredProject -> ProjectConstEnvs -> [QualifiedName]
      -> IO (LoweredProject, ProjectConstEnvs)
    foldModules _ foldedProject constEnvs [] = return (foldedProject, constEnvs)
    foldModules env foldedProject constEnvs (m:ms) =
      case runConstFolding env (constFoldModule (loweredProject M.! m)) of
        Left err ->
          let sourceFilesMap =
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                    M.empty loweredProject in
          TIO.putStrLn (toText err sourceFilesMap) >> exitFailure
        Right (foldedModule, env') ->
          foldModules env' (M.insert m foldedModule foldedProject)
            (M.insert m (constEnv env') constEnvs) ms

-- | Checks that no condition of the project has the same value every time it
-- is evaluated (VAE-001). The check runs after the folding and not with the
-- rest of the basic-block checks because it needs the constants of each
-- module, which the folding is what builds.
-- The modules are checked in dependency order, threading the returned of what
-- each function gives back from one module to the next, so that a call resolves
-- against the function it calls even when that one lives in a module this one
-- imports.
-- | Checks the side effects of the elaborated project, which is where each
-- operation says whether it is checked while the program runs.
sideEffectCheck :: Platform -> ElaboratedProject -> IO ()
sideEffectCheck plt project =
  forM_ (sideEffectCheckModules plt project) $ \err ->
    TIO.putStrLn (toText err (projectSourceFiles project)) >> exitFailure

-- | Returns, for each module, the run-time checks that the values of their
-- operands show to hold, which the elaboration hands to the value prover.
valueAnalysisCheck :: Platform -> ProjectConstEnvs -> LoweredProject -> IO ProjectValueEvidence
valueAnalysisCheck plt constEnvs loweredProject =
  case sortProjectDepsOrLoop (M.map importedModules loweredProject) of
    -- | The build pipeline orders the modules (and reports dependency cycles)
    -- before reaching this point, so a cycle here would be an internal error.
    Left _ -> die . errorMessage $ "Dependency cycle detected during the value analysis"
    Right orderedDependencies -> checkModules M.empty M.empty orderedDependencies

  where

    checkModules _ evidence [] = return evidence
    checkModules returned evidence (m:ms) =
      case runValueAnalysisCheck plt
             (M.findWithDefault M.empty m constEnvs)
             returned
             (loweredAST . metadata $ loweredProject M.! m) of
        (Just err, _, _) ->
          TIO.putStrLn (toText err (projectSourceFiles loweredProject)) >> exitFailure
        (Nothing, returned', proven) -> checkModules returned' (M.insert m proven evidence) ms
