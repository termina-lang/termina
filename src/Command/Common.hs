module Command.Common where

import Command.Utils
import Command.Sources
import Command.Types
import Configuration.Namespace (locationLine)
import Data.List (find)
import Modules.Utils (buildModuleName, qualifiedToModuleName)

import qualified Data.Text as T

import System.FilePath
import System.Directory
import Parser.AST (TerminaModule)
import Parser.Parsing (terminaModuleParser)
import Parser.Types (ParserAnn)
import Parser.Errors
import Text.Parsec (runParser)
import qualified Data.Map.Strict as M
import Semantic.Types (SemanticAnn)
import Modules.Modules
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import ControlFlow.Architecture
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Checks
import ControlFlow.Architecture.Checks.Instances (runCheckInstanceModules)
import Core.AST
    ( TerminaModule'(frags, modules), ModuleImport'(..) )
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
  Sources
  -- | Source folder of the module
  -> FilePath
  -- | Path of the file to load
  -> FilePath
  -> IO ParsedModule
loadTerminaModule sources root filePath = do
  let fullP = root </> filePath <.> "fin"
  (src_code, term) <- parseTerminaFile filePath fullP
  mod_time <- getModificationTime fullP
  imports <- mapM (importOf (M.singleton fullP src_code) fullP) (modules term)
  return $ TerminaModuleData filePath fullP mod_time imports [] src_code (ParsingData . frags $ term)

  where

    importerOwner = maybe Project folderOwner (find ((== root) . folderPath) sources)

    -- | An import, looked up in the folders that the folder of the module sees.
    importOf fileMap fullP (ModuleImport modName ann) =
      case buildModuleName ann modName of
        Left err -> dieWithReport (toText err fileMap)
        Right qname -> do
          found <- lookupModule sources root qname
          case found of
            Found _ -> return (ModuleDependency qname ann)
            Ambiguous a b -> dieWithError (collisionMessage sources qname a b)
            Hidden folder -> dieWithError $ fullP ++ ":" ++ show (locationLine ann) ++ ": "
              ++ hiddenMessage folder importerOwner (qualifiedToModuleName qname)
            NotExported folder -> dieWithError $ fullP ++ ":" ++ show (locationLine ann) ++ ": "
              ++ notExportedMessage folder (qualifiedToModuleName qname)
            Missing -> dieWithReport (toText (annotateError ann (EImportedFileNotFound (root </> qname <.> "fin"))) fileMap)

-- | Read and parse a Termina file, without its imports.
parseTerminaFile ::
  -- | Qualified name of the module
  FilePath
  -- | Path of the file
  -> FilePath
  -> IO (T.Text, TerminaModule ParserAnn)
parseTerminaFile filePath fullP = do
  src_code <- either (const (dieWithError $ fullP ++ ": the file is not valid UTF-8")) return . TE.decodeUtf8' =<< BS.readFile fullP
  case runParser terminaModuleParser filePath fullP (T.unpack src_code) of
    Left err ->
      let pErr = annotateError (Position filePath (errorPos err) (errorPos err)) (EParseError err)
          fileMap = M.singleton fullP src_code
      in
      dieWithReport (toText pErr fileMap)
    Right term -> return (src_code, term)

-- | Load the modules that the modules of a folder import, and everything they
-- import in turn, each one from the folder where the importer finds it. One
-- name found in two folders of the build is an error.
loadModules
  :: Sources
  -> FilePath
  -> [ModuleDependency]
  -> IO ParsedProject
loadModules sources importer imported = loadModules' M.empty [(dep, importer) | dep <- imported]

  where

  loadModules' :: ParsedProject -> [(ModuleDependency, FilePath)] -> IO ParsedProject
  loadModules' fsLoaded [] = pure fsLoaded
  loadModules' fsLoaded ((ModuleDependency qname _, from) : fss) = do
    found <- lookupModule sources from qname
    folder <- case found of
      Found folder -> return folder
      Ambiguous a b -> dieWithError (collisionMessage sources qname a b)
      _ -> dieWithError $ "Module " ++ qname ++ " not found"
    case M.lookup qname fsLoaded of
      Just loaded
        | fullPath loaded == folder </> qname <.> "fin" -> loadModules' fsLoaded fss
        | otherwise -> dieWithError (collisionMessage sources qname (moduleFolder qname (fullPath loaded)) folder)
      Nothing -> do
        loadedModule <- loadTerminaModule sources folder qname
        loadModules' (M.insert qname loadedModule fsLoaded)
          (fss ++ [(dep, folder) | dep <- importedModules loadedModule])

  -- | The source folder of a module, from its file and its qualified name.
  moduleFolder qname path =
    let parts = splitDirectories path
    in joinPath (take (length parts - length (splitDirectories qname)) parts)

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
          dieWithReport (toText err sourceFilesMap)
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
    genArchitecture' tp [] = pure (pruneUnconnectedSystemElements tp)
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
          dieWithReport (toText err sourceFilesMap)
        Right tp' -> genArchitecture' tp' ms

checkEmitterConnections :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkEmitterConnections loweredProject progArchitecture =
  let result = runCheckEmitterConnections progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkChannelConnections :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkChannelConnections loweredProject progArchitecture =
  let result = runCheckChannelConnections progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkResourceUsage :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkResourceUsage loweredProject progArchitecture =
  let result = runCheckResourceUsage progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkTaskPriorities :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkTaskPriorities loweredProject progArchitecture =
  let result = runCheckTaskPriorities progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkSystemExceptAction :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkSystemExceptAction loweredProject progArchitecture =
  let result = runCheckSystemExceptAction progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkActivationCycles :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkActivationCycles loweredProject progArchitecture =
  let result = runCheckActivationCycles progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

-- | The main module is the last one in the order of dependencies
checkInstanceModules :: LoweredProject -> [QualifiedName] -> TerminaProgArch SemanticAnn -> IO ()
checkInstanceModules loweredProject orderedDependencies progArchitecture =
  case orderedDependencies of
    [] -> return ()
    _ ->
      case runCheckInstanceModules (last orderedDependencies) progArchitecture of
        Left err ->
          let sourceFilesMap =
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                    M.empty loweredProject in
          dieWithReport (toText err sourceFilesMap)
        Right _ -> return ()

checkTestDeployment :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkTestDeployment loweredProject progArchitecture =
  let result = runCheckTestDeployment progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkPoolUsage :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkPoolUsage loweredProject progArchitecture =
  let result = runCheckPoolUsage progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

checkDequeConnections :: LoweredProject -> TerminaProgArch SemanticAnn -> IO ()
checkDequeConnections loweredProject progArchitecture =
  let result = runCheckDequeConnections progArchitecture in
  case result of
    Left err ->
      let sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                M.empty loweredProject in
      dieWithReport (toText err sourceFilesMap)
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
      dieWithReport (toText err sourceFilesMap)
    Right _ -> return ()

constFolding :: TerminaConfig -> Platform -> LoweredProject -> IO (LoweredProject, ProjectConstEnvs)
constFolding config plt loweredProject =
  -- | Fold the modules in dependency order, threading the constant environment
  -- from one module to the next so that a module can resolve the constants
  -- defined by the modules it imports.
  case sortProjectDepsOrLoop (M.map importedModules loweredProject) of
    -- | The build pipeline orders the modules (and reports dependency cycles)
    -- before reaching this point, so a cycle here would be an internal error.
    Left _ -> dieWithError $ "Dependency cycle detected during constant folding"
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
          dieWithReport (toText err sourceFilesMap)
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
    dieWithReport (toText err (projectSourceFiles project))

-- | Returns, for each module, the run-time checks that the values of their
-- operands show to hold, which the elaboration hands to the value prover.
valueAnalysisCheck :: Platform -> ProjectConstEnvs -> LoweredProject -> IO ProjectValueEvidence
valueAnalysisCheck plt constEnvs loweredProject =
  case sortProjectDepsOrLoop (M.map importedModules loweredProject) of
    -- | The build pipeline orders the modules (and reports dependency cycles)
    -- before reaching this point, so a cycle here would be an internal error.
    Left _ -> dieWithError $ "Dependency cycle detected during the value analysis"
    Right orderedDependencies -> checkModules M.empty M.empty orderedDependencies

  where

    checkModules _ evidence [] = return evidence
    checkModules returned evidence (m:ms) =
      case runValueAnalysisCheck plt
             (M.findWithDefault M.empty m constEnvs)
             returned
             (loweredAST . metadata $ loweredProject M.! m) of
        (Just err, _, _) ->
          dieWithReport (toText err (projectSourceFiles loweredProject))
        (Nothing, returned', proven) -> checkModules returned' (M.insert m proven evidence) ms
