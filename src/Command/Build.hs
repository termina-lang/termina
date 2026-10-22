{-# LANGUAGE OverloadedStrings #-}

module Command.Build (
    buildCmdArgsParser, buildCommand, generateCommand, BuildCmdArgs
) where

import Configuration.Configuration
import Configuration.Manifest
import Command.Utils
import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe, isJust)
import System.Process (rawSystem)
import Command.Types

import qualified Options.Applicative as O
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.FilePath
import System.Exit
import System.Directory

import qualified Data.Map.Strict as M
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
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import EFP.Schedulability.WCEPath.Generator (genTransactionalWCEPS)
import EFP.Schedulability.WCEPath.Printer
import Command.Common
import Command.Extern (prepareExtern)
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.PlantUML
import Extras.PlantUML.Printer
import Text.Read
import Data.Char
import qualified Data.ByteString.Lazy as BL
import Elaboration (ElaborationReport)
import Elaboration.Report (checksReport)


data CmpDiagramParam =
  CmpDiagramParamRange CmpDiagramRange
  | CmpDiagramParamFlag
   deriving Show


-- | The deployments a command works on
data Selection =
    SelectApp
    | SelectTest String
    | SelectTests
    | SelectAll
    deriving Show

-- | Data type for the "generate" and "build" command arguments
data BuildCmdArgs =
    BuildCmdArgs
        Bool -- ^ Verbose mode
        Bool -- ^ Generate transactional worst-case execution paths
        (Maybe CmpDiagramParam)
        Selection
        (Maybe Platform) -- ^ Only the tests on this platform
        (Maybe ProjectProfile) -- ^ Profile of the tests
    deriving Show

-- | Parser for the "generate" and "build" command arguments
buildCmdArgsParser :: O.Parser BuildCmdArgs
buildCmdArgsParser = BuildCmdArgs
    <$> O.switch (O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable verbose mode")
    <*> O.switch (O.long "gen-transactional-wceps"
        <> O.help "Generate transactional worst-case execution paths")
    <*> O.optional paramParser
    <*> selectionParser
    <*> O.optional (O.option (O.maybeReader checkPlatform)
          ( O.long "platform"
          <> O.metavar "PLATFORM"
          <> O.help "Only the tests on this platform"))
    <*> O.optional (O.option (O.eitherReader readProfile)
          ( O.long "profile"
          <> O.metavar "debug|release"
          <> O.help "Profile of the tests (default: the one of the application, debug in a library)"))

  where

    selectionParser :: O.Parser Selection
    selectionParser =
          (SelectTest <$> O.strOption (O.long "test" <> O.metavar "NAME" <> O.help "One test, on every platform of its list"))
      O.<|> O.flag' SelectTests (O.long "tests" <> O.help "Every test")
      O.<|> O.flag' SelectAll (O.long "all" <> O.help "The application and every test")
      O.<|> pure SelectApp

    readProfile :: String -> Either String ProjectProfile
    readProfile "debug" = Right Debug
    readProfile "release" = Right Release
    readProfile other = Left $ "Unknown profile: " ++ other ++ " (use debug or release)"

    paramParser :: O.Parser CmpDiagramParam
    paramParser =
      paramFlagOnly
      O.<|> paramWithValue

    paramWithValue :: O.Parser CmpDiagramParam
    paramWithValue =
          O.option (O.eitherReader (fmap CmpDiagramParamRange . parseCmpDiagramParam))
            ( O.long "gen-component-diagram"
          <> O.metavar "RNG"
          <> O.help "Use --gen-component-diagram=[list][:depth]]"
            )

    paramFlagOnly :: O.Parser CmpDiagramParam
    paramFlagOnly =
          O.flag' CmpDiagramParamFlag
            ( O.long "gen-component-diagram"
          <> O.help "Use --gen-component-diagram"
            )

    parseCmpDiagramParam :: String -> Either String CmpDiagramRange
    parseCmpDiagramParam s =
      case break (== ':') (trim s) of
        (left, "") ->
          -- no ':': either a pure integer OR a comma list
          case readMaybe left of
            Just n  -> Right (CmpDiagramRange [] (Just n))
            Nothing ->
              let xs = parseItems left
              in if null xs
                  then Left "Expected an integer (e.g. 1) or a non-empty list (e.g. a,b)"
                  else Right (CmpDiagramRange xs Nothing)

        (left, ':' : right) -> do
          -- has ':': must be list on left, int on right
          let xs = parseItems left
          if null xs
            then Left "Expected a non-empty list before ':' (e.g. a,b:2)"
            else case readMaybe (trim right) of
                  Nothing -> Left "Expected an integer after ':' (e.g. a,b:2)"
                  Just n  ->
                    if n <= 0
                      then Left "Depth must be a positive integer"
                      else Right (CmpDiagramRange xs (Just n))

        _ -> Left "Invalid format"

    parseItems :: String -> [String]
    parseItems =
      filter (not . null) . map trim . splitBy ',' . trim

    splitBy :: Char -> String -> [String]
    splitBy _ "" = []
    splitBy c s  =
      let (a, rest) = break (== c) s
      in a : case rest of
              []     -> []
              (_:xs) -> splitBy c xs

    trim :: String -> String
    trim = f . f
      where f = reverse . dropWhile isSpace

monadicTypesMapModules :: TypedProject -> MonadicTypes
monadicTypesMapModules = foldl' monadicTypesMapModule emptyMonadicTypes  . M.elems

  where
    monadicTypesMapModule :: MonadicTypes -> TypedModule -> MonadicTypes
    monadicTypesMapModule prevMap typedModule = runMapMonadicTypesAnnotatedProgram prevMap (typedAST . metadata $ typedModule)

genTWCEPFiles ::
  TerminaConfig
  -> LoweredProject
  -> IO ()
genTWCEPFiles params loweredProject = do
  mapM_ printWCEPModule (M.elems loweredProject)

  where

    printWCEPModule :: BasicBlocksModule -> IO ()
    printWCEPModule bbModule =
      let bbAST = loweredAST . metadata $ bbModule
          wceps = genTransactionalWCEPS bbAST in
      (unless (null wceps) $ do
        let destinationPath = efpFolder params
            twcepFile = destinationPath </> qualifiedName bbModule <.> "twcep"
        createDirectoryIfMissing True (takeDirectory twcepFile)
        TIO.writeFile twcepFile $ runWCEPathPrinter wceps)

genComponentDiagramFile ::
  TerminaConfig
  -> TerminaProgArch a
  -> CmpDiagramParam
  -> IO ()
genComponentDiagramFile params progArch param = do
    let destinationPath = outputFolder params
        cmpFile = destinationPath </> T.unpack (name params) <.> "plantuml"
        result = runPlantUMLCmpGenerator progArch $ case param of
            CmpDiagramParamFlag -> Nothing
            CmpDiagramParamRange range -> Just range
    case result of
        Left err -> die . errorMessage $ T.unpack err
        Right diagram -> do
            createDirectoryIfMissing True (takeDirectory cmpFile)
            TIO.writeFile cmpFile $ runPlantUMLPrinter diagram

genChecksReportFile ::
  TerminaConfig
  -> LoweredProject
  -> M.Map QualifiedName ElaborationReport
  -> IO ()
genChecksReportFile params loweredProject reports = do
    let reportFile = outputFolder params </> "checks" <.> "json"
    createDirectoryIfMissing True (takeDirectory reportFile)
    BL.writeFile reportFile $ checksReport reports (fullPath <$> loweredProject)

genModules ::
  TerminaConfig
  -> Platform
  -- | The map with the option types to generate from defined types 
  -> MonadicTypes
  -- | The project to generate the code from
  -> ElaboratedProject -> IO ()
genModules params plt initialMonadicTypes loweredProject =
  foldM_ printModule initialMonadicTypes (M.elems loweredProject)

  where

    printModule :: MonadicTypes -> ElaboratedModule -> IO MonadicTypes
    printModule currentMonadicTypes bbModule = do
      printSource bbModule
      printHeader currentMonadicTypes bbModule

    printSource :: ElaboratedModule -> IO ()
    printSource bbModule = do
      let destinationPath = outputFolder params
          sourceFile = destinationPath </> "src" </> qualifiedName bbModule <.> "c"
          tAST = elaboratedAST . metadata $ bbModule
      case runGenSourceFile params plt (qualifiedName bbModule) tAST of
        Left err -> die. errorMessage $ show err
        Right cSourceFile -> writeIfChanged sourceFile $ runCPrinter (lineDirectives params) cSourceFile

    printHeader :: MonadicTypes -> ElaboratedModule -> IO MonadicTypes
    printHeader currentMonadicTypes bbModule = do
      let destinationPath = outputFolder params
          tAST = elaboratedAST . metadata $ bbModule
          moduleDeps = (\(ModuleDependency qname _) -> qname) <$> importedModules bbModule
      case runGenHeaderFile params plt (qualifiedName bbModule) moduleDeps tAST currentMonadicTypes of
        Left err -> die . errorMessage $ show err
        Right (cHeaderFile, newMonadicTypes) -> do
          let headerFile = destinationPath </> "include" </> qualifiedName bbModule <.> "h"
          writeIfChanged headerFile $ runCPrinter (lineDirectives params) cHeaderFile
          return newMonadicTypes

genInitFile :: TerminaConfig -> Platform -> ElaboratedProject -> IO ()
genInitFile params plt loweredProject = do
  let initFile = outputFolder params </> "init" <.> "c"
      projectModules = M.toList $ elaboratedAST . metadata <$> loweredProject
  case runGenInitFile params plt initFile projectModules of
    Left err -> die . errorMessage $ show err
    Right cInitFile -> writeIfChanged initFile $ runCPrinter False cInitFile

-- | The headers of the option, status and result types, each when the project
-- uses one of them. A header that is not generated is not expected in the
-- output folder.
genMonadicHeaderFiles :: TerminaConfig -> Platform -> MonadicTypes -> IO [FilePath]
genMonadicHeaderFiles params plt monadicTypes = do
  option <- gen "option" usesOptions runGenOptionHeaderFile
  status <- gen "status" usesStatus runGenStatusHeaderFile
  result <- gen "result" usesResults runGenResultHeaderFile
  return (option ++ status ++ result)

  where

    gen headerName needed generator
      | not needed = return []
      | otherwise = do
          let file = outputFolder params </> "include" </> headerName <.> "h"
          case generator params plt file monadicTypes of
            Left err -> die . errorMessage $ show err
            Right cFile -> writeIfChanged file (runCPrinter (lineDirectives params) cFile) >> return [file]

    notStructOrEnum (TStruct _) = False
    notStructOrEnum (TEnum _) = False
    notStructOrEnum _ = True

    usesOptions = not . S.null . S.filter notStructOrEnum . optionTypes $ monadicTypes
    usesStatus = not . S.null . S.filter notStructOrEnum . statusTypes $ monadicTypes
    usesResults = not . S.null . S.unions . M.elems . M.filterWithKey (\k _ -> notStructOrEnum k) . resultTypes $ monadicTypes

-- | The deployments that the arguments select.
selectedDeployments :: BuildCmdArgs -> IO [Deployment]
selectedDeployments (BuildCmdArgs chatty _ _ selection mPlatform mProfile) = do
    when chatty (putStrLn . debugMessage $ "Reading project configuration from \"termina.yaml\"")
    manifest <- loadManifest >>= either dieWithErrors return
    either (die . errorMessage) return $ selectDeployments manifest selection mPlatform mProfile

selectDeployments :: Manifest -> Selection -> Maybe Platform -> Maybe ProjectProfile -> Either String [Deployment]
selectDeployments manifest selection mPlatform mProfile =
    case selection of
        SelectApp
            | isJust mPlatform || isJust mProfile ->
                Left "--platform and --profile select the platforms and the profile of tests: use them with --test, --tests or --all"
            | otherwise -> maybe (Left noApp) (Right . pure) (appDeployment manifest)
        SelectTest testName ->
            case M.lookup (T.pack testName) tests of
                Nothing -> Left $ "the manifest declares no test " ++ testName
                Just entry -> case testOn (T.pack testName, entry) of
                    [] -> Left $ "the test " ++ testName ++ " does not run on " ++ maybe "" show mPlatform
                    ds -> Right ds
        SelectTests -> nonEmpty (concatMap testOn (M.toList tests))
        SelectAll -> nonEmpty (maybe [] pure (appDeployment manifest) ++ concatMap testOn (M.toList tests))

  where

    tests = manifestTests manifest

    prf = fromMaybe (defaultTestProfile manifest) mProfile

    testOn (testName, entry) =
        [ testDeployment manifest testName entry plt prf
        | plt <- testPlatforms entry, maybe True (== plt) mPlatform ]

    noApp = "the project is a library and has no application: build its tests with --test, --tests or --all"

    nonEmpty [] = Left "no deployment to build"
    nonEmpty ds = Right ds

-- | Command handler for the "generate" command
generateCommand :: BuildCmdArgs -> IO ()
generateCommand args = do
    deployments <- selectedDeployments args
    forM_ deployments $ \deployment -> do
      announce deployments ("Generating " ++ deploymentLabel deployment)
      void (generateDeployment args deployment)

-- | Command handler for the "build" command: it generates each deployment
-- and runs make on its output folder
buildCommand :: BuildCmdArgs -> IO ()
buildCommand args = do
    deployments <- selectedDeployments args
    forM_ deployments $ \deployment -> do
      announce deployments ("Building " ++ deploymentLabel deployment)
      modules <- generateDeployment args deployment
      runMake deployment []
      runMake deployment ["-s", "stripped-hashes"]
      writeProvenance deployment modules

-- | Say which deployment comes next when there are several.
announce :: [Deployment] -> String -> IO ()
announce deployments msg = when (length deployments > 1) (putStrLn . infoMessage $ msg)

runMake :: Deployment -> [String] -> IO ()
runMake deployment targets = do
    let folder = outputFolder (deploymentConfig deployment)
    result <- try (rawSystem "make" (["-C", folder] ++ targets))
    case result of
        Left err -> die . errorMessage $ "Could not run make: " ++ show (err :: IOException)
        Right ExitSuccess -> return ()
        Right (ExitFailure _) -> die . errorMessage $ "make failed for " ++ deploymentLabel deployment

-- | Write provenance.json of a deployment: each module, where its source
-- comes from, and the SHA-256 of its object without debugging information,
-- which the Makefile leaves in bin/modules.sha256.
writeProvenance :: Deployment -> [(QualifiedName, FilePath)] -> IO ()
writeProvenance deployment modules = do
    let config = deploymentConfig deployment
        folder = outputFolder config
        hashesFile = folder </> "bin" </> "modules.sha256"
    hashesExist <- doesFileExist hashesFile
    hashes <- if hashesExist
        then M.fromList . concatMap (toPair . words) . lines <$> readFile hashesFile
        else return M.empty
    let objectOf qname = "bin" </> "src" </> qname <.> "o"
        originOf source = if take 1 (splitDirectories source) == ["tests"] then "test" else "project" :: String
        moduleEntry (qname, source) = object [
            "module" .= map (\c -> if c == '/' then '.' else c) qname,
            "origin" .= originOf source,
            "source" .= source,
            "object" .= objectOf qname,
            "stripped-sha256" .= M.lookup (objectOf qname) hashes
          ]
        provenance = object [
            "name" .= name config,
            "deployment" .= (if isTestBuild config then "test" else "app" :: String),
            "platform" .= platform config,
            "profile" .= profile config,
            "modules" .= map moduleEntry modules
          ]
    BL.writeFile (folder </> "provenance" <.> "json") $
      encodePretty' defConfig { confCompare = keyOrder ["name", "deployment", "platform", "profile", "modules", "module", "origin", "source", "object"] } provenance

  where

    toPair [hash, path] = [(normalise path, hash)]
    toPair _ = []

-- | Generate the C and the Makefile of a deployment. It gives the modules of
-- the deployment with their source files.
generateDeployment :: BuildCmdArgs -> Deployment -> IO [(QualifiedName, FilePath)]
generateDeployment (BuildCmdArgs chatty genTransactionalWCEPs genCmpDiag _ _ _) deployment = do
    let config = deploymentConfig deployment
        plt = deploymentPlatform deployment
        sources = deploymentSources deployment
    when chatty (putStrLn . debugMessage $ "Selected platform: \"" ++ show plt ++ "\"")
    -- | Check the C of the deployment and copy it to the output folder
    when chatty (putStrLn . debugMessage $ "Checking the extern C of the deployment")
    mExtern <- prepareExtern config (deploymentExterns deployment)
    -- | Create output header and source folder if it does not exist
    let outputSrcFolder = outputFolder config </> "src"
    let outputIncludeFolder = outputFolder config </> "include"
    when chatty (putStrLn . debugMessage $ "Creating output source folder (if missing): \"" ++ outputSrcFolder ++ "\"")
    createDirectoryIfMissing True outputSrcFolder
    when chatty (putStrLn . debugMessage $ "Creating output include folder (if missing): \"" ++ outputIncludeFolder ++ "\"")
    createDirectoryIfMissing True outputIncludeFolder
    -- | Load the module that wires the deployment
    let mainModulePath = deploymentMainFolder deployment </> deploymentMainModule deployment <.> "fin"
    when chatty (putStrLn . debugMessage $ "Loading the main module: \"" ++ mainModulePath ++ "\"")
    appModule <- loadTerminaModule (deploymentMainFolder deployment) (deploymentMainModule deployment) sources
    -- | Load the project
    when chatty (putStrLn . debugMessage $ "Loading project modules")
    parsedModules <- loadModules (importedModules appModule) sources
    let parsedProject = M.insert (qualifiedName appModule) appModule parsedModules
    -- | Detect any possible loops in the project
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
    let initialGlobalEnv = makeInitialGlobalEnv (Just config) plt (getPlatformInitialGlobalEnv config plt)
    (typedProject, _finalGlobalEnv) <- typeModules parsedProject initialGlobalEnv orderedDependencies
    -- | Obtain the set of option types
    when chatty (putStrLn . debugMessage $ "Searching for option types")
    let monadicTypes = monadicTypesMapModules typedProject
    -- | Obtain the basic blocks AST of the program
    when chatty (putStrLn . debugMessage $ "Lowering the project modules")
    rawLoweredProject <-
      either
        (\err ->
          TIO.putStrLn (toText err M.empty) >> exitFailure)
        return
        $ lowerProject typedProject
    runLoweredChecks chatty plt rawLoweredProject
    when chatty (putStrLn . debugMessage $ "Performing constant folding")
    (loweredProject, constEnvs) <- constFolding config plt rawLoweredProject
    when chatty (putStrLn . debugMessage $ "Analysing the values of the project modules")
    valueEvidence <- valueAnalysisCheck plt constEnvs loweredProject
    -- | Decide the run-time checks
    when chatty (putStrLn . debugMessage $ "Elaborating the run-time checks")
    let (elaboratedProject, checksReports) = elaborateProject valueEvidence loweredProject
    when chatty (putStrLn . debugMessage $ "Side-effect checking project modules")
    sideEffectCheck plt elaboratedProject
    -- | Obtain the architectural description of the program
    when chatty (putStrLn . debugMessage $ "Checking the architecture of the program")
    programArchitecture <- genArchitecture loweredProject (getPlatformInitialProgram config plt) orderedDependencies
    checkEmitterConnections loweredProject programArchitecture
    checkChannelConnections loweredProject programArchitecture
    checkResourceUsage loweredProject programArchitecture
    checkPoolUsage loweredProject programArchitecture
    checkDequeConnections loweredProject programArchitecture
    checkTaskPriorities loweredProject programArchitecture
    checkSystemExceptAction loweredProject programArchitecture
    checkProjectBoxSources loweredProject programArchitecture
    -- | Generate the code
    when chatty (putStrLn . debugMessage $ "Generating code")
    genModules config plt monadicTypes elaboratedProject
    genInitFile config plt elaboratedProject
    genPlatformCode config plt loweredProject programArchitecture mExtern
    genChecksReportFile config loweredProject checksReports
    monadicHeaders <- genMonadicHeaderFiles config plt monadicTypes
    -- | The C of a module that is no longer in the project would still be
    -- compiled and linked
    let out = outputFolder config
        moduleNames = M.keys elaboratedProject
    removeStaleFiles (out </> "src") [".c"] [out </> "src" </> m <.> "c" | m <- moduleNames]
    removeStaleFiles (out </> "include") [".h"]
      ([out </> "include" </> m <.> "h" | m <- moduleNames] ++ [out </> "include" </> "config" <.> "h"] ++ monadicHeaders)
    when genTransactionalWCEPs $
      when chatty (putStrLn . debugMessage $ "Generating transactional worst-case execution paths") >>
      genTWCEPFiles config loweredProject
    case genCmpDiag of
      Nothing -> return ()
      Just param ->
        when chatty (putStrLn . debugMessage $ "Generating component diagram") >>
        genComponentDiagramFile config programArchitecture param
    when chatty (putStrLn . debugMessage $ "Build completed successfully")
    return [(qualifiedName m, fullPath m) | m <- M.elems elaboratedProject]
