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
import qualified Data.Map.Strict as M
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
import EFP.Schedulability.WCEPath.Parsing
import EFP.Schedulability.WCEPath.TypeChecking
import Semantic.Types
import EFP.Schedulability.WCEPath.Types
import ControlFlow.Architecture.Types
import EFP.Schedulability.WCET.Parsing (terminaWCETParser)
import EFP.Schedulability.WCET.TypeChecking
import EFP.Schedulability.WCET.Types
import Options.Applicative
import EFP.Schedulability.RT.Parsing (terminaRTParser)
import EFP.Schedulability.RT.TypeChecking
import EFP.Schedulability.RT.Semantic.Types
import EFP.Schedulability.RT.Semantic.AST
import EFP.Schedulability.RT.Flatten
import EFP.Schedulability.TransPath.Generator (runTransPathGenerator)
import EFP.Schedulability.TransPath.Types
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.PlantUML
import EFP.Schedulability.PlantUML.Printer
import Text.Printf
import EFP.Schedulability.RT.Printer
import EFP.Schedulability.MAST.Generator
import EFP.Schedulability.MAST.Printer

-- | Data type for the "sched" command arguments
data SchedCmdArgs =
    SchedCmdArgs
        FilePath -- ^ Path to the RT model file
        Bool -- ^ Verbose mode
        Bool -- ^ Generate PlantUML diagrams
        Bool -- ^ Generate intermediate RT model
    deriving (Show,Eq)


-- | Parser for the "sched" command arguments
schedCmdArgsParser :: O.Parser SchedCmdArgs
schedCmdArgsParser = SchedCmdArgs
    <$> argument str (metavar "RT_MODEL_FILE"
        <> help "Path to the RT model file")
    <*> O.switch (O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable verbose mode")
    <*> O.switch (O.long "gen-plantuml"
        <> O.help "Generate PlantUML diagrams for the transactional worst-case execution paths")
    <*> O.switch (O.long "gen-intermediate-rt"
        <> O.help "Generate intermediate RT model after type checking and flattening")

loadRTModule :: FilePath -> IO ParsedRTModule
loadRTModule rtModelFile = do
  -- | Check RT model file extension
  when (takeExtension rtModelFile /= ".rt") $
      die . errorMessage $ "RT model file must have .rt extension"
  -- | Check that RT model file exists
  sourceFileExists <- doesFileExist rtModelFile
  unless sourceFileExists (die . errorMessage $ "RT model file \"" ++ rtModelFile ++ "\" does not exist")
  src_code <- TE.decodeUtf8 <$> BS.readFile rtModelFile
  mod_time <- getModificationTime rtModelFile
  -- parse it
  case runParser terminaRTParser rtModelFile rtModelFile (T.unpack src_code) of
    Left err ->
      let pErr = annotateError (Position rtModelFile (errorPos err) (errorPos err)) (EParseError err)
          fileMap = M.singleton rtModelFile src_code
      in
      TIO.putStrLn (toText pErr fileMap) >> exitFailure
    Right term -> return $ TerminaModuleData rtModelFile rtModelFile mod_time [] [] src_code (ParsingRTData term)

typeRTModule :: TerminaProgArch SemanticAnn
  -> WCEPathMap WCEPSemAnn
  -> BasicBlocksProject
  -> WCEPProject
  -> ParsedRTModule -> IO TypedRTModule
typeRTModule arch trPathMap bbProject pathProject rtModule = do
  let result = runRTTypeChecking arch trPathMap (parsedRTAST . metadata $ rtModule)
  case result of
    (Left err) ->
      -- | Create the source files map. This map will be used to obtain the source files that
      -- will be feed to the error pretty printer. The source files map must use as key the
      -- path of the source file and as element the text of the source file.
      let fileMap = M.singleton (fullPath rtModule) (sourcecode rtModule)
          sourceFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                fileMap bbProject
          pathFilesMap =
            M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                sourceFilesMap pathProject in
      TIO.putStrLn (toText err pathFilesMap) >> exitFailure
    (Right tyElems) -> return $ TerminaModuleData
        (qualifiedName rtModule)
        (fullPath rtModule)
        (modificationTime rtModule)
        []
        []
        (sourcecode rtModule)
        (SemanticRTData tyElems)

flattenRTModule :: TerminaProgArch SemanticAnn 
  -> WCEPathMap WCEPSemAnn
  -> TypedRTModule -> IO (RTTransactionMap RTSemAnn, RTSituationMap RTSemAnn)
flattenRTModule arch wcepMap rtModule = do
    let flattenResult = runFlattenModule arch wcepMap (typedRTAST . metadata $ rtModule)
    case flattenResult of
        Left err ->
            -- | Create the source files map. This map will be used to obtain the source files that
            -- will be feed to the error pretty printer. The source files map must use as key the
            -- path of the source file and as element the text of the source file.
            let fileMap = M.singleton (fullPath rtModule) (sourcecode rtModule) in
            TIO.putStrLn (toText err fileMap) >> exitFailure
        Right result -> return result

genTransPath :: TypedRTModule
  -> WCEPProject
  -> TerminaProgArch SemanticAnn
  -> TerminaConfig
  -> WCEPathMap WCEPSemAnn
  -> WCETimesMap WCETSemAnn
  -> RTElement RTSemAnn -> IO (TransactionPath TRPSemAnn)
genTransPath rtModule pathProject arch config wcepMap wcetMap transaction = do
    let transPathResult = runTransPathGenerator arch config wcepMap wcetMap transaction
    case transPathResult of
        Left err ->
            -- | Create the source files map. This map will be used to obtain the source files that
            -- will be feed to the error pretty printer. The source files map must use as key the
            -- path of the source file and as element the text of the source file.
            let fileMap = M.singleton (fullPath rtModule) (sourcecode rtModule)
                pathFilesMap =
                  M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap)
                      fileMap pathProject in
            TIO.putStrLn (toText err pathFilesMap) >> exitFailure
        Right trPaths -> return trPaths

-- | Load the files containing the transactional worst-case execution paths
loadWCETModules
  :: BasicBlocksProject
    -> FilePath
  -> IO WCETProject
loadWCETModules bbProject efpPath = do
  foldM loadWCETModules' M.empty (M.elems bbProject)

  where

  loadWCETModules' :: WCETProject
    -- Module whose transactional worst-case execution paths are being loaded
    -> BasicBlocksModule
    -> IO WCETProject
  loadWCETModules' fsLoaded bbModule = do
    let fullP = efpPath </> (qualifiedName bbModule <.> "wcet")
        filePath = qualifiedName bbModule
    sourceFileExists <- doesFileExist fullP
    -- If the file containing the transactional worst-case execution paths
    -- for the module does not exist, skip it
    if not sourceFileExists
      then return fsLoaded
      else do
        loadedModule <- loadWCETModule fullP filePath
        pure $ M.insert (qualifiedName loadedModule) loadedModule fsLoaded

  -- | Load file containing transactional worst-case execution paths
  loadWCETModule ::
    FilePath
    -> FilePath
    -> IO WCETModule
  loadWCETModule fullP filePath = do
    -- read it
    src_code <- TE.decodeUtf8 <$> BS.readFile fullP
    mod_time <- getModificationTime fullP
    -- parse it
    case runParser terminaWCETParser filePath fullP (T.unpack src_code) of
      Left err ->
        let pErr = annotateError (Position filePath (errorPos err) (errorPos err)) (EParseError err)
            fileMap = M.singleton fullP src_code
        in
        TIO.putStrLn (toText pErr fileMap) >> exitFailure
      Right term -> return $ TerminaModuleData filePath fullP mod_time [] [] src_code (WCETData term)

-- | Type check the transactional worst-case execution paths of the project modules
typeWCETModules :: TerminaProgArch SemanticAnn
  -> WCEPathMap WCEPSemAnn
  -> BasicBlocksProject
  -> WCETProject
  -> IO (WCETimesMap WCETSemAnn)
typeWCETModules arch wcepMap bbProject wcetProject =
  foldM typeWCETModules' M.empty (M.elems wcetProject)

  where

    typeWCETModules' :: WCETimesMap WCETSemAnn -> WCETModule -> IO (WCETimesMap WCETSemAnn)
    typeWCETModules' wcetTimesMap wcetModule = do
      let result = runWCETTypeChecking arch wcepMap wcetTimesMap (wcetAST . metadata $ wcetModule)
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
                    sourceFilesMap wcetProject in
          TIO.putStrLn (toText err pathFilesMap) >> exitFailure
        (Right newMap) -> return newMap

-- | Load the files containing the transactional worst-case execution paths
loadWCEPathModules
  :: BasicBlocksProject
    -> FilePath
  -> IO WCEPProject
loadWCEPathModules bbProject efpPath = do
  foldM loadWCEPathModules' M.empty (M.elems bbProject)

  where

  loadWCEPathModules' :: WCEPProject
    -- Module whose transactional worst-case execution paths are being loaded
    -> BasicBlocksModule
    -> IO WCEPProject
  loadWCEPathModules' fsLoaded bbModule = do
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
typeWCEPathModules :: TerminaProgArch SemanticAnn
  -> BasicBlocksProject
  -> WCEPProject
  -> IO (WCEPathMap WCEPSemAnn)
typeWCEPathModules arch bbProject pathProject =
  foldM typeWCEPathModules' M.empty (M.elems pathProject)

  where

    typeWCEPathModules' :: WCEPathMap WCEPSemAnn -> TransPathModule -> IO (WCEPathMap WCEPSemAnn)
    typeWCEPathModules' wcePathMap transPathModule = do
      let result = runWCEPathTypeChecking arch wcePathMap (transPathAST . metadata $ transPathModule)
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

genPlantUMLModels :: TerminaConfig -> RTSituationMap RTSemAnn -> TransPathMap TRPSemAnn -> IO ()
genPlantUMLModels config sitMap trPathMap = do
  forM_ (M.elems sitMap) $ \case
    RTSituation situationName evMap (RTSitTy _) ->
      forM_ (M.elems evMap) $ \case 
          (RTEventBursty eventId _ transactionName _ _ _ _ ) -> do
            case M.lookup transactionName trPathMap of
              Just trPath -> genPlantUMLModelForTransaction config situationName eventId transactionName trPath
              Nothing -> 
                TIO.putStrLn ("\x1b[31m[error]\x1b[0m: No transactional path found for transaction " 
                    <> T.pack transactionName <> " in situation " <> T.pack situationName) >> exitFailure
          (RTEventPeriodic eventId _ transactionName _ _ ) -> do
            case M.lookup transactionName trPathMap of
              Just trPath -> genPlantUMLModelForTransaction config situationName eventId transactionName trPath
              Nothing -> 
                TIO.putStrLn ("\x1b[31m[error]\x1b[0m: No transactional path found for transaction " 
                    <> T.pack transactionName <> " in situation " <> T.pack situationName) >> exitFailure
    _ -> TIO.putStrLn "\x1b[31m[error]\x1b[0m: Unexpected RT element when generating PlantUML diagrams for transactional paths" >> exitFailure

genPlantUMLModelForTransaction :: TerminaConfig -> Identifier -> Identifier -> Identifier -> TransactionPath TRPSemAnn -> IO ()
genPlantUMLModelForTransaction config situationName eventId transactionName (SimpleTransactionPath initialStep opMap _) = do
  let destinationPath = outputFolder config </> efpFolder config
      base = destinationPath </> situationName
      total = product (map length (M.elems opMap))
      width_trpath = length (show (max 0 (total - 1)))
  zipWithM_ (\stMap target -> do
      let result = runPlantUMLGenerator eventId initialStep stMap
      case result of
        Left err ->
          TIO.putStrLn err >> exitFailure
        Right diagram -> do
          let targetFile = base </> transactionName </> target <.> "plantuml"
          createDirectoryIfMissing True (takeDirectory targetFile)
          TIO.writeFile targetFile (runPlantUMLPrinter diagram)) (sequenceA opMap) [transactionName ++ "__trpath_" ++ printf "%0*d" width_trpath idx | idx <- [(0 :: Integer) ..]]
genPlantUMLModelForTransaction _ situationName _ transactionName _ = 
  TIO.putStrLn ("\x1b[31m[error]\x1b[0m: Invalid transactionalPath for transaction " 
      <> T.pack transactionName <> " in situation " <> T.pack situationName) >> exitFailure

printIntermediateRT :: FilePath
  -> TerminaConfig
  -> RTTransactionMap RTSemAnn
  -> RTSituationMap RTSemAnn
  -> IO ()
printIntermediateRT rtModelFile config flatTrMap sitMap = do
    let destinationPath = outputFolder config </> efpFolder config
        targetFile = destinationPath </> "flat__" ++ takeBaseName rtModelFile <.> "rt"
    createDirectoryIfMissing True (takeDirectory targetFile)
    TIO.writeFile targetFile (runRTPrinter (M.elems flatTrMap ++ M.elems sitMap))
  

genPickedEvents :: TransPathMap TRPSemAnn 
  -> RTEvent RTSemAnn
  -> IO [SelectedEvent TRPSemAnn]
genPickedEvents trPathMap (RTEventBursty eventId emitterId transactionId interval (TInteger arrivals _) deadlines _) =
  case M.lookup transactionId trPathMap of
    Just (SimpleTransactionPath initialStep opMap _) ->
        mapM (\stMap ->
              return $ SelectedEventBursty eventId emitterId transactionId initialStep stMap interval arrivals deadlines
            ) (sequenceA opMap)
    Just _ -> 
      TIO.putStrLn ("\x1b[31m[error]\x1b[0m: Invalid transactional path for transaction " 
          <> T.pack transactionId <> " when generating picked events") >> exitFailure
    Nothing -> 
      TIO.putStrLn ("\x1b[31m[error]\x1b[0m: No transactional path found for transaction " 
          <> T.pack transactionId <> " when generating picked events") >> exitFailure
genPickedEvents trPathMap (RTEventPeriodic eventId emitterId transactionId deadlines _) =
  case M.lookup transactionId trPathMap of
    Just (SimpleTransactionPath initialStep opMap _) ->
      mapM (\stMap ->
            return $ SelectedEventPeriodic eventId emitterId transactionId initialStep stMap deadlines
          ) (sequenceA opMap)
    Just _ -> 
      TIO.putStrLn ("\x1b[31m[error]\x1b[0m: Invalid transactional path for transaction " 
          <> T.pack transactionId <> " when generating picked events") >> exitFailure
    Nothing -> 
      TIO.putStrLn ("\x1b[31m[error]\x1b[0m: No transactional path found for transaction " 
          <> T.pack transactionId <> " when generating picked events") >> exitFailure

genMASTModels :: TerminaConfig -> TerminaProgArch SemanticAnn -> RTSituationMap RTSemAnn -> TransPathMap TRPSemAnn -> IO ()
genMASTModels config arch sitMap trPathMap = do
  let destinationPath = outputFolder config </> efpFolder config
  forM_ (M.elems sitMap) $ \case
    RTSituation situationName evMap (RTSitTy _) -> do
      let base = destinationPath </> situationName 
      pickedEvents <- mapM (genPickedEvents trPathMap) (M.elems evMap) 
      let numPickedEvents = product (map length pickedEvents)
          width_pickedEvents = length (show (max 0 (numPickedEvents - 1)))
      TIO.putStrLn $
        "\x1b[34m[info]\x1b[0m: Generating scheduling models for situation " <> T.pack situationName
        <> " -> event combinations: " <> T.pack (show numPickedEvents)
      zipWithM_ (\peList modelName ->
        let result = runMASTModelGenerator config arch modelName peList in
        case result of
          Left err -> TIO.putStrLn (toText err M.empty) >> exitFailure
          Right model -> do
            let targetFile = base </> modelName <.> "mast"
            createDirectoryIfMissing True (takeDirectory targetFile)
            TIO.writeFile targetFile (runMASTPrinter model))
        (sequenceA pickedEvents) [situationName ++ "__" ++ printf "%0*d" width_pickedEvents idx | idx <- [(0 :: Integer) ..]]
    _ -> TIO.putStrLn "\x1b[31m[error]\x1b[0m: Unexpected RT element when generating MAST models for transactional paths" >> exitFailure

-- | Command handler for the "sched" command
schedCommand :: SchedCmdArgs -> IO ()
schedCommand (SchedCmdArgs rtModelFile chatty plantUML genIntermediateRT) = do
    when chatty (putStrLn . debugMessage $ "Reading project configuration from \"termina.yaml\"")
    -- | Read the termina.yaml file
    config <- loadConfig >>= either (
            \err -> TIO.putStrLn (T.pack $ show err) >> exitFailure
          ) return
    -- | Decode the selected platform field
    plt <- maybe (die . errorMessage $ "Unsupported platform: \"" ++ show (platform config) ++ "\"") return $ checkPlatform (T.unpack $ platform config)
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
    pathProject <- loadWCEPathModules bbProject (efpFolder config)
    wcepMap <- typeWCEPathModules programArchitecture bbProject pathProject
    when chatty (putStrLn . debugMessage $ "Transactional worst-case execution paths type checked successfully")
    -- | Load the transactional worst-case execution times
    when chatty (putStrLn . debugMessage $ "Loading transactional worst-case execution times")
    wcetProject <- loadWCETModules bbProject (efpFolder config)
    wcetMap <- typeWCETModules programArchitecture wcepMap bbProject wcetProject
    when chatty (putStrLn . debugMessage $ "Transactional worst-case execution times type checked successfully")
    when chatty (putStrLn . debugMessage $ "Loading RT model")
    rtModule <- loadRTModule rtModelFile
    typedRTModule <- typeRTModule programArchitecture wcepMap bbProject pathProject rtModule
    when chatty (putStrLn . debugMessage $ "RT model type checked successfully")
    when chatty (putStrLn . debugMessage $ "Flattening RT model transactions")
    (flatTrMap, sitMap) <- flattenRTModule programArchitecture wcepMap typedRTModule
    when chatty (putStrLn . debugMessage $ "RT model flattened successfully")
    when genIntermediateRT $ printIntermediateRT rtModelFile config flatTrMap sitMap
    when chatty (putStrLn . debugMessage $ "Generating transactional paths")
    trPathMap <- mapM (genTransPath typedRTModule pathProject programArchitecture config wcepMap wcetMap) flatTrMap
    when plantUML (genPlantUMLModels config sitMap trPathMap)
    when chatty (putStrLn . debugMessage $ "Generating scheduling models")
    genMASTModels config programArchitecture sitMap trPathMap
    when chatty (putStrLn . debugMessage $ "Scheduling models generated successfully")
