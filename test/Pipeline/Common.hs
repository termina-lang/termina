module Pipeline.Common
  ( runFullBuild
  , runFullProjectBuild
  , runChecksReport
  , runFoldedProject
  , runFullProjectApp
  , runTypedModule
  , runTypedModules
  , renderMainFile
  , renderMainFileOn
  , renderInitFile
  , renderConfigFile
  , renderTestConfigFile
  , buildAndRenderModule
  , compileErrorCode
  , compileTestErrorCode
  , compileTestProjectErrorCode
  , runTestArchitecture
  , runTestApp
  , runTestBuild
  , runTestDescriptor
  , renderTestMainFile
  , compileProjectErrorCode
  , compileErrorMessage
  , compileProjectErrorMessage
  , Failure(..)
  ) where

import Elaboration (ElaborationReport, elaborateProgram)
import Elaboration.Report (checksReport)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import Core.AST (TerminaModule'(..), ModuleImport'(..))

import Semantic.Environment (makeInitialGlobalEnv, Environment)
import Semantic.Types (SemanticAnn)

import Configuration.Configuration (defaultConfig, TerminaConfig (..), DeploymentKind (..))
import Configuration.Platform (Platform(..))
import Generator.Environment (getPlatformInitialGlobalEnv)
import Generator.CodeGen.Module (runGenSourceFile)
import Generator.CodeGen.Application.Glue (runGenMainFile)
import Generator.TestDescriptor (genTestDescriptor)
import Core.TestCases (NumberedCase, typedCases)
import Data.Word (Word32)
import Generator.CodeGen.Application.Config (runGenConfigFile)
import Generator.CodeGen.Application.Initialization (runGenInitFile)
import Generator.LanguageC.Printer (runCPrinter)
import Lowering.AST (AnnotatedProgram)
import qualified Semantic.AST as SAST

import Command.Types
import Command.Utils (CheckFailure(..), sortProjectDepsOrLoop)
import Modules.Modules (TerminaModuleData(..), ModuleDependency(..))
import Modules.Utils (buildModuleName)
import Parser.Errors (Error(..), ParsingErrors)
import Utils.Annotations (QualifiedName, annotateError, Location(Internal))

import Command.Pipeline (CheckedModules(..), checkModules, typeProject)
import Analysis.SMTBounds (noSMTProofs)
import Data.Functor.Identity (runIdentity)
import ControlFlow.Architecture.Types (TerminaProgArch)
import Utils.Errors (ErrorMessage(errorIdent, toText))

-- | Drives a set of in-memory modules through the *full* transpiler pipeline,
-- exactly as @Command.Build.buildCommand@ does, and renders the C source of
-- every module. The input is a list of @(qualified name, source)@ pairs; a
-- module's @import@ clauses are resolved against the other names in the list,
-- so multi-module projects (cross-module types, visibility, imports) are
-- exercised without touching disk.
--
-- Unlike @Codegen.Common.renderSource@ (parse -> typecheck -> basic blocks ->
-- codegen, the slice the IT golden specs exercise), this runs the stages that
-- operate on the whole project: dependency ordering, project-wide type
-- checking with the environment threaded across modules, exit-path checking,
-- variable-usage checking, constant simplification, architecture generation
-- with its connection/usage/box checks, and the depth constant folding pass.
--
-- The pipeline stages are pure; @Command.Common@ only wraps them in IO to print
-- errors and @exitFailure@. We call the pure runners directly so a failure is a
-- comparable 'Text' (@Left@) the spec can assert on, never a process exit.
runFullProjectBuild :: [(QualifiedName, String)] -> Either Failure (M.Map QualifiedName Text)
runFullProjectBuild = runFullProjectBuildWith configParams

-- | 'runFullProjectBuild' under a given configuration.
runFullProjectBuildWith :: TerminaConfig -> [(QualifiedName, String)]
  -> Either Failure (M.Map QualifiedName Text)
runFullProjectBuildWith cfg sources = do
  result <- runProjectPipeline cfg sources
  mapM (renderModule cfg) (elaboratedModules result)

-- | The report of the run-time checks that @termina build@ writes to
-- @output/checks.json@, for a single module named @test@.
runChecksReport :: String -> Text
runChecksReport input =
  case runProjectPipeline configParams [("test", input)] of
    Left err -> failMessage err
    Right result ->
      TE.decodeUtf8 . BL.toStrict $
        checksReport (checksReports result) (fullPath <$> foldedModules result)

-- | The basic-block modules of a project with the constants folded, which are
-- those the SMT prover receives.
runFoldedProject :: [(QualifiedName, String)] -> Either Failure LoweredProject
runFoldedProject sources = foldedModules <$> runProjectPipeline configParams sources

-- | Drives the same full pipeline as 'runFullProjectBuild' but stops before
-- per-module source rendering, returning the whole-program architecture and
-- the per-module basic-block programs in dependency order. The application
-- glue (the @main@ and @init@ files) is generated from these two artifacts
-- rather than from a single source module, so a spec that wants to exercise
-- the glue renders it via 'renderMainFile' / 'renderInitFile'.
runFullProjectApp ::
  [(QualifiedName, String)]
  -> Either Failure (TerminaProgArch SemanticAnn, [(QualifiedName, AnnotatedProgram SemanticAnn)])
runFullProjectApp = runFullProjectAppWith configParams

-- | 'runFullProjectApp' under a given configuration.
runFullProjectAppWith ::
  TerminaConfig
  -> [(QualifiedName, String)]
  -> Either Failure (TerminaProgArch SemanticAnn, [(QualifiedName, AnnotatedProgram SemanticAnn)])
runFullProjectAppWith cfg sources = do
  result <- runProjectPipeline cfg sources
  let prjprogs = [ (m, loweredAST . metadata $ foldedModules result M.! m) | m <- moduleOrder result ]
  pure (programArch result, prjprogs)

-- | What the pipeline leaves for the specs to render or inspect.
data PipelineResult = PipelineResult
  {
    foldedModules :: LoweredProject
  , moduleOrder :: [QualifiedName]
  , programArch :: TerminaProgArch SemanticAnn
  , elaboratedModules :: ElaboratedProject
  , checksReports :: M.Map QualifiedName ElaborationReport
  }

-- | The full pipeline up to (and including) the architecture checks, shared by
-- 'runFullProjectBuild' (which renders each source module) and
-- 'runFullProjectApp' (which renders the application glue).
runProjectPipeline ::
  TerminaConfig
  -> [(QualifiedName, String)]
  -> Either Failure PipelineResult
runProjectPipeline cfg sources = do
  let files = M.fromList [ (qname, pack src) | (qname, src) <- sources ]
  parsedProject <- M.fromList <$> mapM parseModule sources
  ordered <- orderModules parsedProject
  checked <- checkedOrFailure files (snd (runIdentity (checkModules noSMTProofs cfg TestPlatform S.empty parsedProject ordered)))
  pure (PipelineResult (checkedLowered checked) ordered (checkedArchitecture checked)
    (checkedElaborated checked) (checkedReports checked))

-- | The modules of a project that keep their types after its checks, and the
-- code of the error that stops them.
runTypedModules :: [(QualifiedName, String)] -> ([QualifiedName], Maybe String)
runTypedModules sources = either (\f -> ([], Just (code f))) id $ do
  let files = M.fromList [ (qname, pack src) | (qname, src) <- sources ]
  parsedProject <- M.fromList <$> mapM parseModule sources
  ordered <- orderModules parsedProject
  let (typedProject, result) = runIdentity (checkModules noSMTProofs configParams TestPlatform S.empty parsedProject ordered)
  pure (M.keys typedProject, either (Just . code) (const Nothing) (checkedOrFailure files result))

  where

    code = T.unpack . failCode

-- | The typed AST of one module of a project, which is the stage the language
-- server keeps and the one its index is built from. The stages after it, basic
-- blocks included, produce a different AST and answer other questions.
runTypedModule :: QualifiedName -> [(QualifiedName, String)]
  -> Either Failure (SAST.AnnotatedProgram SemanticAnn)
runTypedModule target sources = do
  let files = M.fromList [ (qname, pack src) | (qname, src) <- sources ]
  parsedProject <- M.fromList <$> mapM parseModule sources
  ordered <- orderModules parsedProject
  let (typedProject, typeFailure) = typeProject S.empty (initialEnv configParams) parsedProject ordered
  checkedOrFailure files (maybe (Right ()) Left typeFailure)
  pure (typedAST . metadata $ typedProject M.! target)

-- | Render the generated @main@ file (task/emitter installation, the app init
-- entry point) from a program architecture, collapsing a codegen failure into
-- the returned 'Text'.
renderMainFile :: TerminaProgArch SemanticAnn -> Either Text Text
renderMainFile = renderMainFileWith configParams

-- | 'renderMainFile' under a given configuration.
renderMainFileWith :: TerminaConfig -> TerminaProgArch SemanticAnn -> Either Text Text
renderMainFileWith cfg = renderMainFileWithOn cfg TestPlatform

-- | 'renderMainFile' for a given target platform.
renderMainFileOn :: Platform -> TerminaProgArch SemanticAnn -> Either Text Text
renderMainFileOn = renderMainFileWithOn configParams

renderMainFileWithOn :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn -> Either Text Text
renderMainFileWithOn cfg plt = renderMainFileWithCases cfg plt []

-- | 'renderMainFile' with the cases of a unit test.
renderMainFileWithCases :: TerminaConfig -> Platform -> [NumberedCase] -> TerminaProgArch SemanticAnn -> Either Text Text
renderMainFileWithCases cfg plt cases progArch =
  case runGenMainFile cfg plt "main" 0 cases progArch of
    Left err -> Left . T.pack $ show err
    Right cFile -> Right $ runCPrinter False cFile

-- | Render the generated @config.h@ (the identifiers of the deployment) from a
-- program architecture, collapsing a codegen failure into the returned 'Text'.
renderConfigFile :: TerminaProgArch SemanticAnn -> Either Text Text
renderConfigFile = renderConfigFileWith configParams

-- | 'renderConfigFile' for the deployment of a test.
renderTestConfigFile :: TerminaProgArch SemanticAnn -> Either Text Text
renderTestConfigFile = renderConfigFileWith testConfigParams

renderConfigFileWith :: TerminaConfig -> TerminaProgArch SemanticAnn -> Either Text Text
renderConfigFileWith cfg progArch =
  case runGenConfigFile cfg TestPlatform "config" progArch of
    Left err -> Left . T.pack $ show err
    Right cFile -> Right $ runCPrinter False cFile

-- | Render the generated @init@ file (global object initialization and port
-- wiring) from the per-module basic-block programs, collapsing a codegen
-- failure into the returned 'Text'.
renderInitFile :: [(QualifiedName, AnnotatedProgram SemanticAnn)] -> Either Text Text
renderInitFile prjprogs =
  case runGenInitFile configParams TestPlatform "init" (fmap (fst . elaborateProgram M.empty) <$> prjprogs) of
    Left err -> Left . T.pack $ show err
    Right cFile -> Right $ runCPrinter False cFile

-- | The error code (@errorIdent@: \"SE-042\", \"BE-001\", \"AE-007\",
-- \"CF-…\") raised by the first failing pipeline stage for a single-module
-- program named @test@, or 'Nothing' if it compiles cleanly. This is the
-- single assertion point for every negative test, whatever stage the error
-- belongs to.
compileErrorCode :: String -> Maybe Text
compileErrorCode input = compileProjectErrorCode [("test", input)]

-- | Multi-module variant of 'compileErrorCode'.
compileProjectErrorCode :: [(QualifiedName, String)] -> Maybe Text
compileProjectErrorCode = compileProjectErrorCodeWith configParams

-- | 'compileErrorCode' for the deployment of a test, the only one that may
-- use test classes, test instances and the components of a unit test.
compileTestErrorCode :: String -> Maybe Text
compileTestErrorCode input =
  compileProjectErrorCodeWith testConfigParams [("test", input)]

-- | Multi-module variant of 'compileTestErrorCode'.
compileTestProjectErrorCode :: [(QualifiedName, String)] -> Maybe Text
compileTestProjectErrorCode = compileProjectErrorCodeWith testConfigParams

-- | The checked architecture of the deployment of a test.
runTestArchitecture :: String -> Either Failure (TerminaProgArch SemanticAnn)
runTestArchitecture input = fst <$> runFullProjectAppWith testConfigParams [("test", input)]

-- | 'runFullProjectApp' for the deployment of a test, in a module named @test@.
runTestApp :: String
  -> Either Failure (TerminaProgArch SemanticAnn, [(QualifiedName, AnnotatedProgram SemanticAnn)])
runTestApp input = runFullProjectAppWith testConfigParams [("test", input)]

-- | 'renderMainFile' for the deployment of a test.
renderTestMainFile :: (TerminaProgArch SemanticAnn, [(QualifiedName, AnnotatedProgram SemanticAnn)]) -> Either Text Text
renderTestMainFile (progArch, prjprogs) = do
  cases <- either (Left . pack) Right (typedCases (concatMap snd prjprogs))
  renderMainFileWithCases testConfigParams TestPlatform cases progArch

-- | The descriptor of a unit test in a module named @test@, and its stamp.
runTestDescriptor :: String -> Either Failure (BL.ByteString, Word32)
runTestDescriptor input = do
  result <- runProjectPipeline testConfigParams [("test", input)]
  cases <- either (\err -> Left (Failure (pack err) (pack err))) Right
    (typedCases (concatMap (loweredAST . metadata) (M.elems (foldedModules result))))
  pure (genTestDescriptor testConfigParams (foldedModules result) (programArch result) cases)

-- | 'runFullProjectBuild' for the deployment of a test, in a module named @test@.
runTestBuild :: String -> Either Failure (M.Map QualifiedName Text)
runTestBuild input = runFullProjectBuildWith testConfigParams [("test", input)]

testConfigParams :: TerminaConfig
testConfigParams = configParams { deploymentKind = TestBuild (pack "test") }

-- | 'compileProjectErrorCode' under a given configuration, for an error that
-- only a project with a feature switched on can reach.
compileProjectErrorCodeWith :: TerminaConfig -> [(QualifiedName, String)] -> Maybe Text
compileProjectErrorCodeWith cfg =
  either (Just . failCode) (const Nothing) . runFullProjectBuildWith cfg

-- | The message the compiler prints for the first failing stage, which is what
-- the golden of messages fixes.
compileErrorMessage :: String -> Maybe Text
compileErrorMessage input = compileProjectErrorMessage [("test", input)]

-- | Multi-module variant of 'compileErrorMessage'.
compileProjectErrorMessage :: [(QualifiedName, String)] -> Maybe Text
compileProjectErrorMessage =
  either (Just . failMessage) (const Nothing) . runFullProjectBuild

-- | Single-module convenience: build a one-module project named @test@ (the
-- name the IT/Codegen specs use) and return the rendered C for it, collapsing
-- any pipeline failure into the returned 'Text' so a spec can assert on it.
runFullBuild :: String -> Text
runFullBuild input = buildAndRenderModule "test" [("test", input)]

-- | Build a project and return the rendered C of @target@, collapsing a
-- pipeline failure (or a missing target) into the returned 'Text'.
buildAndRenderModule :: QualifiedName -> [(QualifiedName, String)] -> Text
buildAndRenderModule target sources =
  case runFullProjectBuild sources of
    Left err -> failMessage err
    Right rendered ->
      M.findWithDefault
        (pack $ "Module not found in project: " ++ target)
        target rendered

-- Pipeline stages -----------------------------------------------------------

parseModule :: (QualifiedName, String) -> Either Failure (QualifiedName, ParsedModule)
parseModule (qname, src) =
  -- | The source name the parser stamps on every position is the name of the
  -- module, as in a real build, so that the error printer finds its source.
  case runParser terminaModuleParser qname qname src of
    Left err -> Left (failure (M.singleton qname (pack src))
      (annotateError Internal (EParseError err) :: ParsingErrors))
    Right (Termina imports prog) -> do
      deps <- mapM toDep imports
      pure (qname, TerminaModuleData qname qname deps [] (pack src) (ParsingData prog))

  where

    toDep (ModuleImport ident ann) =
      case buildModuleName ann ident of
        Left perr ->
          let text = pack $ "Import error in " ++ qname ++ ": " ++ show perr
          in Left (Failure text text)
        Right dep -> Right (ModuleDependency dep ann)

orderModules :: ParsedProject -> Either Failure [QualifiedName]
orderModules parsedProject =
  case sortProjectDepsOrLoop (M.map importedModules parsedProject) of
    Left loop -> Left (failure
      (M.map sourcecode (M.mapKeys id parsedProject))
      (annotateError Internal (EImportedFilesLoop loop) :: ParsingErrors))
    Right ordered -> Right ordered

renderModule :: TerminaConfig -> ElaboratedModule -> Either Failure Text
renderModule cfg eModule =
  case runGenSourceFile cfg TestPlatform (qualifiedName eModule)
         (elaboratedAST . metadata $ eModule) of
    Left err -> Left (Failure (T.pack (show err)) (T.pack (show err)))
    Right cSourceFile -> Right $ runCPrinter False cSourceFile

-- Shared configuration ------------------------------------------------------

configParams :: TerminaConfig
configParams = defaultConfig "test" TestPlatform

initialEnv :: TerminaConfig -> Environment
initialEnv cfg = makeInitialGlobalEnv (Just cfg) TestPlatform
               (getPlatformInitialGlobalEnv cfg TestPlatform)


-- Error plumbing ------------------------------------------------------------

-- | The diagnostic code of an error (@errorIdent@: \"SE-042\", \"AE-007\"…).
--
-- Every internal/defensive constructor shares the code \"Internal\", which is
-- useless when a test fails on one. So for that single case we also append the
-- error's @show@ (which carries the actual constructor, e.g. @ENotConstant@),
-- truncated to keep the failure readable: \"Internal: ...ENotConstant...\".
-- | What a failing stage reports: the code every negative test asserts on, and
-- the message the user reads, which the golden of messages fixes.
data Failure = Failure
  {
    failCode :: Text
  , failMessage :: Text
  }

failure :: (ErrorMessage e, Show e) => M.Map FilePath Text -> e -> Failure
failure files err = Failure (errCode err) (toText err files)

errCode :: (ErrorMessage e, Show e) => e -> Text
errCode err =
  let code = errorIdent err in
  if code == pack "Internal"
    then code <> pack ": " <> pack (take 240 (show err))
    else code

-- | The result of a stage of the transpiler, or what its failure reports.
checkedOrFailure :: M.Map FilePath Text -> Either CheckFailure a -> Either Failure a
checkedOrFailure files =
  either (\f -> Left (Failure (code f) (failureMessage f files))) Right

  where

    code :: CheckFailure -> Text
    code checkFailure =
      if failureCode checkFailure == pack "Internal"
        then failureCode checkFailure <> pack ": "
               <> pack (take 240 (failureShown checkFailure))
        else failureCode checkFailure
