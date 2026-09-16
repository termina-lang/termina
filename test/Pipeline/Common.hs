module Pipeline.Common
  ( runFullBuild
  , runFullProjectBuild
  , runFullProjectApp
  , renderMainFile
  , renderInitFile
  , buildAndRenderModule
  , compileErrorCode
  , compileProjectErrorCode
  , compileErrorMessage
  , compileProjectErrorMessage
  , Failure(..)
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import Core.AST (TerminaModule'(..), ModuleImport'(..))

import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Environment (makeInitialGlobalEnv, Environment)
import Semantic.Types (SemanticAnn)

import Configuration.Configuration (defaultConfig, TerminaConfig)
import Configuration.Platform (Platform(TestPlatform))
import Generator.Environment
    (getPlatformInitialGlobalEnv, getPlatformInitialProgram)
import Generator.CodeGen.Module (runGenSourceFile)
import Generator.CodeGen.Application.Glue (runGenMainFile)
import Generator.CodeGen.Application.Initialization (runGenInitFile)
import Generator.LanguageC.Printer (runCPrinter)
import ControlFlow.BasicBlocks.AST (AnnotatedProgram)

import Command.Types
import Command.Utils
    (genBasicBlocks, basicBlockChecks, runCheck, CheckFailure(..),
     getVisibleModules, sortProjectDepsOrLoop)
import Modules.Modules (TerminaModuleData(..), ModuleDependency(..))
import Modules.Utils (buildModuleName)
import Parser.Errors (Error(..), ParsingErrors)
import Utils.Annotations (QualifiedName, annotateError, Location(Internal))

import ControlFlow.Architecture (runGenArchitecture)
import ControlFlow.Architecture.Types (TerminaProgArch)
import ControlFlow.Architecture.Checks
import ControlFlow.ConstFolding (runConstFolding, constFoldModule)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ValueAnalysis (runValueAnalysisCheck)
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
runFullProjectBuild sources = do
  (foldedProject, _, _) <- runProjectPipeline sources
  mapM renderModule foldedProject

-- | Drives the same full pipeline as 'runFullProjectBuild' but stops before
-- per-module source rendering, returning the whole-program architecture and
-- the per-module basic-block programs in dependency order. The application
-- glue (the @main@ and @init@ files) is generated from these two artifacts
-- rather than from a single source module, so a spec that wants to exercise
-- the glue renders it via 'renderMainFile' / 'renderInitFile'.
runFullProjectApp ::
  [(QualifiedName, String)]
  -> Either Failure (TerminaProgArch SemanticAnn, [(QualifiedName, AnnotatedProgram SemanticAnn)])
runFullProjectApp sources = do
  (foldedProject, ordered, progArch) <- runProjectPipeline sources
  let prjprogs = [ (m, basicBlocksAST . metadata $ foldedProject M.! m) | m <- ordered ]
  pure (progArch, prjprogs)

-- | The full pipeline up to (and including) the architecture checks, shared by
-- 'runFullProjectBuild' (which renders each source module) and
-- 'runFullProjectApp' (which renders the application glue). Returns the
-- constant-folded project, the dependency order, and the program architecture.
runProjectPipeline ::
  [(QualifiedName, String)]
  -> Either Failure (BasicBlocksProject, [QualifiedName], TerminaProgArch SemanticAnn)
runProjectPipeline sources = do
  let files = M.fromList [ (qname, pack src) | (qname, src) <- sources ]
  parsedProject <- M.fromList <$> mapM parseModule sources
  ordered <- orderModules parsedProject
  typedProject <- typeProject files parsedProject ordered
  bbProject <- stage files (genBasicBlocks typedProject)
  mapM_ (\check -> noCheckError files (runCheck check TestPlatform bbProject)) basicBlockChecks
  -- | Constant folding runs before architecture so the architecture pass and
  -- the code generator see every type (array sizes) already folded to literals.
  (foldedProject, constEnvs) <- foldProject files bbProject ordered
  -- | The constant propagation check follows the folding, which is what gives
  -- it the constants of each module.
  analyseValues files foldedProject constEnvs
  progArch <- genProjectArchitecture files foldedProject ordered
  runChecks files progArch
  pure (foldedProject, ordered, progArch)

-- | Render the generated @main@ file (task/emitter installation, the app init
-- entry point) from a program architecture, collapsing a codegen failure into
-- the returned 'Text'.
renderMainFile :: TerminaProgArch SemanticAnn -> Either Text Text
renderMainFile progArch =
  case runGenMainFile configParams TestPlatform "main" progArch of
    Left err -> Left . T.pack $ show err
    Right cFile -> Right $ runCPrinter False cFile

-- | Render the generated @init@ file (global object initialization and port
-- wiring) from the per-module basic-block programs, collapsing a codegen
-- failure into the returned 'Text'.
renderInitFile :: [(QualifiedName, AnnotatedProgram SemanticAnn)] -> Either Text Text
renderInitFile prjprogs =
  case runGenInitFile configParams TestPlatform "init" prjprogs of
    Left err -> Left . T.pack $ show err
    Right cFile -> Right $ runCPrinter False cFile

-- | Constant-fold every module in dependency order, threading the constant
-- environment so a module resolves the constants defined by the modules it
-- imports. Mirrors @Command.Common.constFolding@ but stays in 'Either'.
foldProject :: M.Map FilePath Text -> BasicBlocksProject -> [QualifiedName]
  -> Either Failure (BasicBlocksProject, ProjectConstEnvs)
foldProject files bbProject = go (ConstFoldEnv M.empty TestPlatform) M.empty M.empty
  where
    go _ folded constEnvs [] = Right (folded, constEnvs)
    go env folded constEnvs (m:ms) =
      case runConstFolding env (constFoldModule (bbProject M.! m)) of
        Left err -> Left (failure files err)
        Right (foldedModule, env') ->
          go env' (M.insert m foldedModule folded) (M.insert m (constEnv env') constEnvs) ms

-- | Report a condition that always has the same value, which is what the value
-- analysis check looks for. Mirrors @Command.Common.valueAnalysisCheck@ but
-- stays in 'Either'.
analyseValues :: M.Map FilePath Text -> BasicBlocksProject -> ProjectConstEnvs
  -> Either Failure ()
analyseValues files bbProject constEnvs =
  case mapMaybe checkModule (M.toList bbProject) of
    [] -> Right ()
    (err : _) -> Left (failure files err)

  where

    checkModule (m, bbModule) = runValueAnalysisCheck TestPlatform
      (M.findWithDefault M.empty m constEnvs)
      (basicBlocksAST . metadata $ bbModule)

-- | The error code (@errorIdent@: \"SE-042\", \"BE-001\", \"AE-007\",
-- \"CF-…\") raised by the first failing pipeline stage for a single-module
-- program named @test@, or 'Nothing' if it compiles cleanly. This is the
-- single assertion point for every negative test, whatever stage the error
-- belongs to.
compileErrorCode :: String -> Maybe Text
compileErrorCode input = compileProjectErrorCode [("test", input)]

-- | Multi-module variant of 'compileErrorCode'.
compileProjectErrorCode :: [(QualifiedName, String)] -> Maybe Text
compileProjectErrorCode =
  either (Just . failCode) (const Nothing) . runFullProjectBuild

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
      pure (qname, TerminaModuleData qname qname dummyTime deps [] (pack src) (ParsingData prog))

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

typeProject :: M.Map FilePath Text -> ParsedProject -> [QualifiedName]
  -> Either Failure TypedProject
typeProject files parsedProject = go M.empty initialEnv

  where

    go typed _ [] = Right typed
    go typed prevState (m:ms) =
      let parsedModule = parsedProject M.! m
          prevModsMap = M.map visibleModules typed
          vmods = S.fromList $ getVisibleModules prevModsMap (importedModules parsedModule)
      in case runTypeChecking prevState
                (typeTerminaModule (S.insert m vmods) (parsedAST . metadata $ parsedModule)) of
           Left err -> Left (failure files err)
           Right (typedProgram, newState) ->
             let typedModule = TerminaModuleData m m dummyTime
                   (importedModules parsedModule) (S.toList vmods)
                   (sourcecode parsedModule) (SemanticData typedProgram)
             in go (M.insert m typedModule typed) newState ms

genProjectArchitecture :: M.Map FilePath Text -> BasicBlocksProject -> [QualifiedName]
  -> Either Failure (TerminaProgArch SemanticAnn)
genProjectArchitecture files bbProject = go initialProg

  where

    go tp [] = Right tp
    go tp (m:ms) =
      case runGenArchitecture tp m (basicBlocksAST . metadata $ bbProject M.! m) of
        Left err -> Left (failure files err)
        Right tp' -> go tp' ms

runChecks :: M.Map FilePath Text -> TerminaProgArch SemanticAnn -> Either Failure ()
runChecks files progArch =
  stage files $
    sequence_ [ runCheckEmitterConnections progArch
              , runCheckChannelConnections progArch
              , runCheckResourceUsage progArch
              , runCheckPoolUsage progArch
              , runCheckBoxSources progArch ]

renderModule :: BasicBlocksModule -> Either Failure Text
renderModule bbModule =
  case runGenSourceFile configParams TestPlatform (qualifiedName bbModule)
         (basicBlocksAST . metadata $ bbModule) of
    Left err -> Left (Failure (T.pack (show err)) (T.pack (show err)))
    Right cSourceFile -> Right $ runCPrinter False cSourceFile

-- Shared configuration ------------------------------------------------------

configParams :: TerminaConfig
configParams = defaultConfig "test" TestPlatform

initialEnv :: Environment
initialEnv = makeInitialGlobalEnv (Just configParams) TestPlatform
               (getPlatformInitialGlobalEnv configParams TestPlatform)

initialProg :: TerminaProgArch SemanticAnn
initialProg = getPlatformInitialProgram configParams TestPlatform

-- | A placeholder modification time: only the IO file-caching logic in
-- Command.Build reads it; the pure pipeline never inspects it.
dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 1997 8 29) (secondsToDiffTime (2 * 3600 + 14 * 60))

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

-- | Collapse a stage that yields @Either error@ into the error's code on the
-- @Left@.
stage :: (ErrorMessage e, Show e) => M.Map FilePath Text -> Either e a -> Either Failure a
stage files = either (Left . failure files) Right

-- | The same, for a check of the basic-block AST, which reports the answers its
-- callers need instead of the error itself.
noCheckError :: M.Map FilePath Text -> Maybe CheckFailure -> Either Failure ()
noCheckError files =
  maybe (Right ()) (\f -> Left (Failure (code f) (failureMessage f files)))

  where

    code :: CheckFailure -> Text
    code checkFailure =
      if failureCode checkFailure == pack "Internal"
        then failureCode checkFailure <> pack ": "
               <> pack (take 240 (failureShown checkFailure))
        else failureCode checkFailure
