module Pipeline.Common
  ( runFullBuild
  , runFullProjectBuild
  , buildAndRenderModule
  , compileErrorCode
  , compileProjectErrorCode
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
    (getPlatformInterruptMap, getPlatformInitialGlobalEnv, getPlatformInitialProgram)
import Generator.CodeGen.Module (runGenSourceFile)
import Generator.LanguageC.Printer (runCPrinter)

import Command.Types
import Command.Utils
    (genBasicBlocks, basicBlockPathsCheckModules, useDefCheckModules,
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
import Utils.Errors (ErrorMessage(errorIdent))

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
runFullProjectBuild :: [(QualifiedName, String)] -> Either Text (M.Map QualifiedName Text)
runFullProjectBuild sources = do
  parsedProject <- M.fromList <$> mapM parseModule sources
  ordered <- orderModules parsedProject
  typedProject <- typeProject parsedProject ordered
  bbProject <- stage $ genBasicBlocks typedProject
  noError $ basicBlockPathsCheckModules bbProject
  noError $ useDefCheckModules bbProject
  -- | Constant folding runs before architecture so the architecture pass and
  -- the code generator see every type (array sizes) already folded to literals.
  foldedProject <- foldProject bbProject ordered
  progArch <- genProjectArchitecture foldedProject ordered
  runChecks progArch
  mapM renderModule foldedProject

-- | Constant-fold every module in dependency order, threading the constant
-- environment so a module resolves the constants defined by the modules it
-- imports. Mirrors @Command.Common.constFolding@ but stays in 'Either'.
foldProject :: BasicBlocksProject -> [QualifiedName] -> Either Text BasicBlocksProject
foldProject bbProject = go (ConstFoldEnv M.empty) M.empty
  where
    go _ folded [] = Right folded
    go env folded (m:ms) =
      case runConstFolding env (constFoldModule (bbProject M.! m)) of
        Left err -> Left (errCode err)
        Right (foldedModule, env') -> go env' (M.insert m foldedModule folded) ms

-- | The error code (@errorIdent@: \"SE-042\", \"VE-003\", \"AE-007\",
-- \"CF-…\") raised by the first failing pipeline stage for a single-module
-- program named @test@, or 'Nothing' if it compiles cleanly. This is the
-- single assertion point for every negative test, whatever stage the error
-- belongs to.
compileErrorCode :: String -> Maybe Text
compileErrorCode input = compileProjectErrorCode [("test", input)]

-- | Multi-module variant of 'compileErrorCode'.
compileProjectErrorCode :: [(QualifiedName, String)] -> Maybe Text
compileProjectErrorCode = either Just (const Nothing) . runFullProjectBuild

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
    Left err -> err
    Right rendered ->
      M.findWithDefault
        (pack $ "Module not found in project: " ++ target)
        target rendered

-- Pipeline stages -----------------------------------------------------------

parseModule :: (QualifiedName, String) -> Either Text (QualifiedName, ParsedModule)
parseModule (qname, src) =
  case runParser terminaModuleParser qname "" src of
    Left err -> Left (errCode (annotateError Internal (EParseError err) :: ParsingErrors))
    Right (Termina imports prog) -> do
      deps <- mapM toDep imports
      pure (qname, TerminaModuleData qname qname dummyTime deps [] (pack src) (ParsingData prog))

  where

    toDep (ModuleImport ident ann) =
      case buildModuleName ann ident of
        Left perr -> Left . pack $ "Import error in " ++ qname ++ ": " ++ show perr
        Right dep -> Right (ModuleDependency dep ann)

orderModules :: ParsedProject -> Either Text [QualifiedName]
orderModules parsedProject =
  case sortProjectDepsOrLoop (M.map importedModules parsedProject) of
    Left loop -> Left (errCode (annotateError Internal (EImportedFilesLoop loop) :: ParsingErrors))
    Right ordered -> Right ordered

typeProject :: ParsedProject -> [QualifiedName] -> Either Text TypedProject
typeProject parsedProject = go M.empty initialEnv

  where

    go typed _ [] = Right typed
    go typed prevState (m:ms) =
      let parsedModule = parsedProject M.! m
          prevModsMap = M.map visibleModules typed
          vmods = S.fromList $ getVisibleModules prevModsMap (importedModules parsedModule)
      in case runTypeChecking prevState
                (typeTerminaModule (S.insert m vmods) (parsedAST . metadata $ parsedModule)) of
           Left err -> Left (errCode err)
           Right (typedProgram, newState) ->
             let typedModule = TerminaModuleData m m dummyTime
                   (importedModules parsedModule) (S.toList vmods)
                   (sourcecode parsedModule) (SemanticData typedProgram)
             in go (M.insert m typedModule typed) newState ms

genProjectArchitecture :: BasicBlocksProject -> [QualifiedName] -> Either Text (TerminaProgArch SemanticAnn)
genProjectArchitecture bbProject = go initialProg

  where

    go tp [] = Right tp
    go tp (m:ms) =
      case runGenArchitecture tp m (basicBlocksAST . metadata $ bbProject M.! m) of
        Left err -> Left (errCode err)
        Right tp' -> go tp' ms

runChecks :: TerminaProgArch SemanticAnn -> Either Text ()
runChecks progArch =
  stage $
    sequence_ [ runCheckEmitterConnections progArch
              , runCheckChannelConnections progArch
              , runCheckResourceUsage progArch
              , runCheckPoolUsage progArch
              , runCheckBoxSources progArch ]

renderModule :: BasicBlocksModule -> Either Text Text
renderModule bbModule =
  case runGenSourceFile configParams irqMap (qualifiedName bbModule)
         (basicBlocksAST . metadata $ bbModule) of
    Left err -> Left . T.pack $ show err
    Right cSourceFile -> Right $ runCPrinter False cSourceFile

-- Shared configuration ------------------------------------------------------

configParams :: TerminaConfig
configParams = defaultConfig "test" TestPlatform

irqMap :: M.Map QualifiedName Integer
irqMap = getPlatformInterruptMap TestPlatform

initialEnv :: Environment
initialEnv = makeInitialGlobalEnv (Just configParams)
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
errCode :: (ErrorMessage e, Show e) => e -> Text
errCode err =
  let code = errorIdent err in
  if code == pack "Internal"
    then code <> pack ": " <> pack (take 240 (show err))
    else code

-- | Collapse a stage that yields @Either error@ into the error's code on the
-- @Left@.
stage :: (ErrorMessage e, Show e) => Either e a -> Either Text a
stage = either (Left . errCode) Right

-- | Collapse a stage that yields @Maybe error@ (a pass that either finds a
-- problem or does not) into the error's code on the @Left@.
noError :: (ErrorMessage e, Show e) => Maybe e -> Either Text ()
noError = maybe (Right ()) (Left . errCode)
