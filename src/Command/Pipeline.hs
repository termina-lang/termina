-- | The stages that check the modules of a deployment, from the type checker
-- to the architecture. Each stage gives the first error it finds.
module Command.Pipeline (
    CheckedModules(..), checkModules,
    typeProject, foldProject, analyseValues
) where

import Command.Types
import Command.Utils
import Configuration.Configuration (TerminaConfig(..))
import Configuration.Platform (Platform)
import ControlFlow.Architecture (runGenArchitecture, pruneUnconnectedSystemElements)
import ControlFlow.Architecture.Checks (runArchitectureChecks)
import ControlFlow.Architecture.Types (TerminaProgArch)
import ControlFlow.ConstFolding (runConstFolding, constFoldModule)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ValueAnalysis (runValueAnalysisCheck)
import Control.Applicative ((<|>))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Elaboration (ElaborationReport)
import Generator.Environment (getPlatformInitialGlobalEnv, getPlatformInitialProgram)
import Generator.Monadic (MonadicTypes, emptyMonadicTypes, runMapMonadicTypesAnnotatedProgram)
import Modules.Modules
import Semantic.Environment (Environment(..), addDeclaredNames, makeInitialGlobalEnv)
import Semantic.TypeChecking (runTypeChecking, typeTerminaModule)
import Semantic.Types (SemanticAnn)
import Utils.Annotations (QualifiedName)

-- | The modules of a deployment checked up to its architecture.
data CheckedModules = CheckedModules {
    checkedMonadicTypes :: !MonadicTypes,
    -- | The basic-block AST with the constants folded
    checkedLowered :: !LoweredProject,
    checkedElaborated :: !ElaboratedProject,
    -- | The run-time checks that each module keeps
    checkedReports :: !(M.Map QualifiedName ElaborationReport),
    checkedArchitecture :: !(TerminaProgArch SemanticAnn)
  }

-- | Check the modules of a deployment, given in the order in which they are
-- type checked. It gives the modules type checked, all of them or those
-- before the first type error, and the result of the checks.
checkModules :: TerminaConfig -> Platform -> S.Set QualifiedName -> ParsedProject -> [QualifiedName]
  -> (TypedProject, Either CheckFailure CheckedModules)
checkModules config plt deps parsedProject orderedModules =
    (typedProject, maybe (checkTyped typedProject) Left typeFailure)

  where

    initialEnv = makeInitialGlobalEnv (Just config) plt (getPlatformInitialGlobalEnv config plt)

    (typedProject, typeFailure) = typeProject deps initialEnv parsedProject orderedModules

    checkTyped typed = do
        rawLoweredProject <- lowerModules typed
        maybe (Right ()) Left (loweredChecks rawLoweredProject)
        (loweredProject, constEnvs) <- foldProject config plt rawLoweredProject orderedModules
        valueEvidence <- analyseValues plt constEnvs loweredProject orderedModules
        let (elaboratedProject, checksReports) = elaborateProject valueEvidence loweredProject
        sideEffectCheck plt elaboratedProject
        programArchitecture <- genProjectArchitecture (getPlatformInitialProgram config plt) loweredProject orderedModules
        architectureCheck orderedModules programArchitecture
        return (CheckedModules (monadicTypesOf typed) loweredProject elaboratedProject checksReports programArchitecture)

-- | Obtain the basic-block AST of the modules.
lowerModules :: TypedProject -> Either CheckFailure LoweredProject
lowerModules = either (Left . checkFailure) Right . lowerProject

-- | Check the paths, the definite assignment, the boxes, the deques and the
-- scope of the variables of the basic-block AST, in this order.
loweredChecks :: LoweredProject -> Maybe CheckFailure
loweredChecks project =
    (checkFailure <$> basicBlockPathsCheckModules project)
    <|> (checkFailure <$> varUsageCheckModules project)
    <|> (checkFailure <$> boxUsageCheckModules project)
    <|> (checkFailure <$> dequeUsageCheckModules project)
    <|> (checkFailure <$> varScopeCheckModules project)

-- | Check the side effects of the elaborated modules.
sideEffectCheck :: Platform -> ElaboratedProject -> Either CheckFailure ()
sideEffectCheck plt = maybe (Right ()) (Left . checkFailure) . sideEffectCheckModules plt

-- | Check the architecture of the program.
architectureCheck :: [QualifiedName] -> TerminaProgArch SemanticAnn -> Either CheckFailure ()
architectureCheck orderedModules = either (Left . checkFailure) Right . runArchitectureChecks orderedModules

-- | Type check the modules of a project in order, starting from an
-- environment. It gives the modules type checked up to the first error, and
-- the error.
typeProject :: S.Set QualifiedName -> Environment -> ParsedProject -> [QualifiedName] -> (TypedProject, Maybe CheckFailure)
typeProject deps initialEnv parsedProject order =
    typeModules M.empty
      ((addDeclaredNames (projectDeclaredNames parsedProject) initialEnv) { dependencyModules = deps })
      (filter fromDependency order ++ filter (not . fromDependency) order)

  where

    -- | A module of a dependency never imports one of the project, so the
    -- modules of the dependencies can be typed first, and a name that the
    -- project repeats is reported in the project.
    fromDependency m = S.member m deps

    typeModules typedProject _ [] = (typedProject, Nothing)
    typeModules typedProject env (m:ms) =
      let parsedModule = parsedProject M.! m
          vmods = S.fromList $ getVisibleModules (M.map visibleModules typedProject) (importedModules parsedModule)
      in case runTypeChecking env (typeTerminaModule (S.insert m vmods) . parsedAST . metadata $ parsedModule) of
          Left err -> (typedProject, Just (checkFailure err))
          Right (typedProgram, env') ->
            let typedModule = parsedModule { visibleModules = S.toList vmods, metadata = SemanticData typedProgram }
            in typeModules (M.insert m typedModule typedProject) env' ms

-- | Fold the constants of the modules in order. The constants of a module are
-- passed on to the modules after it.
foldProject :: TerminaConfig -> Platform -> LoweredProject -> [QualifiedName]
  -> Either CheckFailure (LoweredProject, ProjectConstEnvs)
foldProject config plt loweredProject =
    foldModules (ConstFoldEnv M.empty plt (microsecondsPerTick config)) M.empty M.empty

  where

    foldModules _ foldedProject constEnvs [] = Right (foldedProject, constEnvs)
    foldModules env foldedProject constEnvs (m:ms) = do
      (foldedModule, env') <- either (Left . checkFailure) Right (runConstFolding env (constFoldModule (loweredProject M.! m)))
      foldModules env' (M.insert m foldedModule foldedProject) (M.insert m (constEnv env') constEnvs) ms

-- | Check that no condition of the modules has the same value every time it is
-- evaluated (VAE-001), and give the run-time checks that the values of their
-- operands show to hold. The modules go in order, and what each function
-- returns is passed on to the modules after it.
analyseValues :: Platform -> ProjectConstEnvs -> LoweredProject -> [QualifiedName]
  -> Either CheckFailure ProjectValueEvidence
analyseValues plt constEnvs loweredProject = checkModules' M.empty M.empty

  where

    checkModules' _ evidence [] = Right evidence
    checkModules' returned evidence (m:ms) =
      case runValueAnalysisCheck plt
             (M.findWithDefault M.empty m constEnvs)
             returned
             (loweredAST . metadata $ loweredProject M.! m) of
        (Just err, _, _) -> Left (checkFailure err)
        (Nothing, returned', proven) -> checkModules' returned' (M.insert m proven evidence) ms

-- | The architecture of the program, built from the modules in order.
genProjectArchitecture :: TerminaProgArch SemanticAnn -> LoweredProject -> [QualifiedName]
  -> Either CheckFailure (TerminaProgArch SemanticAnn)
genProjectArchitecture initialProgram loweredProject = genModules initialProgram

  where

    genModules tp [] = Right (pruneUnconnectedSystemElements tp)
    genModules tp (m:ms) = do
      tp' <- either (Left . checkFailure) Right (runGenArchitecture tp m (loweredAST . metadata $ loweredProject M.! m))
      genModules tp' ms

-- | The option, status and result types that the modules use.
monadicTypesOf :: TypedProject -> MonadicTypes
monadicTypesOf = foldl' addModule emptyMonadicTypes . M.elems

  where

    addModule prevMap typedModule = runMapMonadicTypesAnnotatedProgram prevMap (typedAST . metadata $ typedModule)
