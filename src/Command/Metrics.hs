-- | The command that measures the source of a project: lines, size,
-- cyclomatic complexity and coupling of its modules and units. It prints a
-- table and writes the CSV of the units and the JSON with every figure into
-- @output/metrics/@.
module Command.Metrics (
    metricsCmdArgsParser, metricsCommand, MetricsCmdArgs
) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import qualified Options.Applicative as O
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), makeRelative)

import Command.Build (CheckedDeployment(..), checkDeployment)
import Command.Provenance (terminaVersion)
import Command.Common (libraryPlatforms, loadExportedModules, sourceTexts, typeLibrary)
import Command.Dependencies (Fetching(..))
import Command.Pipeline (CheckedModules(..))
import Command.Project (Dependencies(..), loadProject)
import Command.Sources (LoadedProject(..), buildSources, dependencyModulesOf)
import Command.Types
import Command.Utils (checkFailure, dieWithError, infoMessage, lowerProject, orDieWithFailure, projectSourceFiles)
import Configuration.Manifest (ProjectInfo(..), Manifest(..), appDeployment)
import ControlFlow.Architecture.Types (TerminaProgArch)
import Metrics.Coupling
import Metrics.Html
import Metrics.Report
import Utils.HtmlReport (Provenance(..), markFile, script, scriptFile, styleSheet, terminaMark)
import Modules.Modules (ModuleDependency(..), TerminaModuleData(..))
import Modules.Utils (qualifiedToModuleName)
import Semantic.Types (SemanticAnn)
import Utils.Annotations (QualifiedName)

-- | The options of the command: whether the modules of the dependencies are
-- measured too.
newtype MetricsCmdArgs = MetricsCmdArgs Bool
    deriving (Show, Eq)

metricsCmdArgsParser :: O.Parser MetricsCmdArgs
metricsCmdArgsParser = MetricsCmdArgs
    <$> O.switch (O.long "include-deps"
        <> O.help "Measure also the modules of the dependencies, in vendor/ and in local folders")

-- | The modules of a project checked as far as the metrics need: typed, in
-- basic blocks and, for an application, with its architecture.
data Measured = Measured
    { measuredTyped :: TypedProject
    , measuredLowered :: LoweredProject
    , measuredArch :: Maybe (TerminaProgArch SemanticAnn)
    , measuredDeps :: S.Set QualifiedName
    }

-- | Check an application up to its architecture, or type a library and lower
-- its modules.
checkProject :: LoadedProject -> IO Measured
checkProject project =
    case appDeployment manifest of
        Just deployment -> do
            checked <- checkDeployment False project deployment
            return (Measured (checkedTyped checked) (checkedLowered (checkedResult checked))
                (Just (checkedArchitecture (checkedResult checked))) (dependencyModulesOf (checkedSources checked)))
        Nothing -> do
            let sources = buildSources project Nothing "src"
                deps = dependencyModulesOf sources
            (parsed, order) <- loadExportedModules project >>= orDieWithFailure (sourceTexts sources)
            plt <- case libraryPlatforms manifest of
                plt : _ -> return plt
                [] -> dieWithError "the library has no platform to check its modules on"
            let (typed, failure) = typeLibrary manifest plt deps parsed order
            orDieWithFailure (projectSourceFiles parsed) (maybe (Right ()) Left failure)
            lowered <- orDieWithFailure (projectSourceFiles parsed) (either (Left . checkFailure) Right (lowerProject typed))
            return (Measured typed lowered Nothing deps)

  where

    manifest = loadedManifest project

metricsCommand :: MetricsCmdArgs -> IO ()
metricsCommand (MetricsCmdArgs includeDeps) = do
    project <- loadProject (Update Offline)
    measured <- checkProject project
    let root = loadedRoot project
        typed = measuredTyped measured
        selected = M.filterWithKey (\q _ -> includeDeps || not (S.member q (measuredDeps measured))) typed
        lowered = [ loweredAST (metadata m) | (q, m) <- M.toList (measuredLowered measured), M.member q selected ]
        calls = unitCalls (measuredArch measured) lowered
        callers = unitCallers calls
        imports = moduleImports (M.map (\m -> [ qualifiedToModuleName q | ModuleDependency q _ <- importedModules m ]) (M.mapKeys qualifiedToModuleName selected))
        modulesMetrics =
            [ moduleMetrics name (makeRelative root (fullPath m)) (T.unpack (sourcecode m)) (typedAST (metadata m))
                calls callers (M.findWithDefault ([], []) name imports)
            | m <- M.elems selected, let name = qualifiedToModuleName (qualifiedName m) ]
        report = ProjectMetrics
            (M.elems (M.fromList [ (mmName mm, mm) | mm <- modulesMetrics ]))
            (maybe [] (`instancesOf` calls) (measuredArch measured))
            (maybe noTotals archTotals (measuredArch measured))
        out = root </> "output" </> "metrics"
    when (null modulesMetrics) (dieWithError "the project has no modules to measure")
    putStr (textReport report)
    createDirectoryIfMissing True out
    writeFile (out </> "units.csv") (unitsCsv report)
    BL.writeFile (out </> "metrics.json") (metricsJson report)
    written <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M" <$> getZonedTime
    let info = manifestProject (loadedManifest project)
        provenance = Provenance (T.unpack (projectName info) ++ " " ++ T.unpack (projectVersion info)) terminaVersion written
    BS.writeFile (out </> markFile) terminaMark
    BS.writeFile (out </> styleFile) styleSheet
    BS.writeFile (out </> scriptFile) script
    writeFile (out </> "index.html") (reportPage provenance report)
    putStrLn (infoMessage ("The metrics report is " ++ ("output" </> "metrics" </> "index.html")))

  where

    noTotals = ArchTotals 0 0 0 0 0 0 0 0 0
