module ControlFlow.Architecture.Checks (
    runCheckPoolUsage,
    runCheckDequeConnections,
    runCheckResourceUsage,
    runCheckChannelConnections,
    runCheckEmitterConnections,
    runCheckBoxSources,
    runCheckTaskPriorities,
    runCheckSystemExceptAction,
    runCheckActivationCycles,
    runCheckTestDeployment
) where
import Semantic.Types
import ControlFlow.Architecture.Types
import Control.Monad.Reader
import Control.Monad.Except
import ControlFlow.Architecture.Errors
import ControlFlow.Architecture.Checks.BoxSources
import ControlFlow.Architecture.Checks.Connections
import ControlFlow.Architecture.Checks.Tests

runCheckPoolUsage :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckPoolUsage = runReader (runExceptT checkPoolUsage)

runCheckDequeConnections ::
    TerminaProgArch SemanticAnn
    -> Either ArchitectureError ()
runCheckDequeConnections = runReader (runExceptT checkDequeConnections)

runCheckResourceUsage :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckResourceUsage = runReader (runExceptT checkResourceUsage)

runCheckChannelConnections ::
    TerminaProgArch SemanticAnn
    -> Either ArchitectureError ()
runCheckChannelConnections = runReader (runExceptT checkChannelConnections)

runCheckEmitterConnections :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckEmitterConnections = runReader (runExceptT checkEmitterConnections)

runCheckBoxSources :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckBoxSources = runReader (runExceptT checkBoxSources)

runCheckTaskPriorities ::
    TerminaProgArch SemanticAnn
    -> Either ArchitectureError ()
runCheckTaskPriorities = runReader (runExceptT checkTaskPriorities)

runCheckSystemExceptAction ::
    TerminaProgArch SemanticAnn
    -> Either ArchitectureError ()
runCheckSystemExceptAction = runReader (runExceptT checkSystemExceptAction)

runCheckActivationCycles ::
    TerminaProgArch SemanticAnn
    -> Either ArchitectureError ()
runCheckActivationCycles = runReader (runExceptT checkActivationCycles)

runCheckTestDeployment ::
    TerminaProgArch SemanticAnn
    -> Either ArchitectureError ()
runCheckTestDeployment = runReader (runExceptT checkTestDeployment)
