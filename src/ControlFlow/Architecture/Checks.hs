module ControlFlow.Architecture.Checks (
    runCheckPoolUsage,
    runCheckResourceUsage,
    runCheckChannelConnections,
    runCheckEmitterConnections,
    runCheckBoxSources
) where
import Semantic.Types
import ControlFlow.Architecture.Types
import Control.Monad.Reader
import Control.Monad.Except
import ControlFlow.Architecture.Errors.Errors
import ControlFlow.Architecture.Checks.BoxSources
import ControlFlow.Architecture.Checks.Connections

runCheckPoolUsage :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckPoolUsage = runReader (runExceptT checkPoolUsage)

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