module ControlFlow.Architecture.Checks (
    getDisconnectedEmitters,
    getChannelsWithoutInputs,
    getChannelsWithoutTargets,
    getUnusedResources,
    getUnusedPools,
    runCheckBoxSources
) where
import qualified Data.Map as M
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import Control.Monad.Reader
import Control.Monad.Except
import ControlFlow.Architecture.Errors.Errors
import ControlFlow.Architecture.Checks.BoxSources

getDisconnectedEmitters :: TerminaProgArch SemanticAnn -> [Identifier]
getDisconnectedEmitters tp = M.keys . M.filter ((`M.notMember` emitterTargets tp) . getEmmiterIdentifier) . emitters $ tp

getChannelsWithoutInputs :: TerminaProgArch SemanticAnn -> [Identifier]
getChannelsWithoutInputs tp = M.keys . M.filter ((`M.notMember` channelSources tp) . (\(TPMsgQueue ident _ _ _ _) -> ident )) . channels $ tp

getChannelsWithoutTargets :: TerminaProgArch SemanticAnn -> [Identifier]
getChannelsWithoutTargets tp = M.keys . M.filter ((`M.notMember` channelTargets tp) . (\(TPMsgQueue ident _ _ _ _) -> ident )) . channels $ tp

getUnusedResources :: TerminaProgArch SemanticAnn -> [Identifier]
getUnusedResources tp = M.keys . M.filter ((`M.notMember` resourceSources tp) . resourceName) . resources $ tp

getUnusedPools :: TerminaProgArch SemanticAnn -> [Identifier]
getUnusedPools tp = M.keys . M.filter ((`M.notMember` resourceSources tp) . (\(TPPool ident _ _ _ _) -> ident)) . pools $ tp

runCheckBoxSources :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckBoxSources = runReader (runExceptT checkBoxSources)