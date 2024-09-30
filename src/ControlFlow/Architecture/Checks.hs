module ControlFlow.Architecture.Checks where
import qualified Data.Map as M
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils

getDisconnectedEmitters :: TerminaProgArch SemanticAnn -> [Identifier]
getDisconnectedEmitters tp = M.keys . M.filter ((`M.notMember` emitterTargets tp) . getEmmiterIdentifier) . emitters $ tp