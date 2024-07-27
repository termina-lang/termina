module DataFlow.Architecture.Checks where
import qualified Data.Map as M
import AST.Seman
import Semantic.Monad
import DataFlow.Architecture.Types
import DataFlow.Architecture.Utils

getDisconnectedEmitters :: TerminaProgArch SemanticAnn -> [Identifier]
getDisconnectedEmitters tp = M.keys . M.filter ((`M.notMember` emitterTargets tp) . getEmmiterIdentifier) . emitters $ tp