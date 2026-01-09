module EFP.Schedulability.MAST.Monad where
import ControlFlow.Architecture.Types
import Semantic.Types
import Configuration.Configuration
import Control.Monad.Except
import EFP.Schedulability.MAST.Errors
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.RT.Semantic.Types
import qualified Data.Map.Strict as M
import EFP.Schedulability.MAST.AST
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Semantic.AST

type MASTProcessingResourceMap = M.Map Identifier MASTProcessingResource
type MASTOperationMap = M.Map Identifier MASTOperation
type MASTSchedulingServerMap = M.Map Identifier MASTSchedulingServer
type MASTSharedResourceMap = M.Map Identifier MASTSharedResource
type MASTTransactionMap = M.Map Identifier MASTTransaction

data MASTGenEnv = MASTGenEnv
    {
        progArch :: TerminaProgArch SemanticAnn
        , configParams :: TerminaConfig
        , resourceLockingMap :: ResourceLockingMap
        , stepMap :: TRPStepMap RTSemAnn
        , deadlinesMap :: RTDeadlineMap RTSemAnn
        , operations :: MASTOperationMap
        , schedulingServers :: MASTSchedulingServerMap
        , sharedResources :: MASTSharedResourceMap
        , transactions :: MASTTransactionMap
    }

type MASTGenMonad = ExceptT MASTGenErrors (ST.State MASTGenEnv)