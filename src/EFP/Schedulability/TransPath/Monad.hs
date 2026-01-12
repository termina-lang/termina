module EFP.Schedulability.TransPath.Monad where
import ControlFlow.Architecture.Types
import Semantic.Types
import EFP.Schedulability.WCEPath.Types
import Control.Monad.Except
import EFP.Schedulability.TransPath.Errors
import qualified Data.Map.Strict as M
import EFP.Schedulability.TransPath.AST
import qualified Control.Monad.State as ST
import EFP.Schedulability.TransPath.Types
import Configuration.Configuration
import EFP.Schedulability.WCET.Types

type TRPGenEnvironment = M.Map Identifier (ConstExpression TRPSemAnn)

data TRPGenState = TRPGenState
    {
        progArch :: TerminaProgArch SemanticAnn
        , configParams :: TerminaConfig
        , resourceLockingMap :: ResourceLockingMap
        , transPaths :: WCEPathMap WCEPSemAnn
        , transWCETs :: WCETimesMap WCETSemAnn
        , localConstEnv :: TRPGenEnvironment
        , operationMap :: TRPOperationMap TRPSemAnn
    } deriving Show

type TRPGenMonad = ExceptT TRPGenErrors (ST.State TRPGenState) 

localInputScope :: TRPGenMonad a -> TRPGenMonad a
localInputScope comp = do
  prevst <- ST.get
  res <- comp
  currst <- ST.get
  ST.put (currst { localConstEnv = localConstEnv prevst })
  return res

newActivityMap :: TRPGenMonad ()
newActivityMap = do
    ST.modify (\s -> s { operationMap = M.empty })