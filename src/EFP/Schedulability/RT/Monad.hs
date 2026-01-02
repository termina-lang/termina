module EFP.Schedulability.RT.Monad where
import qualified Data.Map as M
import EFP.Schedulability.RT.AST
import Utils.Annotations
import ControlFlow.Architecture.Types
import Semantic.Types
import EFP.Schedulability.WCEPath.Types
import EFP.Schedulability.RT.Types
import Control.Monad.Except
import EFP.Schedulability.RT.Errors
import qualified Control.Monad.State as ST



type TPGlobalConstsEnv = M.Map Identifier Location


data RTState = RTState
    {
        progArch :: TerminaProgArch SemanticAnn
        , transPaths :: WCEPathMap WCEPSemAnn
        , currentSteps :: M.Map Identifier Location
        , transactions :: RTTransactionMap RTSemAnn
        , situations :: RTSituationMap RTSemAnn
    } deriving Show

type RTMonad = ExceptT RTErrors (ST.State RTState)

type RTFlatMonad = Except RTErrors