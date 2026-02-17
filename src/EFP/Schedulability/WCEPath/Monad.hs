module EFP.Schedulability.WCEPath.Monad where
    
import qualified Data.Map.Strict as M
import EFP.Schedulability.WCEPath.AST
import Utils.Annotations
import ControlFlow.Architecture.Types
import Semantic.Types
import qualified Data.Set as S
import EFP.Schedulability.WCEPath.Types
import Control.Monad.Except
import EFP.Schedulability.WCEPath.Errors
import qualified Control.Monad.State as ST

type TPGlobalConstsEnv = M.Map Identifier Location


data TransPathState = TransPathState
    {
        progArch :: TerminaProgArch SemanticAnn
        , globalConsts :: TPGlobalConstsEnv
        , localConsts :: S.Set Identifier
        , transPaths :: WCEPathMap WCEPSemAnn
    } deriving Show

type TransPathMonad = ExceptT WCEPathErrors (ST.State TransPathState)