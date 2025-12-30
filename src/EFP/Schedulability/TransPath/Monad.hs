module EFP.Schedulability.TransPath.Monad where
import ControlFlow.Architecture.Types
import Semantic.Types
import EFP.Schedulability.WCEPath.Types
import Control.Monad.Reader
import Control.Monad.Except
import EFP.Schedulability.TransPath.Errors

data TRPGenInput = TRPGenInput
    {
        progArch :: TerminaProgArch SemanticAnn
        , transPaths :: WCEPathMap TRPSemAnn
    } deriving Show

type TRPGenMonad = ExceptT TRPGenErrors (Reader TRPGenInput) 
