module ControlFlow.ConstFolding.Monad where
import qualified Data.Map as M
import Semantic.AST
import Semantic.Types
import ControlFlow.Architecture.Types
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST

data TransFoldSt = TransFoldSt
  {
    localConstEnv :: M.Map Identifier (Const SemanticAnn),
    globalConstEnv :: M.Map Identifier (Const SemanticAnn),
    currentElement :: Maybe Identifier,
    progArch :: TerminaProgArch SemanticAnn
  }

type TransFoldMonad = ExceptT ConstFoldError (ST.State TransFoldSt)

type ConstSimplMonad = Except ConstFoldError

localInputScope :: TransFoldMonad a -> TransFoldMonad a
localInputScope comp = do
  prevst <- ST.get
  res <- comp
  currst <- ST.get
  ST.put (currst { localConstEnv = localConstEnv prevst })
  return res

switchInputScope :: Identifier -> TransFoldMonad a -> TransFoldMonad a
switchInputScope target comp = do
  prevst <- ST.get
  ST.put (prevst { currentElement = Just target })
  res <- comp
  ST.put prevst
  return res
