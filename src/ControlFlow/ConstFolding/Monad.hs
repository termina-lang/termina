module ControlFlow.ConstFolding.Monad where
import qualified Data.Map.Strict as M
import Semantic.AST
import Semantic.Types
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST

newtype ConstFoldEnv = ConstFoldEnv
  {
    constEnv :: M.Map Identifier (Const SemanticAnn)
  }

type ConstFoldMonad = ExceptT ConstFoldError (ST.State ConstFoldEnv)
