module ControlFlow.ConstFolding.Monad where
import qualified Data.Map.Strict as M
import Semantic.AST
import Semantic.Types
import Configuration.Platform (Platform)
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST

data ConstFoldEnv = ConstFoldEnv
  {
    constEnv :: M.Map Identifier (Const SemanticAnn)
  , targetPlatform :: Platform
    -- | Length of the tick in microseconds, which the period of a periodic
    -- timer has to be a multiple of.
  , tickMicroseconds :: Integer
  }

type ConstFoldMonad = ExceptT ConstFoldError (ST.State ConstFoldEnv)
