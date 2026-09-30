module ControlFlow.ConstFolding.Monad where
import qualified Data.Map.Strict as M
import Semantic.AST
import Semantic.Types
import Configuration.Platform (Platform)
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST

-- | What is known of a name: the value of an integer or a boolean, which the
-- evaluator computes with, or the folded initializer of any other constant,
-- which the initializer of another constant copies in place of a reference to
-- it.
data ConstEntry =
  ConstValue (Const SemanticAnn)
  | ConstInitializer (Expression SemanticAnn)

-- | The value of a name that holds one.
constValueOf :: ConstEntry -> Maybe (Const SemanticAnn)
constValueOf (ConstValue value) = Just value
constValueOf (ConstInitializer _) = Nothing

data ConstFoldEnv = ConstFoldEnv
  {
    constEnv :: M.Map Identifier ConstEntry
  , targetPlatform :: Platform
    -- | Length of the tick in microseconds, which the period of a periodic
    -- timer has to be a multiple of.
  , tickMicroseconds :: Integer
  }

type ConstFoldMonad = ExceptT ConstFoldError (ST.State ConstFoldEnv)
