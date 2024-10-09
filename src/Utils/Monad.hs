module Utils.Monad (localScope) where

import Control.Monad.State.Strict

-- Executes a computation in a local scope. The state of the checker is saved
-- before the computation is executed and restored afterwards.
localScope :: MonadState s m => m a -> m a
localScope comp = do
  prevst <- get
  res <- comp
  put prevst
  return res