-- | Debugging Module

module Debugging (
  showMsg,
  module Debug.Trace
) where

import Debug.Trace

showMsg :: Monad m => String -> m ()
showMsg = flip trace (return ())
