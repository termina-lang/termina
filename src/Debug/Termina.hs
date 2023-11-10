-- | Debugging Module

module Debug.Termina (
  showMsg,
  showMsgVal,
  module Debug.Trace
) where

import Debug.Trace

showMsg :: Monad m => String -> m ()
showMsg = flip trace (return ())

showMsgVal :: String -> a -> a
showMsgVal = trace
