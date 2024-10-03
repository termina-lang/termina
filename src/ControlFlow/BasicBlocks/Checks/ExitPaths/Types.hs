module ControlFlow.BasicBlocks.Checks.ExitPaths.Types (
    BBPathsCheck,
    ExitPathsCheckST(..),
    setMustExit,
    setPartialExit,
    setAllowedContinue,
    setAllowedSend,
    setExitNotAllowed,
    localScope
) where

import Control.Monad.Except
import ControlFlow.BasicBlocks.Checks.ExitPaths.Errors
import qualified Control.Monad.State as ST

data ExitPathsCheckST = 
    EPMustExit
    | EPPartialExit
    | EPAllowedContinue
    | EPAllowedSend
    | EPExitNotAllowed
    deriving (Show, Eq, Ord)

type BBPathsCheck = ExceptT PathsCheckError (ST.State ExitPathsCheckST)

setMustExit :: BBPathsCheck ()
setMustExit = do
    ST.put EPMustExit

setPartialExit :: BBPathsCheck ()
setPartialExit = do
    ST.put EPPartialExit

setAllowedContinue :: BBPathsCheck ()
setAllowedContinue = do
    ST.put EPAllowedContinue

setAllowedSend :: BBPathsCheck ()
setAllowedSend = do
    ST.put EPAllowedSend

setExitNotAllowed :: BBPathsCheck ()
setExitNotAllowed = do
    ST.put EPExitNotAllowed

-- Execute comp but bracktracking the state
-- Useful for blocks semantic analysis
localScope :: BBPathsCheck a -> BBPathsCheck a
localScope comp = do
  prevst <- ST.get
  res <- comp
  ST.put prevst
  return res
