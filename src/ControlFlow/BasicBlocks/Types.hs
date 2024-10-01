module ControlFlow.BasicBlocks.Types (
    BBGenerator,
    BBExitCheck,
    BBExitCheckST(..),
    setPartialExit,
    setAllowedContinue,
    setAllowedSend,
    setExitNotAllowed,
    localScope
) where

import Control.Monad.Except
import ControlFlow.BasicBlocks.Errors.Errors
import qualified Control.Monad.State as ST

-- | This type represents the monad used to generate basic blocks.
type BBGenerator = Except BBGeneratorError

data BBExitCheckST = 
    BBMustExit
    | BBPartialExit
    | BBAllowedContinue
    | BBAllowedSend
    | BBExitNotAllowed
    deriving (Show, Eq, Ord)

type BBExitCheck = ExceptT BBExitCheckError (ST.State BBExitCheckST)

setPartialExit :: BBExitCheck ()
setPartialExit = do
    ST.put BBPartialExit

setAllowedContinue :: BBExitCheck ()
setAllowedContinue = do
    ST.put BBAllowedContinue

setAllowedSend :: BBExitCheck ()
setAllowedSend = do
    ST.put BBAllowedSend

setExitNotAllowed :: BBExitCheck ()
setExitNotAllowed = do
    ST.put BBExitNotAllowed

-- Execute comp but bracktracking the state
-- Useful for blocks semantic analysis
localScope :: BBExitCheck a -> BBExitCheck a
localScope comp = do
  prevst <- ST.get
  res <- comp
  ST.put prevst
  return res
