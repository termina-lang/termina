module ControlFlow.BasicBlocks.Types (
    BBGenerator,
    BBPathsCheck,
    BBPathsCheckST(..),
    setMustExit,
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

data BBPathsCheckST = 
    BBMustExit
    | BBPartialExit
    | BBAllowedContinue
    | BBAllowedSend
    | BBExitNotAllowed
    deriving (Show, Eq, Ord)

type BBPathsCheck = ExceptT BBPathsCheckError (ST.State BBPathsCheckST)

setMustExit :: BBPathsCheck ()
setMustExit = do
    ST.put BBMustExit

setPartialExit :: BBPathsCheck ()
setPartialExit = do
    ST.put BBPartialExit

setAllowedContinue :: BBPathsCheck ()
setAllowedContinue = do
    ST.put BBAllowedContinue

setAllowedSend :: BBPathsCheck ()
setAllowedSend = do
    ST.put BBAllowedSend

setExitNotAllowed :: BBPathsCheck ()
setExitNotAllowed = do
    ST.put BBExitNotAllowed

-- Execute comp but bracktracking the state
-- Useful for blocks semantic analysis
localScope :: BBPathsCheck a -> BBPathsCheck a
localScope comp = do
  prevst <- ST.get
  res <- comp
  ST.put prevst
  return res
