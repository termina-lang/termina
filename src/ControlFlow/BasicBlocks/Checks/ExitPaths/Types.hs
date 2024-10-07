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

-- | State of the exit paths check
data ExitPathsCheckST = 
    -- | Initial state. In this state, the block must exit. This can be done by
    -- a return statement or, if we are in an action, a continue statement.
    EPMustExit
    -- | Partial exit. This state is used when in the following statements, the
    -- block may exit on a branch but not it all branches. This is used for
    -- if-else and match statements that precede the last statement of the
    -- block. This state may only be reached when we are analyzing an action.
    -- In any other blocks, where continue statements are not allowed, this
    -- state is unreachable. When we are in this state, the code may either
    -- execute a branch statement or a send statement. If a send statement is
    -- executed, the block may no longer exit and the checker's state changes to
    -- EPAllowedSend.
    | EPPartialExit
    -- | Allowed to continue. This is an internal state that is used when we
    -- are analyzing a branch inside an action that is allowed to continue, i.e., 
    -- its last statement may be a continue statement.
    | EPAllowedContinue
    -- | Allowed to send. This state can only be reached when analyzing an
    -- action.  In this state, the block may no longer exit and it may send
    -- messages (it may do so on one or more branches), or execute a non-sending
    -- statement. In that case, the state changes to EPExitNotAllowed.
    | EPAllowedSend
    -- | Exit not allowed. This state is reached when the block is not allowed
    -- to exit nor, in the case of an action, to send messages. This state is
    -- the last state of the checker.
    | EPExitNotAllowed
    deriving (Show, Eq, Ord)

type BBPathsCheck = ExceptT PathsCheckError (ST.State ExitPathsCheckST)

-- | Set the state of the checker to "must exit" (EPMustExit)
setMustExit :: BBPathsCheck ()
setMustExit = do
    ST.put EPMustExit

-- | Set the state of the checker to "partial exit" (EPPartialExit)
setPartialExit :: BBPathsCheck ()
setPartialExit = do
    ST.put EPPartialExit

-- | Set the state of the checker to "allowed continue" (EPAllowedContinue)
setAllowedContinue :: BBPathsCheck ()
setAllowedContinue = do
    ST.put EPAllowedContinue

-- | Set the state of the checker to "allowed send" (EPAllowedSend)
setAllowedSend :: BBPathsCheck ()
setAllowedSend = do
    ST.put EPAllowedSend

-- | Set the state of the checker to "exit not allowed" (EPExitNotAllowed)
setExitNotAllowed :: BBPathsCheck ()
setExitNotAllowed = do
    ST.put EPExitNotAllowed

-- Executes a computation in a local scope. The state of the checker is saved
-- before the computation is executed and restored afterwards.
localScope :: BBPathsCheck a -> BBPathsCheck a
localScope comp = do
  prevst <- ST.get
  res <- comp
  ST.put prevst
  return res
