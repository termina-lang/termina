{-# LANGUAGE FlexibleContexts #-}
-- | The skeleton of a forward data-flow pass over the basic-block AST.
--
-- A pass of this kind keeps two kinds of state and the difference between them
-- is what makes its branches correct: what belongs to the path being walked,
-- which has to be restored when a branch ends and joined where paths meet, and
-- what belongs to the program, which is never restored because "there is a path
-- where this happens" is a fact that no branch can undo. 'DFState' names that
-- split, 'branch' restores only the first half, and the lattice of the first
-- half says how paths join.
--
-- What the caller supplies is the meaning of each node ('Transfer'); the control
-- flow of the language is written here once: an @if@ with no @else@ leaves a
-- path that does nothing, the cases of a @match@ with no default are exhaustive,
-- and what the body of a loop assigns does not hold after it.
module ControlFlow.Dataflow (
    Lattice(..)
  , DFState(..)
  , DataflowM
  , Transfer(..)
  , getPath
  , putPath
  , modifyPath
  , getGlobal
  , modifyGlobal
  , branch
  , joinPaths
  , fixpoint
  , walkForward
  , runDataflow
) where

import ControlFlow.BasicBlocks.AST
import Semantic.Types (SemanticAnn)

import Control.Monad.Except
import qualified Control.Monad.State as ST

-- | The state of a path, which is what the passes reason with. The join is
-- applied where two paths meet, and the equality is what stops the walk of a
-- loop.
class Eq p => Lattice p where
  joinPath :: p -> p -> p

-- | A pass that carries nothing from one node to the next has no state of its
-- own along a path, and the walk of its branches costs nothing.
instance Lattice () where
  joinPath _ _ = ()

-- | What is true of the path being walked and what is true of the program.
data DFState p g = DFState
  {
    pathState :: p
  , globalState :: g
  }

type DataflowM p g e = ExceptT e (ST.State (DFState p g))

getPath :: DataflowM p g e p
getPath = ST.gets pathState

putPath :: p -> DataflowM p g e ()
putPath p = ST.modify (\st -> st { pathState = p })

modifyPath :: (p -> p) -> DataflowM p g e ()
modifyPath f = ST.modify (\st -> st { pathState = f (pathState st) })

getGlobal :: DataflowM p g e g
getGlobal = ST.gets globalState

modifyGlobal :: (g -> g) -> DataflowM p g e ()
modifyGlobal f = ST.modify (\st -> st { globalState = f (globalState st) })

-- | Walks one branch and returns the path it leaves behind, putting back the
-- path the branch started from so that the next one starts where this one did.
-- What the branch learned about the program stays.
branch :: DataflowM p g e () -> DataflowM p g e p
branch m = do
  entry <- getPath
  m
  out <- getPath
  putPath entry
  return out

-- | Joins the paths that meet after a conditional.
joinPaths :: Lattice p => [p] -> DataflowM p g e ()
joinPaths [] = return ()
joinPaths (p:ps) = putPath (foldl joinPath p ps)

-- | Walks the body of a loop until the state at its head stops growing, and
-- leaves that state behind. The body is walked from the head state and its exit
-- is joined back into it, which is what lets a value written in one turn be seen
-- by the next one; the walk of a turn that is not the last one is still allowed
-- to learn things about the program, and that is how a candidate found early is
-- rescued later.
fixpoint :: Lattice p => DataflowM p g e () -> DataflowM p g e ()
fixpoint body = getPath >>= go

  where

    go known = do
      putPath known
      out <- branch body
      let known' = joinPath known out
      if known' == known then putPath known' else go known'

-- | What a node means to the pass.
data Transfer p g e = Transfer
  {
    -- | A statement of a regular block.
    onStatement :: Statement SemanticAnn -> DataflowM p g e ()
    -- | A block that only evaluates expressions, which the pass reads through
    -- 'ControlFlow.BasicBlocks.Traversal.simpleBlockChildren'.
  , onSimpleBlock :: BasicBlock SemanticAnn -> DataflowM p g e ()
    -- | An expression evaluated in a control position without deciding a path,
    -- such as the bounds of a loop or the object a @match@ inspects.
  , onExpression :: Expression SemanticAnn -> DataflowM p g e ()
    -- | An expression whose value decides which path is taken.
  , onCondition :: Expression SemanticAnn -> DataflowM p g e ()
    -- | Entering a case of a @match@, which binds the variables of its variant.
  , onCaseEntry :: MatchCase SemanticAnn -> DataflowM p g e ()
    -- | Entering a loop, which declares an iterator of the given type that
    -- runs from the first of the two bounds up to but not including the
    -- second, one value per turn.
  , onLoopEntry ::
      Identifier -> TerminaType SemanticAnn
      -> Expression SemanticAnn -> Expression SemanticAnn
      -> DataflowM p g e ()
    -- | What is known inside the branch a condition guards, and inside the ones
    -- it does not. A pass that learns nothing from a condition leaves both at
    -- @pure ()@.
  , refineTrue :: Expression SemanticAnn -> DataflowM p g e ()
  , refineFalse :: Expression SemanticAnn -> DataflowM p g e ()
  }

-- | Walks a block forwards.
walkForward :: Lattice p => Transfer p g e -> Block SemanticAnn -> DataflowM p g e ()
walkForward transfer = walkBlock

  where

    walkBlock = mapM_ walkBasicBlock . blockBody

    walkBasicBlock (RegularBlock stmts) = mapM_ (onStatement transfer) stmts

    walkBasicBlock (IfElseBlock condIf elseIfs mElse _) = do
      onCondition transfer (condIfCond condIf)
      ifOut <- branch (refineTrue transfer (condIfCond condIf) >> walkBlock (condIfBody condIf))
      elseIfOuts <- walkElseIfs [condIfCond condIf] elseIfs
      entry <- getPath
      -- | Without an else branch there is a path that does nothing.
      elseOut <- case mElse of
        Nothing -> return entry
        Just elseBlock -> branch $ do
          mapM_ (refineFalse transfer) (condIfCond condIf : map condElseIfCond elseIfs)
          walkBlock (condElseBody elseBlock)
      joinPaths (ifOut : elseOut : elseIfOuts)

    walkBasicBlock (MatchBlock expr cases mDefaultCase _) = do
      onExpression transfer expr
      caseOuts <- mapM (\c -> branch (onCaseEntry transfer c >> walkBlock (matchBody c))) cases
      entry <- getPath
      case mDefaultCase of
        Just (DefaultCase blk _) -> do
          defaultOut <- branch (walkBlock blk)
          joinPaths (defaultOut : caseOuts)
        -- | Without a default case the listed cases are exhaustive.
        Nothing -> joinPaths (if null caseOuts then [entry] else caseOuts)

    -- | The break condition is part of the head of the loop, which the
    -- generated @for@ shows: it is evaluated before every turn, the first one
    -- included, and what a turn assigns is what the next one tests. It is
    -- therefore walked inside the loop and not before it.
    walkBasicBlock (ForLoopBlock iterator iteratorTy initE endE mBreak blk _) = do
      onExpression transfer initE
      onExpression transfer endE
      -- | What a pass records under the name of the iterator outlives the walk
      -- of the loop. Nothing reads it afterwards, since the iterator goes out
      -- of scope with the loop and a later declaration of the same name writes
      -- over the record.
      onLoopEntry transfer iterator iteratorTy initE endE
      fixpoint (mapM_ (onCondition transfer) mBreak >> walkBlock blk)

    walkBasicBlock block = onSimpleBlock transfer block

    -- | Each else-if is walked knowing that the conditions before it were false.
    walkElseIfs _ [] = return []
    walkElseIfs before (elseIf : rest) = do
      onCondition transfer (condElseIfCond elseIf)
      out <- branch $ do
        mapM_ (refineFalse transfer) before
        refineTrue transfer (condElseIfCond elseIf)
        walkBlock (condElseIfBody elseIf)
      outs <- walkElseIfs (before ++ [condElseIfCond elseIf]) rest
      return (out : outs)

-- | Runs a pass from the state it starts with, returning the error it raised, if
-- any, and the state it ended with.
runDataflow :: DFState p g -> DataflowM p g e a -> (Either e a, DFState p g)
runDataflow initial = flip ST.runState initial . runExceptT
