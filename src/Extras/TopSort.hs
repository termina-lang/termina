{-# Language LambdaCase #-}
-- | Small toy implementation of top sort.
-- Instead of using other implementations, we fail when a cycle is detected.
-- There are several implementations, but we use a DFS.
-- For the good parts, https://en.wikipedia.org/wiki/Topological_sorting#:~:text=graph%20drawing.-,Depth,-%2Dfirst%20search%5B
-- For the bad parts, pm Martín.

module Extras.TopSort where

import qualified Data.Set as S  -- We'll keep a set of visited nodes.
import qualified Data.Map.Strict as M  -- We interpret map as graphs.

import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Functor

data TopSt a
  = St
  -- Temporary marked set
  { getTemp :: S.Set a
  -- Permanent marked set
  , getPerm :: S.Set a
  }

emptyTopS :: TopSt a
emptyTopS = St S.empty S.empty

addTemp, addPerm :: Ord a => a -> TopSt a -> TopSt a
addTemp a (St temp perm) = St (S.insert a temp) perm
addPerm a (St temp perm) = St temp (S.insert a perm)

-- Remove or nothing.
rmTemp :: Ord a => a -> TopSt a -> TopSt a
rmTemp a (St temp perm) = St (S.delete a temp) perm

data TopE a
  = EEnd a
  | ELoop (a,a)
  | ENotFound a

type TopSort a = ExceptT (TopE a) (State (TopSt a))

lmodify :: (s -> s) -> ExceptT e (State s) ()
lmodify = lift . modify

-- lError :: e -> StateT a (Except e) b
-- lError = lift . throwError

-- lCatch :: StateT a (Except e) l -> (e -> StateT a (Except e) b) -> StateT a (Except e) b

-- Adj map
type Graph a = M.Map a [a]

-------------------------------------------------
-- Note [0]
-- We return two elements forming a cycle (loop).
-- These elements may be part of the loop, but the ones causing it may be two
-- others.
-------------------------------------------------
-- | Main function giving a solution to all contraints or a loop.
topSort
  :: Ord a
  -- | Takes a dependency graph
  => Graph a
  -- Returns either a loop between elements [0] or an ordered list of them.
  -> Either (a,a) [a]
topSort graph = case evalState (runExceptT computation) emptyTopS of
  Left (EEnd _a) -> error "[Top Sort] Impossible Behaviour: Only one endpoint of a loop found."
  Left (ENotFound _a) -> error "[Top Sort] Impossible Behaviour: Node not found in node list."
  Left (ELoop lpair) -> Left lpair
  Right res -> Right res
  where
    nodes = M.keys graph
    computation = topSortInternal graph nodes []

-- | Straight topSort function from dependency list.
topSortFromDepList :: Ord a => [(a, [a])] -> Either (a,a) [a]
topSortFromDepList = topSort . M.fromList

topSortInternal
  :: Ord a
  -- Graph
  => Graph a
  -- Nodes
  -> [a]
  -- Accum ordered list
  -> [a]
  -- Result
  -> TopSort a [a]
topSortInternal _graph [] acc = return acc
topSortInternal graph (a:as) acc =
  gets getPerm >>= \permSet ->
  if S.member a permSet
  then topSortInternal graph as acc
  else
    catchError
         (visit graph a)
         (\case {EEnd s -> throwError (ELoop (a, s)); s -> throwError s})
    <&> (++ acc)

visit :: Ord a => Graph a -> a -> TopSort a [a]
visit graph src
  = gets getPerm >>= \permSet ->
  if S.member src permSet then return []
  else
    do
      tempSet <- gets getTemp
      when (S.member src tempSet) (throwError (EEnd src))
      -- temp mark |src|
      lmodify (addTemp src)
      accms <- case M.lookup src graph of
                    Nothing -> throwError (ENotFound src)
                    Just adj_src -> mapM (visit graph) adj_src
      lmodify (addPerm src . rmTemp src)
      return (src : concat accms)
