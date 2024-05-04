{-# Language LambdaCase #-}
-- | Small toy implementation of top sort.
-- Instead of using other implementations, we fail when a cycle is detected.
-- There are several implementations, but we use a DFS.
-- For the good parts, https://en.wikipedia.org/wiki/Topological_sorting#:~:text=graph%20drawing.-,Depth,-%2Dfirst%20search%5B
-- For the bad parts, pm Martín.

module Extras.TopSort where

-- import qualified Data.Set as S  -- We'll keep a set of visited nodes.
import qualified Extras.Set as S
import qualified Data.Map.Strict as M  -- We interpret map as graphs.

import Control.Monad.State.Strict
import Control.Monad.Except

data TopSt a
  = St
  -- Temporary marked set
  { getTemp :: S.Set a
  -- Permanent marked set
  , getPerm :: S.Set a
  , res :: [a]
  }

emptyTopS :: TopSt a
emptyTopS = St S.empty S.empty []

-- Add to temp and perm sets
addTemp, addPerm :: Ord a => a -> TopSt a -> Maybe (TopSt a)
addTemp a sets =
  (\s -> sets{getTemp = s}) <$> S.insert a (getTemp sets)
  -- maybe (error "TopSort Max Bound -addTemp-")
  -- sets{getTemp = S.insert a (getTemp sets)}
addPerm a sets =
  -- maybe (error "TopSort Max Bound -addPerm-")
  (\s -> sets{getPerm = s}) <$> S.insert a (getPerm sets)

addL :: a -> TopSt a -> TopSt a
addL a st = st{res=a : res st}

-- Remove or nothing.
rmTemp :: Ord a => a -> TopSt a -> TopSt a
rmTemp a st = st{getTemp = S.delete a (getTemp st)}

data TopE a
  = ELoop [a] -- ^ Loop Error fund.
  | ENotFound a -- ^ Internal error.
  | MaxBound
  deriving Show

-- Monad to compute.
-- Error TopE and State TopSt
type TopSort a = ExceptT (TopE a) (State (TopSt a))

-- Adj map
type Graph a = M.Map a [a]

lmodify :: (s -> s) -> ExceptT e (State s) ()
lmodify = lift . modify

lmodifyE :: (TopSt a -> Maybe (TopSt a)) -> TopSort a ()
lmodifyE m = getE >>= maybe (throwError MaxBound) putE . m
  where
    getE = lift get
    putE = lift . put

-------------------------------------------------
-- Note [0]
-- We return two elements forming a cycle (loop).
-- These elements may be part of the loop, but the ones causing it may be two
-- others.
-------------------------------------------------
-- | Main function giving a solution to all contraints or a loop.
topSort
  :: (Ord a)
  -- | Takes a dependency graph
  => Graph a
  -- Returns either a loop between elements [0] or an ordered list of them.
  -> Either (TopE a) [a]
topSort graph = evalState (runExceptT (computation >> gets (reverse . res))) emptyTopS
  where
    nodes = M.keys graph
    computation = topSortInternal graph nodes

-- | Straight topSort function from dependency list.
topSortFromDepList :: (Ord a) => [(a, [a])] -> Either (TopE a) [a]
topSortFromDepList = topSort . M.fromList

topSortInternal
  :: Ord a
  -- Graph
  => Graph a
  -- Nodes
  -> [a]
  -- Result
  -> TopSort a ()
topSortInternal graph =
  mapM_
   (\a ->
    get >>= \sets ->
    if S.member a (getPerm sets) || S.member a (getTemp sets)
    then -- skip if marked
      return ()
    else -- if a is unmarked
        visit graph a
   )

visit :: Ord a => Graph a -> a -> TopSort a ()
visit graph src
  = gets getPerm >>= \permSet ->
  if S.member src permSet
  then return ()
  else
    do
      tempSet <- gets getTemp
      when (S.member src tempSet) (throwError (ELoop (S.toList tempSet)))
      -- temp mark |src|
      lmodifyE (addTemp src)
      --
      accms <- case M.lookup src graph of
                    Nothing -> throwError (ENotFound src)
                    Just adj_src -> mapM (visit graph) adj_src
      lmodify (rmTemp src)
      lmodifyE (addPerm src)
      lmodify (addL src)
