{-# LANGUAGE MultiParamTypeClasses #-}
-- | Small toy implementation of top sort.
-- Instead of using other implementations, we fail when a cycle is detected.
-- There are several implementations, but we use a DFS.
-- For the good parts, https://en.wikipedia.org/wiki/Topological_sorting#:~:text=graph%20drawing.-,Depth,-%2Dfirst%20search%5B
-- For the bad parts, pm Martín.

module Extras.TopSort where

-- import qualified Data.Set as S  -- We'll keep a set of visited nodes.
import qualified Extras.Set as S
import qualified Data.Map.Strict as M  -- We interpret map as graphs.

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except

data TopSt k e
  = St
  -- Temporary marked set
  { getTemp :: M.Map k e
  -- Permanent marked set
  , getPerm :: S.Set k
  , res :: [k]
  }

class (Ord k) => TopSortKey k e where
  topSortKey :: e -> k

emptyTopS :: TopSt k e
emptyTopS = St M.empty S.empty []

addTemp :: (TopSortKey k e) => k -> e -> TopSt k e -> TopSt k e
addTemp k e st = st{getTemp = M.insert k e (getTemp st)}

-- Add to temp and perm sets
addPerm :: TopSortKey k e => k -> TopSt k e -> Maybe (TopSt k e)
addPerm a sets =
  -- maybe (error "TopSort Max Bound -addPerm-")
  (\s -> sets{getPerm = s}) <$> S.insert a (getPerm sets)

addL :: k -> TopSt k e -> TopSt k e
addL a st = st{res=a : res st}

-- Remove or nothing.
rmTemp :: TopSortKey k e => k -> TopSt k e -> TopSt k e
rmTemp a st = st{getTemp = M.delete a (getTemp st)}

data TopSortError k e
  = ELoop [e] -- ^ Loop Error fund.
  | ENotFound e k -- ^ Internal error.
  | MaxBound
  deriving Show

-- Monad to compute.
-- Error TopSortError and State TopSt
type TopSort k e = ExceptT (TopSortError k e) (State (TopSt k e))

-- Adj map
-- k == keys
-- e == elements
type Graph k e = M.Map k [e]

modifyE :: (TopSt k e -> Maybe (TopSt k e)) -> TopSort k e ()
modifyE m = get >>= maybe (throwError MaxBound) put . m

-------------------------------------------------
-- Note [0]
-- We return two elements forming a cycle (loop).
-- These elements may be part of the loop, but the ones causing it may be two
-- others.
-------------------------------------------------
-- | Main function giving a solution to all contraints or a loop.
topSort
  :: (TopSortKey k e)
  -- | Takes a dependency graph
  => Graph k e
  -- Returns either a loop between elements [0] or an ordered list of them.
  -> Either (TopSortError k e) [k]
topSort graph = evalState (runExceptT (computation >> gets (reverse . res))) emptyTopS
  where
    computation = topSortInternal graph

-- | Straight topSort function from dependency list.
topSortFromDepList :: (TopSortKey k e) => [(k, [e])] -> Either (TopSortError k e) [k]
topSortFromDepList = topSort . M.fromList

topSortInternal
  :: TopSortKey k e
  -- Graph
  => Graph k e
  -- Result
  -> TopSort k e ()
topSortInternal graph =
  mapM_
   (\(k, es) ->
    get >>= \sets ->
    if S.member k (getPerm sets) || M.member k (getTemp sets)
    then -- skip if marked
      return ()
    else do -- if a is unmarked
        mapM_ (visit graph k) es
        modifyE (addPerm k)
        modify (addL k)
   ) $ M.toList graph

visit :: TopSortKey k e => Graph k e -> k -> e -> TopSort k e ()
visit graph parent src
  = gets getPerm >>= \permSet ->
  if S.member (topSortKey src) permSet
  then return ()
  else
    do
      tempSet <- gets getTemp
      when (M.member (topSortKey src) tempSet) (throwError (ELoop (M.elems tempSet ++ [src])))
      -- temp mark |src|
      modify (addTemp (topSortKey src) src)
      --
      case M.lookup (topSortKey src) graph of
          Nothing -> throwError (ENotFound src parent)
          Just adj_src -> mapM_ (visit graph (topSortKey src)) adj_src
      modify (rmTemp (topSortKey src))
      modifyE (addPerm (topSortKey src))
      modify (addL (topSortKey src))
