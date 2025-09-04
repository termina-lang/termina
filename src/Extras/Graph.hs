{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Extras.Graph where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.List as L

-- Adj map
-- k == keys
-- e == elements
type Graph k e = M.Map k [e]

-- | Implementation of top sort.
-- Instead of using other implementations, we fail when a cycle is detected.
-- There are several implementations, but we use a DFS.
-- For the good parts, https://en.wikipedia.org/wiki/Topological_sorting#:~:text=graph%20drawing.-,Depth,-%2Dfirst%20search%5B
-- For the bad parts, pm Martín.

data TopSt k e
  = St
  -- Temporary marked set
  { getTemp :: M.Map k e
  -- Permanent marked set
  , getPerm :: S.Set k
  , res :: [k]
  }

class (Ord k) => GraphKey k e where
  graphKey :: e -> k

emptyTopS :: TopSt k e
emptyTopS = St M.empty S.empty []

addTemp :: (GraphKey k e) => k -> e -> TopSt k e -> TopSt k e
addTemp k e st = st{getTemp = M.insert k e (getTemp st)}

-- Add to temp and perm sets
addPerm :: GraphKey k e => k -> TopSt k e -> TopSt k e
addPerm a sets = sets {
    getPerm = S.insert a (getPerm sets) }

addL :: k -> TopSt k e -> TopSt k e
addL a st = st{res=a : res st}

-- Remove or nothing.
rmTemp :: GraphKey k e => k -> TopSt k e -> TopSt k e
rmTemp a st = st{getTemp = M.delete a (getTemp st)}

data TopSortError k e
  = ELoop [e] -- ^ Loop Error fund.
  | ENotFound e k -- ^ Internal error.
  deriving Show

-- Monad to compute.
-- Error TopSortError and State TopSt
type TopSort k e = ExceptT (TopSortError k e) (State (TopSt k e))

modifyE :: (TopSt k e -> TopSt k e) -> TopSort k e ()
modifyE m = get >>= put . m

instance GraphKey String String where
  graphKey = id

-------------------------------------------------
-- Note [0]
-- We return two elements forming a cycle (loop).
-- These elements may be part of the loop, but the ones causing it may be two
-- others.
-------------------------------------------------
-- | Main function giving a solution to all contraints or a loop.
topSort
  :: (GraphKey k e)
  -- | Takes a dependency graph
  => Graph k e
  -- Returns either a loop between elements [0] or an ordered list of them.
  -> Either (TopSortError k e) [k]
topSort graph = evalState (runExceptT (computation >> gets (reverse . res))) emptyTopS
  where
    computation = topSortInternal graph

-- | Straight topSort function from dependency list.
topSortFromDepList :: (GraphKey k e) => [(k, [e])] -> Either (TopSortError k e) [k]
topSortFromDepList = topSort . M.fromList

topSortInternal
  :: GraphKey k e
  -- Graph
  => Graph k e
  -- Result
  -> TopSort k e ()
topSortInternal graph =
  -- Iterate over each node (key k) and its direct dependencies (list es) in the graph.
  -- M.toList converts the graph (a Map) into a list of (key, value) pairs.
  mapM_
   (\(k, es) ->
    -- Get the current state (sets of marked nodes and the result list).
    get >>= \sets ->
    -- Check if the current node 'k' is already permanently marked (getPerm sets)
    -- or temporarily marked (getTemp sets).
    if S.member k (getPerm sets) || M.member k (getTemp sets)
    then -- If 'k' is already marked (either perm or temp), it means it has been
         -- visited or is currently in the recursion stack. Skip processing.
      return ()
    else do -- If 'k' is unmarked:
        -- For each direct dependency 'e' in 'es' of the current node 'k',
        -- perform a DFS visit. 'k' acts as the parent for these visits.
        mapM_ (visit graph k) es
        -- After all dependencies of 'k' (and their sub-dependencies) have been
        -- visited and processed, mark 'k' as permanently visited.
        -- modifyE handles potential errors from addPerm (e.g., MaxBound).
        modifyE (addPerm k)
        -- Add 'k' to the beginning of the result list ('res').
        -- Since nodes are added after their dependencies, this builds the list
        -- in reverse topological order. The final list will be reversed by the caller.
        modify (addL k)
   ) $ M.toList graph

-- | Performs the DFS visit for a specific dependency 'src' of a 'parent' node.
-- It detects cycles and adds nodes to the sorted list in reverse post-order.
visit :: GraphKey k e => Graph k e -> k -> e -> TopSort k e ()
visit graph parent src
  -- Retrieve the set of permanently marked nodes from the state.
  = gets getPerm >>= \permSet ->
  -- Check if the key of the source element 'src' (obtained via 'graphKey')
  -- is already in the permanent set.
  if S.member (graphKey src) permSet
  -- If 'src' is permanently marked, it means this node and its dependencies
  -- have already been fully processed, so we can return and do nothing further for this path.
  then return ()
  else
    do
      -- Retrieve the set of temporarily marked nodes.
      -- Temporarily marked nodes are those currently in the recursion stack of the DFS.
      tempSet <- gets getTemp
      -- Check if the key of 'src' is in the temporary set.
      when (M.member (graphKey src) tempSet)
        -- If 'src' is in the temporary set, it means we have encountered 'src' again
        -- while it's already being processed (i.e., it's an ancestor in the DFS tree).
        -- This indicates a cycle in the graph.
        -- Throw an ELoop error, providing the elements currently in the temporary set
        -- (which form part of the cycle) plus the current 'src' element.
        (throwError (ELoop (M.elems tempSet ++ [src])))
      -- Mark 'src' temporarily by adding its key and the element itself to the temporary set.
      -- This signifies that 'src' is now in the current recursion path.
      -- temp mark |src|
      modify (addTemp (graphKey src) src)
      --
      -- Look up the key of 'src' in the main graph to find its adjacent nodes (i.e., its direct dependencies).
      case M.lookup (graphKey src) graph of
          -- If the key of 'src' is not found in the graph, it means 'src' was listed as a dependency
          -- for 'parent', but 'src' itself doesn't have an entry defining its own dependencies (or lack thereof).
          -- This is considered an internal error or an ill-formed graph.
          Nothing -> throwError (ENotFound src parent)
          -- If the key of 'src' is found, 'adj_src' is the list of its direct dependencies.
          -- Recursively call 'visit' for each dependency 'e' in 'adj_src'.
          -- The 'parent' for these recursive calls becomes the key of the current 'src' node.
          Just adj_src -> mapM_ (visit graph (graphKey src)) adj_src
      -- After all dependencies of 'src' have been visited (i.e., the recursive calls above have completed),
      -- remove the key of 'src' from the temporary set.
      -- This signifies that 'src' is no longer in the current recursion path.
      modify (rmTemp (graphKey src))
      -- Add the key of 'src' to the permanent set.
      -- This marks 'src' as fully processed – all its descendants have been visited.
      modifyE (addPerm (graphKey src))
      -- Prepend the key of 'src' to the result list ('res' in the state 'TopSt').
      -- Since nodes are added after their dependencies are processed, this builds the list
      -- in reverse topological order. The final list will be reversed by the caller.
      modify (addL (graphKey src))

-- | Reverses a graph. 
-- If the original graph has an edge P -> C, the reversed graph will have an
-- edge C -> P. The keys of the resulting map are of type 'k', and the adjacency
-- lists contain keys 'k'.
reverseGraph
  :: (GraphKey k e)
  => Graph k e      -- ^ The original graph (Map from node key k to list of dependencies e)
  -> M.Map k [k]    -- ^ The reversed graph (Map from node key k to list of nodes that depend on it)
reverseGraph graph =

  let
    -- 1. Collect all unique node keys present in the original graph.
    -- These include keys of the graph map and keys derived from dependency elements.
    allOriginalNodeKeys = S.fromList $ M.keys graph
    allDependencyElements = concatMap snd (M.toList graph)
    allDependencyNodeKeys = S.fromList $ map graphKey allDependencyElements

    -- Get a list of all unique keys of type 'k'.
    -- These will be the keys in our reversed graph.
    uniqueNodeKeys = S.union allOriginalNodeKeys allDependencyNodeKeys

    -- 2. Initialize the reversed graph.
    -- Every unique node key from the original graph becomes a key in the reversed graph,
    -- initially mapped to an empty list (meaning, so far, no nodes depend on it).
    initialReversedGraph = M.fromList [(k, []) | k <- S.toList uniqueNodeKeys]

  in
    -- 3. Populate the reversed graph by iterating through the original graph.
    -- For each node 'parentNodeK' and its dependency 'dependencyE' (which maps to 'childNodeK'),
    -- add 'parentNodeK' to the list of nodes that depend on 'childNodeK'.
    M.foldrWithKey processNode initialReversedGraph graph

  where

    -- processNode is called for each (parentNodeK, directDependenciesE) pair in the original graph.
    processNode :: (GraphKey k e) => k -> [e] -> M.Map k [k] -> M.Map k [k]
    processNode parentNodeK directDependenciesE accReversedGraph =
      -- For each direct dependency 'childE' of 'parentNodeK':
      L.foldl' (addReverseEdge parentNodeK) accReversedGraph directDependenciesE

    -- addReverseEdge updates the accumulator for a single original dependency.
    addReverseEdge :: (GraphKey k e) => k -> M.Map k [k] -> e -> M.Map k [k]
    addReverseEdge parentNodeK accMap childE =
      let childNodeK = graphKey childE
      -- Add 'parentNodeK' to the list of nodes that depend on 'childNodeK'.
      -- M.adjust ensures that 'childNodeK' must already be a key in accMap,
      -- which is guaranteed by 'initialReversedGraph'.
      in
      M.adjust (parentNodeK :) childNodeK accMap

data FindDisPathSt k = FindDisPathSt
  { globallyVisitedIntermediates :: S.Set k
  -- ^ Set of globally forbidden intermediate nodes.
  , accumulatedPaths :: [[k]]
  -- ^ Accumulated paths 
  }

data FindDisPathsError k 
  = EFDPNoDependencies k -- ^ No dependencies found for the given starting key.
  | EFDPNotFound k -- ^ Internal error.
  deriving Show

-- Monad to compute.
-- Error TopSortError and State TopSt
type FindDisPathMonad k e = ExceptT (FindDisPathsError k) (State (FindDisPathSt k))

findDisjointPaths ::
  (GraphKey k e) 
  -- | Takes a dependency graph
  => Graph k e
  -- | Starting node key
  -> k
  -- | Returns a list of disjoint paths from the starting node to all reachable nodes.
  -> Either (FindDisPathsError k) [[k]]
findDisjointPaths graph startNodeK =  
  evalState (runExceptT (findDisPathsInternal graph startNodeK >> gets accumulatedPaths)) (FindDisPathSt S.empty [])

findDisPathsInternal :: (GraphKey k e) => Graph k e -> k -> FindDisPathMonad k e ()
findDisPathsInternal graph startNodeK = do
  -- Check if the starting node is in the map
  case M.lookup startNodeK graph of
    Just [] -> throwError (EFDPNoDependencies startNodeK)
    Just [depE] -> 
      let depK = graphKey depE in
      modify (\s -> s { accumulatedPaths = [[depK]] })
    Just deps -> do
      -- If the starting node has dependencies, find disjoint paths.
      forM_ deps $ \depE -> do
        let depK = graphKey depE
        dfs graph depK [] -- Start DFS from the dependency node

    Nothing ->
      -- If the starting node is not found in the graph, throw an error.
      throwError (EFDPNotFound startNodeK)
  
  where 

    dfs :: (GraphKey k e) => Graph k e -> k -> [k] -> FindDisPathMonad k e ()
    dfs graph' currentNodeK path = do
      -- Check if the current node is already globally visited.
      visitedSet <- gets globallyVisitedIntermediates
      if S.member currentNodeK visitedSet then 
        return ()
      else do
        -- Mark the current node as globally visited to avoid repeating it in other paths.
        modify (\s -> s { globallyVisitedIntermediates = S.insert currentNodeK (globallyVisitedIntermediates s) })
        case M.lookup currentNodeK graph' of
          Just [] -> do
            -- If there are no more dependencies, complete the current path
            let fullPath = currentNodeK : path
            modify (\s -> s { accumulatedPaths = fullPath : accumulatedPaths s })
          Just [depE] -> do
            let nextNodeK = graphKey depE
            if S.member nextNodeK visitedSet then 
              return ()
            else do
              -- If the next node is not globally visited, continue the DFS.
              dfs graph' nextNodeK (currentNodeK : path)

          Just deps -> do
            -- If there are dependencies, continue the DFS for each dependency.
            forM_ deps $ \depE -> do
              let nextNodeK = graphKey depE
              dfs graph' nextNodeK (currentNodeK : path)
          Nothing -> throwError (EFDPNotFound currentNodeK)

