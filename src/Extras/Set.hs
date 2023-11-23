-- | Wrapper implementation of Data.Set
-- The problem is that we want to check that everytime we add an element we are
-- bellow the maxBound.

module Extras.Set
  (module OriginalSet
  , insert
  , union
  )
where

import Data.Set as OriginalSet hiding (union, insert)
import qualified Data.Set as S (union, insert)

insert :: Ord a => a -> Set a -> Maybe (Set a)
insert x s
  = if size s < maxBound
  then Just (S.insert x s) else Nothing

union :: Ord a => Set a -> Set a -> Maybe (Set a)
union s1 s2
  = if (size s1 + size s2) < maxBound
  then Just (S.union s1 s2)
  else Nothing
