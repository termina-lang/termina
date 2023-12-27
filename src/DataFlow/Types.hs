-- | Some useful Type definitions

module DataFlow.Types where

-- Special variables go from |Defined| -> |Allocated| -> |Used|.
-- Unless the method is a /procedure/, in which case, it can take dyns as
-- arguments.
data MVars
  = Defined
  | Allocated
  | Used
  deriving Show
