{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.VarUsage.Types where
import Utils.Annotations
import Utils.Errors

-- Special variables go from |Defined| -> |Allocated| -> |Used|.
-- Unless the method is a /procedure/, in which case, it can take boxes as
-- arguments.
data MVars
  = Defined Location
  | Allocated Location
  | Moved Location
  deriving (Show)

instance Located MVars where
  getLocation (Defined loc) = loc
  getLocation (Allocated loc) = loc
  getLocation (Moved loc) = loc

  updateLocation (Defined _) loc = Defined loc
  updateLocation (Allocated _) loc = Allocated loc
  updateLocation (Moved _) loc = Moved loc

sameState :: MVars -> MVars -> Bool
sameState (Defined _) (Defined _) = True
sameState (Allocated _) (Allocated _) = True
sameState (Moved _) (Moved _) = True
sameState _ _ = False

isAllocated :: MVars -> Bool
isAllocated (Allocated _) = True
isAllocated _ = False

instance ShowText MVars where
    showText (Defined _) = "defined"
    showText (Allocated _) = "allocated"
    showText (Moved _) = "moved"