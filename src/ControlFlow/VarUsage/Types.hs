{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.VarUsage.Types where
import Utils.Annotations
import Utils.Errors

-- Special variables go from |Defined| -> |Allocated| -> |Used|.
-- Unless the method is a /procedure/, in which case, it can take boxes as
-- arguments.
data MVars
  = Defined TLocation
  | Allocated TLocation
  | Moved TLocation
  deriving (Show)

getLocation :: MVars -> TLocation
getLocation (Defined loc) = loc
getLocation (Allocated loc) = loc
getLocation (Moved loc) = loc

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