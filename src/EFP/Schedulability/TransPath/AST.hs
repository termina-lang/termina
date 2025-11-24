module EFP.Schedulability.TransPath.AST where

import Core.AST
import Utils.Annotations

data WCEPathCondAltType =
    WCEPathCondIf
    | WCEPathCondElseIf
    | WCEPathCondElse
    deriving Show

data WCEPathCondAlternative  = 
    WCEPathCondAlternative
        WCEPathCondAltType
        Bool -- ^ Is the else if taken?
        [WCEPathBlock] -- ^ blocks in the else if block
        Location
    deriving Show

data WCEPathCase =
    WCEPathCase
        Bool
        [WCEPathBlock] -- ^ blocks in the case block
        Location
    deriving Show

-- | Worst-case execution path block
data WCEPathBlock
    = 
    -- | If-else-if block
    WCEPConditional
        [WCEPathCondAlternative] -- ^ list of alternatives
    -- | For-loop basic block
    | WCEPForLoop
        Integer -- ^ number of iterations in the worst-case
        [WCEPathBlock] -- ^ blocks in the for loop body.
        Location
    -- | Match block
    | WCEPMatch 
        [WCEPathCase] -- ^ list of match blocks
    -- | Send message
    | WCEPSendMessage 
        Identifier -- ^ Task name
        Identifier -- ^ Action name
        Location
    -- | Invoke a resource procedure
    | WCEPProcedureInvoke 
        Identifier -- ^ Resource name
        Identifier -- ^ Procedure name 
        Location
    | WCEPAllocBox 
        Identifier -- ^ Port name
        Location
    -- | Call to the free procedure of a memory allocator 
    | WCEPFreeBox 
        Identifier -- ^ Port name
        Location
    -- | Regular block (list of statements)
    | WCEPRegularBlock Location
    | WCEPReturn Location
    | WCEPContinue 
        Identifier -- ^ Action name
        Location
    | WCEPReboot Location
    -- | System call
    | WCEPSystemCall Location
    deriving Show

data TransactionalWCEPath
    = TransactionalWCEPath 
        Identifier -- ^ task/resource name
        Identifier -- ^ action/procedure name
        Identifier -- ^ path name
        [WCEPathBlock]
    deriving Show

----------------------------------------
-- Termina Programs definitions

newtype PathModuleImport = ModuleImport' [String]
    deriving Show

data PathModule = PathModule PathModuleImport [TransactionalWCEPath]
    deriving Show
