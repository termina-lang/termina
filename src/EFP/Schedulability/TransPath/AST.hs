module EFP.Schedulability.TransPath.AST where

import Core.AST
import Utils.Annotations
import ControlFlow.BasicBlocks.AST

-- | Worst-case execution path block
data WCEPathBlock a
    = 
    -- | If block
    WCEPathCondIf
        [WCEPathBlock a] -- ^ blocks in the if block
        Location
    -- | Else-if block
    | WCEPathCondElseIf
        [WCEPathBlock a] -- ^ blocks in the else-if block
        Location
    -- | Else block
    | WCEPathCondElse
        [WCEPathBlock a] -- ^ blocks in the else block
        Location
    -- | For-loop basic block
    | WCEPathForLoop
        (Expression a) -- ^ initial value of the iterator
        (Expression a) -- ^ final value of the iterator
        [WCEPathBlock a] -- ^ blocks in the for loop body.
        Location
    -- | Match case block
    | WCEPathMatchCase
        [WCEPathBlock a] -- ^ blocks in the case block
        Location
    -- | Send message
    | WCEPSendMessage 
        Identifier -- ^ Port name
        Location
    | WCEPathMemberFunctionCall 
        Identifier -- ^ Function name
        [Expression a] -- ^ Constant argument expressions
        Location
    -- | Invoke a resource procedure
    | WCEPProcedureInvoke 
        Identifier -- ^ Port name
        Identifier -- ^ Procedure name 
        [Expression a] -- ^ Constant argument expression
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
    | WCEPSystemCall Identifier Location
    deriving Show

data TransactionalWCEPath a
    = TransactionalWCEPath 
        Identifier -- ^ task/resource name
        Identifier -- ^ action/procedure name
        Identifier -- ^ path name
        [WCEPathBlock a]
    deriving Show

----------------------------------------
-- Termina Programs definitions

newtype PathModuleImport = ModuleImport' [String]
    deriving Show

data PathModule a = PathModule PathModuleImport [TransactionalWCEPath a]
    deriving Show
