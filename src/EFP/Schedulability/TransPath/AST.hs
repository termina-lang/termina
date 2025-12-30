module EFP.Schedulability.TransPath.AST where

import EFP.Schedulability.Core.AST

-- | Worst-case execution path block
data TransPathBlock a
    = 
    -- | For-loop basic block
    TPBlockForLoop
        (ConstExpression a) -- ^ initial value of the iterator
        (ConstExpression a) -- ^ final value of the iterator
        [TransPathBlock a] -- ^ blocks in the for loop body.
    | TPBlockMemberFunctionCall 
        [ConstExpression a] -- ^ Constant argument expressions
        (TransPathActivity a)
    -- | Invoke a resource procedure
    | TPBlockProcedureInvoke 
        [ConstExpression a] -- ^ Constant argument expression
        (TransPathActivity a)
    | TPBlockAllocBox 
        Identifier -- ^ Pool resource name
    -- | Call to the free procedure of a memory allocator 
    | TPBlockFreeBox 
        Identifier -- ^ Pool resource name
    -- | Regular block
    | TPBlockRegularBlock
    | TPBlockSystemCall 
        Identifier
        [ConstExpression a] -- ^ Constant argument expression
    deriving Show

data TransPathActivity a
    =
    TransPathActivity
        Identifier -- ^ component name
        Identifier -- ^ action/procedure/method name
        Identifier -- ^ path name
        [Identifier] -- ^ constant parameters
        [TransPathBlock a] -- ^ Blocks in the activity
        [TransPathActivity a] -- ^ Continuation activities
    deriving Show