module EFP.Schedulability.TransPath.AST
    (module EFP.Schedulability.Core.AST
    , module EFP.Schedulability.TransPath.AST) where

import EFP.Schedulability.Core.AST
import qualified Data.Map.Strict as M

type WCETime = Double
type TRPOperationMap a = M.Map Identifier [TRPOperation a]
type TRPStepMap a = M.Map Identifier (TRPOperation a)

-- | Worst-case execution path block
data TransPathBlock a
    = 
    TPBlockCondIf
        [TransPathBlock a] -- ^ blocks in the if block
        BlockPosition
        a
    -- | Else-if block
    | TPBlockCondElseIf
        [TransPathBlock a] -- ^ blocks in the else-if block
        BlockPosition
        a
    -- | Else block
    | TPBlockCondElse
        [TransPathBlock a] -- ^ blocks in the else block
        BlockPosition
        a
    -- | For-loop basic block
    | TPBlockForLoop
        Integer -- ^ Number of iterations
        [TransPathBlock a] -- ^ blocks in the for loop body.
        BlockPosition
        a
    -- | Match case block
    | TPBlockMatchCase
        [TransPathBlock a] -- ^ blocks in the case block
        BlockPosition
        a
    | TPBlockMemberFunctionCall 
        [ConstExpression a] -- ^ Constant argument expressions
        (TRPOperation a) -- ^ Operation being called
        BlockPosition
        a
    -- | Invoke a resource procedure
    | TPBlockProcedureInvoke 
        [ConstExpression a] -- ^ Constant argument expression
        (TRPOperation a) -- ^ Operation being called
        BlockPosition
        a
    | TPBlockAllocBox 
        Identifier -- ^ Pool resource name
        BlockPosition
        a
    -- | Call to the free procedure of a memory allocator 
    | TPBlockFreeBox 
        Identifier -- ^ Pool resource name
        BlockPosition
        a
    | TPBlockReturn BlockPosition a
    | TPBlockReboot BlockPosition a
    | TPBlockSystemCall 
        Identifier
        [ConstExpression a] -- ^ Constant argument expression
        BlockPosition
        a
    deriving Show

data TRPOperation a
    =
    TRPTaskOperation
        Identifier -- ^ step name
        Identifier -- ^ task name
        Identifier -- ^ action name
        Identifier -- ^ path name
        [TransPathBlock a] -- ^ Blocks in the operation
        [Identifier] -- ^ Continuation operations
        WCETime -- ^ Worst-case execution time
        a -- ^ Annotation
    | TRPHandlerOperation
        Identifier -- ^ step name
        Identifier -- ^ handler name
        Identifier -- ^ action name
        Identifier -- ^ path name
        [TransPathBlock a] -- ^ Blocks in the operation
        [Identifier] -- ^ Continuation operations
        WCETime -- ^ Worst-case execution time
        a -- ^ Annotation
    | TRPResourceOperation
        Identifier -- ^ resource name
        Identifier -- ^ procedure/method name
        Identifier -- ^ path name
        [TransPathBlock a] -- ^ Blocks in the operation
        WCETime -- ^ Worst-case execution time
        a -- ^ Annotation
    deriving Show

data TransactionPath a =
    SimpleTransactionPath
        Identifier -- ^ Initial step identifier
        (TRPOperationMap a) -- ^ Map of operations
        a -- ^ Annotation
    | CondTransactionPath
        [(TInteger, Identifier, TRPOperationMap a)] -- ^ Conditional branches
        a -- ^ Annotation