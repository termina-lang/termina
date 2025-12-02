module EFP.Schedulability.TransPath.AST
    (module EFP.Schedulability.TransPath.AST
    , module EFP.Schedulability.Core.AST) where

import EFP.Schedulability.Core.AST
import qualified Data.Map as M

data BlockPosition = BlockPosition
    {
        blockPosStartLine :: !Integer
    ,   blockPosStartColumn :: !Integer
    ,   blockPosEndLine :: !Integer
    ,   blockPosEndColumn :: !Integer
    } deriving (Show, Eq)

-- | Worst-case execution path block
data WCEPathBlock a
    = 
    -- | If block
    WCEPathCondIf
        [WCEPathBlock a] -- ^ blocks in the if block
        BlockPosition
        a
    -- | Else-if block
    | WCEPathCondElseIf
        [WCEPathBlock a] -- ^ blocks in the else-if block
        BlockPosition
        a
    -- | Else block
    | WCEPathCondElse
        [WCEPathBlock a] -- ^ blocks in the else block
        BlockPosition
        a
    -- | For-loop basic block
    | WCEPathForLoop
        (ConstExpression a) -- ^ initial value of the iterator
        (ConstExpression a) -- ^ final value of the iterator
        [WCEPathBlock a] -- ^ blocks in the for loop body.
        BlockPosition
        a
    -- | Match case block
    | WCEPathMatchCase
        [WCEPathBlock a] -- ^ blocks in the case block
        BlockPosition
        a
    -- | Send message
    | WCEPSendMessage 
        Identifier -- ^ Port name
        BlockPosition
        a
    | WCEPathMemberFunctionCall 
        Identifier -- ^ Function name
        [ConstExpression a] -- ^ Constant argument expressions
        BlockPosition
        a
    -- | Invoke a resource procedure
    | WCEPProcedureInvoke 
        Identifier -- ^ Port name
        Identifier -- ^ Procedure name 
        [ConstExpression a] -- ^ Constant argument expression
        BlockPosition
        a
    | WCEPAllocBox 
        Identifier -- ^ Port name
        BlockPosition
        a
    -- | Call to the free procedure of a memory allocator 
    | WCEPFreeBox 
        Identifier -- ^ Port name
        BlockPosition
        a
    -- | Regular block (list of statements)
    | WCEPRegularBlock BlockPosition a
    | WCEPReturn BlockPosition a
    | WCEPContinue 
        Identifier -- ^ Action name
        BlockPosition
        a
    | WCEPReboot BlockPosition a
    -- | System call
    | WCEPSystemCall 
        Identifier
        [ConstExpression a] -- ^ Constant argument expression
        BlockPosition
        a
    deriving Show

data TransactionalWCEPath a
    = TransactionalWCEPath 
        Identifier -- ^ task/resource name
        Identifier -- ^ action/procedure name
        Identifier -- ^ path name
        [Identifier] -- ^ constant parameters
        [WCEPathBlock a]
        a
    deriving Show

----------------------------------------
-- Termina Programs definitions

newtype PathModuleImport = ModuleImport' [String]
    deriving Show

data PathModule a = PathModule PathModuleImport [TransactionalWCEPath a]
    deriving Show

type TransPathsMap a = M.Map Identifier [TransactionalWCEPath a]