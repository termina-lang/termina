module EFP.Schedulability.TransPath.AST where

import Core.AST

data BlockPosition = BlockPosition
    {
        blockPosStartLine :: !Integer
    ,   blockPosStartColumn :: !Integer
    ,   blockPosEndLine :: !Integer
    ,   blockPosEndColumn :: !Integer
    } deriving (Show, Eq)

data WCEPConstExpression
    = WCEPConstInt TInteger
    | WCEPConstObject Identifier
    | WCEPConstBinOp Op WCEPConstExpression WCEPConstExpression
    deriving Show

-- | Worst-case execution path block
data WCEPathBlock
    = 
    -- | If block
    WCEPathCondIf
        [WCEPathBlock] -- ^ blocks in the if block
        BlockPosition
    -- | Else-if block
    | WCEPathCondElseIf
        [WCEPathBlock] -- ^ blocks in the else-if block
        BlockPosition
    -- | Else block
    | WCEPathCondElse
        [WCEPathBlock] -- ^ blocks in the else block
        BlockPosition
    -- | For-loop basic block
    | WCEPathForLoop
        WCEPConstExpression -- ^ initial value of the iterator
        WCEPConstExpression -- ^ final value of the iterator
        [WCEPathBlock] -- ^ blocks in the for loop body.
        BlockPosition
    -- | Match case block
    | WCEPathMatchCase
        [WCEPathBlock] -- ^ blocks in the case block
        BlockPosition
    -- | Send message
    | WCEPSendMessage 
        Identifier -- ^ Port name
        BlockPosition
    | WCEPathMemberFunctionCall 
        Identifier -- ^ Function name
        [WCEPConstExpression] -- ^ Constant argument expressions
        BlockPosition
    -- | Invoke a resource procedure
    | WCEPProcedureInvoke 
        Identifier -- ^ Port name
        Identifier -- ^ Procedure name 
        [WCEPConstExpression] -- ^ Constant argument expression
        BlockPosition
    | WCEPAllocBox 
        Identifier -- ^ Port name
        BlockPosition
    -- | Call to the free procedure of a memory allocator 
    | WCEPFreeBox 
        Identifier -- ^ Port name
        BlockPosition
    -- | Regular block (list of statements)
    | WCEPRegularBlock BlockPosition
    | WCEPReturn BlockPosition
    | WCEPContinue 
        Identifier -- ^ Action name
        BlockPosition
    | WCEPReboot BlockPosition
    -- | System call
    | WCEPSystemCall Identifier BlockPosition
    deriving Show

data TransactionalWCEPath
    = TransactionalWCEPath 
        Identifier -- ^ task/resource name
        Identifier -- ^ action/procedure name
        Identifier -- ^ path name
        [Identifier] -- ^ constant parameters
        [WCEPathBlock]
    deriving Show

----------------------------------------
-- Termina Programs definitions

newtype PathModuleImport = ModuleImport' [String]
    deriving Show

data PathModule a = PathModule PathModuleImport [TransactionalWCEPath]
    deriving Show
