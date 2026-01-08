module EFP.Schedulability.MAST.Generator where

import EFP.Schedulability.MAST.AST
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.Types
import EFP.Schedulability.MAST.Monad
import qualified Data.Set as S
import Control.Monad


getResourceAccess :: S.Set Identifier -> TransPathBlock a -> MASTGenMonad (S.Set Identifier)
getResourceAccess acc (TPBlockCondIf blks _ _) = foldM getResourceAccess acc blks
getResourceAccess acc (TPBlockCondElseIf blks _ _) = foldM getResourceAccess acc blks
getResourceAccess acc (TPBlockCondElse blks _ _) = foldM getResourceAccess acc blks
getResourceAccess acc (TPBlockForLoop _ blks _ _) = foldM getResourceAccess acc blks
getResourceAccess acc (TPBlockMatchCase blks _ _) = foldM getResourceAccess acc blks
getResourceAccess _acc (TPBlockMemberFunctionCall {}) = undefined
getResourceAccess _acc (TPBlockProcedureInvoke {}) = undefined
getResourceAccess _acc (TPBlockAllocBox {}) = undefined
getResourceAccess _acc (TPBlockFreeBox {}) = undefined
getResourceAccess _acc (TPBlockSystemCall {}) = undefined
getResourceAccess acc _ = return acc

getMASTOperation :: Identifier -> TRPOperation TRPSemAnn -> MASTGenMonad MASTOperation
getMASTOperation _prefix (TRPTaskOperation {}) = undefined
getMASTOperation _prefix (TRPHandlerOperation {}) = undefined
getMASTOperation _prefix (TRPResourceOperation {}) = undefined