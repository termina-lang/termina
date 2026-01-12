module EFP.Schedulability.TransPath.Types where
import EFP.Schedulability.TransPath.AST
import qualified Data.Map.Strict as M
import ControlFlow.Architecture.Types

data TRPSemAnn = 
    TRPExprTy ConstExprType
    | TRPBlockRegularTy
    | TRPBlockAccessTy ResourceLock
    | TRPOperationTy
    | TRPTransactionsPathTy
    deriving Show

type TransPathMap a = M.Map Identifier (TransactionPath a)
