module EFP.Schedulability.TransPath.Types where
import EFP.Schedulability.TransPath.AST
import qualified Data.Map.Strict as M

data TRPSemAnn = 
    TRPExprTy ConstExprType
    | TRPBlockTy
    | TRPOperationTy
    | TRPTransactionsPathTy
    deriving Show

type TransPathMap a = M.Map Identifier (TransactionPath a)
