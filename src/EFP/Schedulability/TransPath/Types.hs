module EFP.Schedulability.TransPath.Types where
import EFP.Schedulability.TransPath.AST
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data TRPSemAnn = 
    TRPExprTy ConstExprType
    | TRPBlockTy
    | TRPOperationTy (S.Set (Identifier, Identifier, Identifier))
    | TRPTransactionsPathTy
    deriving Show

type TransPathMap a = M.Map Identifier (TransactionPath a)
