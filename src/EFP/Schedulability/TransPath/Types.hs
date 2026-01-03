module EFP.Schedulability.TransPath.Types where
import EFP.Schedulability.TransPath.AST

data TRPSemAnn = 
    TRPExprTy ConstExprType
    | TRPBlockTy
    | TRPActivityTy
    | TRPTransactionsPathTy
    deriving Show
