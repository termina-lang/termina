module EFP.Schedulability.TransPath.Utils where
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.Monad
import Control.Monad.Except
import Utils.Annotations
import Control.Monad.State
import qualified Data.Map as M
import EFP.Schedulability.TransPath.Types
import Data.Bits
import EFP.Schedulability.TransPath.Errors

evalBinOp :: 
    Op 
    -> ConstExpression TRPSemAnn 
    -> ConstExpression TRPSemAnn 
    -> TRPGenMonad (ConstExpression TRPSemAnn)
evalBinOp op (ConstInt (TInteger v1 repr1) _) (ConstInt (TInteger v2 _repr2) _) = 
    case op of
        Addition -> return $ ConstInt (TInteger (v1 + v2) repr1) (TRPExprTy TConstInt)
        Subtraction -> return $ ConstInt (TInteger (v1 - v2) repr1) (TRPExprTy TConstInt)
        Multiplication -> return $ ConstInt (TInteger (v1 * v2) repr1) (TRPExprTy TConstInt)
        Division -> 
            if v2 == 0 then
                throwError . annotateError Internal $ EConstExpressionDivisionByZero
            else
                return $ ConstInt (TInteger (v1 `div` v2) repr1) (TRPExprTy TConstInt)
        Modulo -> 
            if v2 == 0 then
                throwError . annotateError Internal $ EConstExpressionDivisionByZero
            else
                return $ ConstInt (TInteger (v1 `mod` v2) repr1) (TRPExprTy TConstInt)
        BitwiseLeftShift ->
            return $ ConstInt (TInteger (v1 `shiftL` fromIntegral v2) repr1) (TRPExprTy TConstInt)
        BitwiseRightShift ->
            return $ ConstInt (TInteger (v1 `shiftR` fromIntegral v2) repr1) (TRPExprTy TConstInt)
        BitwiseAnd ->
            return $ ConstInt (TInteger (v1 .&. v2) repr1) (TRPExprTy TConstInt)
        BitwiseOr ->
            return $ ConstInt (TInteger (v1 .|. v2) repr1) (TRPExprTy TConstInt)
        BitwiseXor ->
            return $ ConstInt (TInteger (v1 `xor` v2) repr1) (TRPExprTy TConstInt)
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperand op
-- | Both operands are floating-point
evalBinOp op (ConstDouble v1 _) (ConstDouble v2 _) = 
    case op of
        Addition -> return $ ConstDouble (v1 + v2) (TRPExprTy TConstDouble)
        Subtraction -> return $ ConstDouble (v1 - v2) (TRPExprTy TConstDouble)
        Multiplication -> return $ ConstDouble (v1 * v2) (TRPExprTy TConstDouble)
        Division -> 
            if v2 == 0 then
                throwError . annotateError Internal $ EConstExpressionDivisionByZero
            else
                return $ ConstDouble (v1 / v2) (TRPExprTy TConstDouble)
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperand op
-- | Mismatched operand types
evalBinOp op (ConstDouble v1 _) (ConstInt (TInteger v2 _repr2) _) =
    case op of
        Addition -> return $ ConstDouble (v1 + fromInteger v2) (TRPExprTy TConstDouble)
        Subtraction -> return $ ConstDouble (v1 - fromInteger v2) (TRPExprTy TConstDouble)
        Multiplication -> return $ ConstDouble (v1 * fromInteger v2) (TRPExprTy TConstDouble)
        Division -> 
            if v2 == 0 then
                throwError . annotateError Internal $ EConstExpressionDivisionByZero
            else
                return $ ConstDouble (v1 / fromInteger v2) (TRPExprTy TConstDouble)
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperand op
evalBinOp op (ConstInt (TInteger v1 _repr1) _) (ConstDouble v2 _) =
    case op of
        Addition -> return $ ConstDouble (fromInteger v1 + v2) (TRPExprTy TConstDouble)
        Subtraction -> return $ ConstDouble (fromInteger v1 - v2) (TRPExprTy TConstDouble)
        Multiplication -> return $ ConstDouble (fromInteger v1 * v2) (TRPExprTy TConstDouble)
        Division -> 
            if v2 == 0 then
                throwError . annotateError Internal $ EConstExpressionDivisionByZero
            else
                return $ ConstDouble (fromInteger v1 / v2) (TRPExprTy TConstDouble)
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperand op
evalBinOp _ _ _ = 
    throwError . annotateError Internal $ EInvalidConstExpressionOperandTypes

-- | Evaluate a constant expression
evalConstExpression :: 
    ConstExpression a -> TRPGenMonad (ConstExpression TRPSemAnn)
evalConstExpression (ConstInt val _) = return $ ConstInt val (TRPExprTy TConstInt)
evalConstExpression (ConstDouble val _) = return $ ConstDouble val (TRPExprTy TConstDouble)
evalConstExpression (ConstObject ident _ann) = do
    env <- gets localConstEnv
    case M.lookup ident env of
        Just val -> return val
        Nothing -> throwError . annotateError Internal $ EUnknownConstant ident
evalConstExpression (ConstBinOp op left right _ann) = do
    -- | Evaluate left and right expressions first
    left' <- evalConstExpression left
    right' <- evalConstExpression right
    -- | Now perform the operation
    evalBinOp op left' right'

passArguments :: 
    [Identifier] 
    -> [ConstExpression TRPSemAnn] -> TRPGenMonad ()
passArguments [] [] = return ()
passArguments (argName:argNames) (argValue:argValues) = do
    modify $ \st -> 
        st { localConstEnv = M.insert argName argValue (localConstEnv st) }
    passArguments argNames argValues
passArguments _ _ = throwError . annotateError Internal $ EInvalidArgumentPassing