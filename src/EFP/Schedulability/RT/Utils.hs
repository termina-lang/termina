{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.RT.Utils where
import EFP.Schedulability.RT.Semantic.AST
import EFP.Schedulability.RT.Semantic.Types
import Utils.Annotations
import Control.Monad ()
import Control.Monad.Except
import EFP.Schedulability.RT.Errors
import Data.Bits

isConditional :: RTTransStep RTSemAnn -> Bool
isConditional (RTTransStepConditional _ _) = True
isConditional _ = False

isIntegerExpr :: ConstExpression RTSemAnn -> Bool
isIntegerExpr e = case getAnnotation e of
    RTExprTy TConstInt _ -> True
    _ -> False

evalBinOp :: (MonadError RTErrors m) => 
    RTSemAnn 
    -> Op 
    -> ConstExpression RTSemAnn 
    -> ConstExpression RTSemAnn 
    -> m (ConstExpression RTSemAnn)
evalBinOp ann op (ConstInt (TInteger v1 repr1) _) (ConstInt (TInteger v2 _repr2) _) = 
    case op of
        Addition -> return $ ConstInt (TInteger (v1 + v2) repr1) ann
        Subtraction -> return $ ConstInt (TInteger (v1 - v2) repr1) ann
        Multiplication -> return $ ConstInt (TInteger (v1 * v2) repr1) ann
        Division -> 
            if v2 == 0 then
                throwError . annotateError (getLocation ann) $ EConstExpressionDivisionByZero
            else
                return $ ConstInt (TInteger (v1 `div` v2) repr1) ann
        Modulo -> 
            if v2 == 0 then
                throwError . annotateError (getLocation ann) $ EConstExpressionDivisionByZero
            else
                return $ ConstInt (TInteger (v1 `mod` v2) repr1) ann
        BitwiseLeftShift ->
            return $ ConstInt (TInteger (v1 `shiftL` fromIntegral v2) repr1) ann
        BitwiseRightShift ->
            return $ ConstInt (TInteger (v1 `shiftR` fromIntegral v2) repr1) ann
        BitwiseAnd ->
            return $ ConstInt (TInteger (v1 .&. v2) repr1) ann
        BitwiseOr ->
            return $ ConstInt (TInteger (v1 .|. v2) repr1) ann
        BitwiseXor ->
            return $ ConstInt (TInteger (v1 `xor` v2) repr1) ann
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperand op
-- | Both operands are floating-point
evalBinOp ann op (ConstDouble v1 _) (ConstDouble v2 _) = 
    case op of
        Addition -> return $ ConstDouble (v1 + v2) ann
        Subtraction -> return $ ConstDouble (v1 - v2) ann
        Multiplication -> return $ ConstDouble (v1 * v2) ann
        Division -> 
            if v2 == 0 then
                throwError . annotateError (getLocation ann) $ EConstExpressionDivisionByZero
            else
                return $ ConstDouble (v1 / v2) ann
        _ -> throwError . annotateError (getLocation ann) $ EInvalidConstExpressionOperand op
-- | Mismatched operand types
evalBinOp ann _ left right =
    case (getAnnotation left, getAnnotation right) of
        (RTExprTy t1 _, RTExprTy t2 _) -> 
            throwError . annotateError (getLocation ann) $ EConstExpressionTypeMismatch t1 t2
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperandTypes

-- | Evaluate a constant expression
evalConstExpression :: (MonadError RTErrors m) 
    => ConstExpression RTSemAnn -> m (ConstExpression RTSemAnn)
evalConstExpression c@(ConstInt {}) = return c
evalConstExpression c@(ConstDouble {}) = return c
evalConstExpression (ConstObject ident ann) = 
    -- | We do not support constants in these models (yet), so throw an error
    throwError . annotateError (getLocation ann) $ EUnknownConstant ident
evalConstExpression (ConstBinOp op left right ann) = do
    -- | Evaluate left and right expressions first
    left' <- evalConstExpression left
    right' <- evalConstExpression right
    -- | Now perform the operation
    evalBinOp ann op left' right'
