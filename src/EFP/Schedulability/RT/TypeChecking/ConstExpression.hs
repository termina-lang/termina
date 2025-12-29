{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.RT.TypeChecking.ConstExpression where
import EFP.Schedulability.Core.AST
import EFP.Schedulability.Core.Types
import EFP.Schedulability.RT.Monad
import EFP.Schedulability.RT.Types
import Utils.Annotations
import Control.Monad.Except
import EFP.Schedulability.RT.Errors
import Control.Monad


typeConstExpression :: ConstExpression ParserAnn -> RTMonad (ConstExpression RTSemAnn)
typeConstExpression (ConstInt i ann) = 
    return $ ConstInt i (RTExprTy TConstInt (getLocation ann))
typeConstExpression (ConstDouble d ann) = 
    return $ ConstDouble d (RTExprTy TConstDouble (getLocation ann))
typeConstExpression (ConstObject ident ann) = do
    -- | We do not support constants in these models (yet), so throw an error
    throwError . annotateError (getLocation ann) $ EUnknownConstant ident
typeConstExpression (ConstBinOp op left right ann) = do
    left' <- typeConstExpression left
    right' <- typeConstExpression right
    case (getAnnotation left', getAnnotation right') of
        (RTExprTy t1 _, RTExprTy t2 _) -> do
            unless (t1 == t2) $ 
                throwError . annotateError (getLocation ann) $ EConstExpressionTypeMismatch t1 t2
            return $ ConstBinOp op left' right' (RTExprTy t1 (getLocation ann))
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperandTypes