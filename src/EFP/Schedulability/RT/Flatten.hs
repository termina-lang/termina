module EFP.Schedulability.RT.Flatten (runFlattenTransaction) where

import EFP.Schedulability.RT.AST
import EFP.Schedulability.RT.Monad
import Control.Monad
import EFP.Schedulability.RT.Types
import Utils.Annotations
import Control.Monad.Except
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.Utils

flattenTransaction :: RTElement RTSemAnn -> RTFlatMonad (RTElement RTSemAnn)
flattenTransaction (RTTransaction ident step ann) = flip (RTTransaction ident) ann <$> flattenStep step

    where

    checkConditionalExprRange :: Location -> Location -> ConstExpression RTSemAnn -> RTFlatMonad ()
    checkConditionalExprRange loc innerLoc expr = do
        case expr of
            ConstInt (TInteger v _) _ ->
                unless (v > 0 && v <= 100) $
                    throwError . annotateError loc $ EFlatConditionalExpressionOutOfRange v innerLoc
            _ -> throwError . annotateError Internal $ EInvalidConditionalExpressionType
    
    flattenStep :: RTTransStep RTSemAnn -> RTFlatMonad (RTTransStep RTSemAnn)
    flattenStep st@(RTTransStepAction _name _task _action _path Nothing _ann) = return st
    flattenStep (RTTransStepAction name task action path (Just next) ann') = do
        flatNext <- flattenStep next
        case flatNext of
            RTTransStepConditional conds _ -> 
                flip RTTransStepConditional ann' <$> mapM (\(condExpr, condStep) ->
                        return (condExpr, RTTransStepAction name task action path (Just condStep) ann')) conds
            _ -> return $ RTTransStepAction name task action path (Just flatNext) ann
    -- | If the next step is a conditional, we need to flatten each branch of the conditional
    flattenStep (RTTransStepConditional conds ann') = do
        flatConds <- forM conds $ \(condExpr, condStep) -> do
            let loc = getLocation . getAnnotation $ condExpr
            flatCondStep <- flattenStep condStep
            -- | In the original conditional, the conditional step may never be
            -- another conditional.  However, after flattening, it may be. In
            -- that case, we need to adjust the conditional expressions
            -- accordingly.
            case flatCondStep of
                RTTransStepConditional innerConds _ ->
                    -- | For each inner conditional, we need to create a new
                    -- conditional expression that is the product of the outer
                    -- and inner conditional expressions, divided by 100.
                    forM innerConds $ \(innerCondExpr, innerCondStep) -> do
                        let innerLoc = getLocation . getAnnotation $ innerCondExpr
                        newCondExpr <- evalConstExpression 
                            (ConstBinOp Division 
                                (ConstBinOp Multiplication condExpr innerCondExpr ann') 
                                (ConstInt (TInteger 100 DecRepr) ann') ann')
                        checkConditionalExprRange loc innerLoc newCondExpr
                        return (newCondExpr, innerCondStep)
                _ -> return [(condExpr, flatCondStep)]
        return $ RTTransStepConditional (concat flatConds) ann
    flattenStep (RTTransStepMuticast steps ann') = do
        -- | Flatten each step in the multicast
        flatSteps <- forM steps flattenStep
        -- | If any of the flattened steps is a conditional, we need to
        -- distribute the multicast over the conditional.
        let hasConditional = any isConditional flatSteps
        if hasConditional then do
            -- | Instead of a multicast of conditional steps, we need to create a
            -- conditional step where each branch is a multicast of the steps
            -- corresponding to that branch.
            let conditionalStepsList = map (\step' -> case step' of
                    RTTransStepConditional conds _ -> conds
                    RTTransStepMuticast {} -> error "Unexpected multicast inside multicast flattening"
                    _ -> [(ConstInt (TInteger 100 DecRepr) (getAnnotation step'), step)]) flatSteps
            -- | Now, for each combination of conditional branches, create a new multicast step
            -- We use sequence to get all combinations
            let flatConds = sequence conditionalStepsList
            newConds <- forM flatConds $ \fcnds -> do
                resultingCondExpr <- foldM (\acc (condExpr, _) -> 
                        -- | Multiply the conditional expressions and divide by 100
                        return $ ConstBinOp Division 
                                        (ConstBinOp Multiplication condExpr acc (getAnnotation condExpr)) 
                                        (ConstInt (TInteger 100 DecRepr) (RTExprTy TConstInt Internal)) 
                                        (getAnnotation condExpr)) 
                    (ConstInt (TInteger 100 DecRepr) (RTExprTy TConstInt Internal)) fcnds 
                checkConditionalExprRange (getLocation ann') (getLocation . getAnnotation $ resultingCondExpr) resultingCondExpr
                return (resultingCondExpr, RTTransStepMuticast (map snd fcnds) ann')
            return $ RTTransStepConditional newConds ann
        else
            return $ RTTransStepMuticast flatSteps ann
flattenTransaction _ = throwError . annotateError Internal $ EInvalidFlatteningTarget

runFlattenTransaction :: RTElement RTSemAnn -> Either RTErrors (RTElement RTSemAnn)
runFlattenTransaction transaction =
    runExcept (flattenTransaction transaction) 