module EFP.Schedulability.RT.Flatten (runFlattenModule) where

import EFP.Schedulability.RT.AST
import EFP.Schedulability.RT.Monad
import Control.Monad
import EFP.Schedulability.RT.Types
import Utils.Annotations
import Control.Monad.Except
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.Utils
import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M
import ControlFlow.Architecture.Types
import Semantic.Types
import EFP.Schedulability.WCEPath.Types

flattenElement :: RTElement RTSemAnn -> RTMonad ()
flattenElement (RTTransaction ident step ann) = do
    fStep <- flattenStep step
    ST.modify $
        \s -> s { transactions = M.insert ident (RTTransaction ident fStep ann) (transactions s) }

    where

    checkConditionalExprRange :: Location -> Location -> ConstExpression RTSemAnn -> RTMonad ()
    checkConditionalExprRange loc innerLoc expr = do
        case expr of
            ConstInt (TInteger v _) _ ->
                unless (v > 0 && v <= 100) $
                    throwError . annotateError loc $ EFlatConditionalExpressionOutOfRange v innerLoc
            _ -> throwError . annotateError Internal $ EInvalidConditionalExpressionType
    
    flattenStep :: RTTransStep RTSemAnn -> RTMonad (RTTransStep RTSemAnn)
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
flattenElement (RTSituation sid _ (RTSitTy evMap loc)) = do
    let periodicEvents = [p |p@(_, RTEventPeriodic {}) <- M.toList evMap]
        interruptEvMap = M.filter (\case RTEventInterrupt {} -> True; _ -> False) evMap

    let sitInit = flip ConstStructInitializer (RTStructInitTy Internal) 
            $ map genTypedStructInitializer (M.toList interruptEvMap)
    let situationIrqEvents = RTSituation sid sitInit (RTSitTy interruptEvMap loc)
    newSituations <- foldM flattenPeriodicEvent [situationIrqEvents] periodicEvents 
    forM_ newSituations $ \case
            RTSituation sitName sitInit' (RTSitTy evMap' ann') ->
                    ST.modify $ \s -> s { situations = M.insert sitName (RTSituation sitName sitInit' (RTSitTy evMap' ann')) (situations s) }
            _ -> throwError . annotateError Internal $ EInvalidRTElementDefinition

    where

        flattenPeriodicEvent :: [RTElement RTSemAnn] -> (Identifier, RTEvent RTSemAnn) -> RTMonad [RTElement RTSemAnn]
        flattenPeriodicEvent _ (_, RTEventInterrupt {}) = throwError . annotateError Internal $ EInvalidEventDefinitionType
        flattenPeriodicEvent prevSituations (eventId, RTEventPeriodic transactionName deadlines ann) = do
            trMap <- ST.gets transactions
            case M.lookup transactionName trMap of
                Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransaction transactionName
                Just (RTSituation {}) -> throwError . annotateError (getLocation ann) $ EInvalidRTElementDefinition
                Just (RTTransaction _ (RTTransStepAction {}) _) -> 
                    forM prevSituations $ \case
                        (RTSituation prevSituationName _ (RTSitTy evMap' ann'')) ->
                            let newSituationName = prevSituationName ++ transactionName 
                                newEvMap = M.insert eventId (RTEventPeriodic transactionName deadlines ann) evMap'
                                sitInit' = flip ConstStructInitializer (RTStructInitTy Internal) 
                                            $ map genTypedStructInitializer (M.toList newEvMap) in
                            return $ RTSituation newSituationName sitInit' (RTSitTy newEvMap ann'')
                        _ -> throwError . annotateError Internal $ EInvalidRTElementDefinition
                Just (RTTransaction _ (RTTransStepConditional conds _) ann') -> do
                    -- | We need to eliminate the transaction from the map, because
                    -- we are going to create a new transaction for each branch of the
                    -- conditional
                    ST.modify $ \s -> s { transactions = M.delete transactionName (transactions s) }
                    -- | We need to generate a new event for each branch of the conditional
                    concat <$> zipWithM (\(_condExpr, condStep) idx -> do
                        case condStep of
                            RTTransStepAction {} -> do
                                let newTransactionName = transactionName ++ "__cond_" ++ show idx
                                let newTransaction = RTTransaction newTransactionName condStep ann'
                                ST.modify $ \s -> s { transactions = M.insert newTransactionName newTransaction (transactions s) }
                                let newPeriodicEvent = RTEventPeriodic newTransactionName deadlines ann
                                forM prevSituations $ \case
                                    (RTSituation prevSituationName _ (RTSitTy evMap' ann'')) ->
                                        let newSituationName = prevSituationName ++ "__" ++ newTransactionName 
                                            newEvMap = M.insert eventId newPeriodicEvent evMap'
                                            sitInit' = flip ConstStructInitializer (RTStructInitTy Internal) 
                                                $ map genTypedStructInitializer (M.toList newEvMap) in
                                        return $ RTSituation newSituationName sitInit' (RTSitTy newEvMap ann'')
                                    _ -> throwError . annotateError Internal $ EInvalidRTElementDefinition
                            _ -> throwError . annotateError (getLocation ann) $ EInvalidRTElementDefinition) conds [(0 :: Integer) ..]
                Just (RTTransaction {}) -> throwError . annotateError (getLocation ann) $ EInvalidTransaction

flattenElement (RTSituation {}) = throwError . annotateError Internal $ EInvalidSituationAnnotation

runFlattenModule :: TerminaProgArch SemanticAnn
    -> WCEPathMap WCEPSemAnn
    -> [RTElement RTSemAnn] -> Either RTErrors (RTTransactionMap RTSemAnn, RTSituationMap RTSemAnn)
runFlattenModule arch transPath elements =
    let initialState = RTState arch transPath M.empty M.empty M.empty in
    case ST.runState (runExceptT (mapM_ flattenElement elements)) initialState of
        (Left err, _) -> Left err
        (_, st) -> Right (transactions st, situations st)