module EFP.Schedulability.RT.TypeChecking.Situation where

import qualified Data.Map.Strict as M

import EFP.Schedulability.RT.Semantic.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.Parser.AST
import qualified EFP.Schedulability.RT.Semantic.AST as SAST
import Control.Monad
import qualified Data.Set as S
import EFP.Schedulability.Core.Types
import EFP.Schedulability.RT.Monad
import EFP.Schedulability.RT.Utils
import EFP.Schedulability.RT.TypeChecking.ConstExpression


typeEventDefinition :: SAST.RTEventMap RTSemAnn -> RTEvent ParserAnn -> RTMonad (SAST.RTEventMap RTSemAnn)
typeEventDefinition acc (RTEventBursty eventId evInit ann') = do
    -- | Check that the field has not been initialized before
    case M.lookup eventId acc of
        Just prev -> throwError . annotateError (getLocation ann') $ EDuplicateEventDefinition eventId (getLocation . getAnnotation $ prev)
        Nothing -> return ()
    -- | Search for the emitter in the architecture
    typeBurstyEvent eventId evInit >>= \typedEv ->
        return $ M.insert eventId typedEv acc
typeEventDefinition acc (RTEventPeriodic eventId evInit ann') = do
    -- | Check that the field has not been initialized before
    case M.lookup eventId acc of
        Just prev -> throwError . annotateError (getLocation ann') $ EDuplicateEventDefinition eventId (getLocation . getAnnotation $ prev)
        Nothing -> return ()
    -- | Search for the emitter in the architecture
    typePeriodicEvent eventId evInit >>= \typedEv ->
        return $ M.insert eventId typedEv acc

-- | Type transaction object expression
-- It must refer to a valid transaction identifier
typeTransactionObjectExpr :: ConstExpression ParserAnn -> RTMonad (SAST.RTElement RTSemAnn)
typeTransactionObjectExpr (ConstObject ident ann') = do
    trans <- ST.gets transactions
    case M.lookup ident trans of
        Just tr -> return tr
        Nothing -> throwError . annotateError (getLocation ann') $ EUnknownTransaction ident
typeTransactionObjectExpr expr =
    throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidTransactionFieldType

typeEmitterObjectExpr :: ConstExpression ParserAnn -> RTMonad Identifier
typeEmitterObjectExpr (ConstObject ident ann') = do
    arch <- ST.gets progArch
    if M.member ident (emitters arch) then
        return ident
    else
        throwError . annotateError (getLocation ann') $ EUnknownEventEmitter ident
typeEmitterObjectExpr expr =
    throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidEmitterFieldType

-- | This function recursively extracts all step names from a transaction step
extractStepNames :: S.Set Identifier -> SAST.RTTransStep RTSemAnn -> S.Set Identifier
extractStepNames acc (SAST.RTTransStepEnd stepName _) =
    S.insert stepName acc
extractStepNames acc (SAST.RTTransStepAction stepName _ _ _ nextStep _) =
    let acc' = S.insert stepName acc in
    extractStepNames acc' nextStep
extractStepNames acc (SAST.RTTransStepMuticast transSteps _) =
    foldl extractStepNames acc transSteps
extractStepNames acc (SAST.RTTransStepConditional condSteps _) =
    foldl (\a (_, step) -> extractStepNames a step) acc condSteps

typeDeadline :: ConstExpression RTSemAnn -> RTMonad Double
typeDeadline (ConstDouble deadline ann) = do
    when (deadline <= 0.0) $ throwError . annotateError (getLocation ann) $ EInvalidDeadlineValue deadline
    return deadline
typeDeadline (ConstInt (TInteger intVal _) ann) = do
    let deadline = fromIntegral intVal
    when (deadline <= 0.0) $ throwError . annotateError (getLocation ann) $ EInvalidDeadlineValue deadline
    return deadline
typeDeadline expr = throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidDeadlineExpression

typeDeadlinesMap :: Identifier -> ConstStructInitializer ParserAnn -> RTMonad (SAST.RTDeadlineMap RTSemAnn)
typeDeadlinesMap transactionId (ConstStructInitializer fields ann) = do
    trans <- ST.gets transactions
    -- | Get all the step identifiers in the transaction
    transaction <- case M.lookup transactionId trans of
        Just t -> return t
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransaction transactionId
    steps <- case transaction of
        SAST.RTTransaction _ firstStep _ -> return $ extractStepNames S.empty firstStep
        SAST.RTSituation {} -> throwError . annotateError Internal $ EInvalidTransactionMap
    deadlines <- mapM (\(ConstFieldAssignment fname fvalue ann') -> do
        -- Check that the name is a valid step name in the transaction
        unless (S.member fname steps) $
            throwError . annotateError (getLocation ann') $ EUnknownTransactionStep fname (transactionId, getLocation . getAnnotation $ transaction)
        case fvalue of
            ConstStructSimpleValue expr -> do
                deadline <- typeConstExpression expr >>= evalConstExpression >>= typeDeadline
                return (fname, deadline)
            ConstStructFieldValue _ ->
                throwError . annotateError (getLocation ann') $ EInvalidDeadlineFieldType) fields
    return $ M.fromList deadlines

checkFieldNames :: [ConstFieldAssignment ParserAnn] -> S.Set Identifier -> RTMonad (M.Map String (ConstFieldAssignment ParserAnn))
checkFieldNames fields' validNames = do
    foldM (\acc fa@(ConstFieldAssignment fname _ ann') -> do
        case M.lookup fname acc of
            Just prev ->
                throwError . annotateError (getLocation ann') $ EDuplicateEventField fname (getLocation . cfaAnnotation $ prev)
            Nothing ->
                return ()
        if fname `S.member` validNames then
            return $ M.insert fname fa acc
        else
            throwError . annotateError (getLocation ann') $ EInvalidEventField fname (S.toList validNames)) M.empty fields'

checkEmitterConnection :: Location -> Identifier -> Identifier -> Identifier -> RTMonad ()
checkEmitterConnection loc emitterId targetCmp targetAction = do
    arch <- ST.gets progArch
    case M.lookup emitterId (emitterTargets arch) of
        Nothing -> throwError . annotateError loc $ EUnknownEventEmitter emitterId
        Just (cmp, port, _) -> do
            -- | If the component is not the target component, throw an error
            unless (cmp == targetCmp) $
                throwError . annotateError loc $ EEmitterTargetMismatch emitterId targetCmp cmp
            -- | Get the component class
            tpCls <- case M.lookup cmp (tasks arch) of
                Just tsk -> case M.lookup (taskClass tsk) (taskClasses arch) of
                    Just cls -> return cls
                    Nothing -> throwError . annotateError loc $ EInvalidTaskClass (taskClass tsk)
                Nothing -> case M.lookup cmp (handlers arch) of
                    Just hdl -> case M.lookup (handlerClass hdl) (handlerClasses arch) of
                        Just cls -> return cls
                        Nothing -> throwError . annotateError loc $ EInvalidHandlerClass (handlerClass hdl)
                    Nothing -> throwError . annotateError loc $ EUnknownComponent cmp
            -- | Obtain the target action from the sink port
            case M.lookup port (sinkPorts tpCls) of
                Nothing -> throwError . annotateError Internal $ EInvalidSinkPort port
                Just (_, act) ->
                    unless (act == targetAction) $
                        throwError . annotateError loc $ EEmitterActionMismatch emitterId targetCmp targetAction (port, act, getLocation . classAnns $ tpCls)
            return ()

typeIntervalField :: ConstExpression RTSemAnn -> RTMonad Double
typeIntervalField (ConstInt (TInteger intVal _) ann) = do
    when (intVal <= 0) $ throwError . annotateError (getLocation ann) $ EInvalidIntervalValue intVal 
    return $ fromIntegral intVal
typeIntervalField expr = throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidIntervalExpression

typeArrivalsField :: ConstExpression RTSemAnn -> RTMonad TInteger
typeArrivalsField (ConstInt i@(TInteger intVal _) ann) = do
    when (intVal <= 0) $ throwError . annotateError (getLocation ann) $ EInvalidArrivalsValue intVal
    return i
typeArrivalsField expr = throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidArrivalsExpression

typeBurstyEvent :: Identifier -> ConstStructInitializer ParserAnn -> RTMonad (SAST.RTEvent RTSemAnn)
typeBurstyEvent eventId (ConstStructInitializer fields' ann) = do
    let validNames = S.fromList ["interval", "arrivals", "deadlines", "transaction", "emitter"]
    -- | Check that all fields are valid and there are no duplicates
    fieldsMap <- checkFieldNames fields' validNames
    -- | Type interval field
    intervalExpr <- case M.lookup "interval" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) ->
            typeConstExpression expr >>= evalConstExpression >>= typeIntervalField
        Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "interval"
        Nothing -> throwError . annotateError (getLocation ann) $ EMissingEventField "interval"
    -- | Type arrivals field
    arrivalsExpr <- case M.lookup "arrivals" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) ->
            typeConstExpression expr >>= typeArrivalsField
        Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "arrivals"
        Nothing -> throwError . annotateError (getLocation ann) $ EMissingEventField "arrivals"
    -- | Type emitter field
    emitterId <- case M.lookup "emitter" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> do
            typeEmitterObjectExpr expr
        Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "emitter"
        Nothing -> throwError . annotateError (getLocation ann) $ EMissingEventField "emitter"
    -- | Type transactions field
    transactionId <- case M.lookup "transaction" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> do
            transaction <- typeTransactionObjectExpr expr
            case transaction of
                SAST.RTTransaction ident (SAST.RTTransStepAction _ targetCmp targetAction _ _ _) _ ->
                    checkEmitterConnection (getLocation ann) emitterId targetCmp targetAction
                    >> return ident
                SAST.RTTransaction {} -> throwError . annotateError Internal $ EInvalidTransaction
                SAST.RTSituation {} -> throwError . annotateError Internal $ EInvalidTransactionMap
        Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "transaction"
        Nothing -> throwError . annotateError (getLocation ann) $ EMissingEventField "transaction"
    -- | Type deadlines field
    deadlinesMap <- case M.lookup "deadlines" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructFieldValue structInit) _) ->
            typeDeadlinesMap transactionId structInit
        Just (ConstFieldAssignment _ (ConstStructSimpleValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "deadlines"
        Nothing ->
            throwError . annotateError (getLocation ann) $ EMissingEventField "deadlines"
    return $ SAST.RTEventBursty eventId emitterId transactionId intervalExpr arrivalsExpr deadlinesMap (RTEventTy (getLocation ann))

typePeriodicEvent :: Identifier -> ConstStructInitializer ParserAnn -> RTMonad (SAST.RTEvent RTSemAnn)
typePeriodicEvent eventId (ConstStructInitializer fields' ann) = do
    let validNames = S.fromList ["deadlines", "transaction", "emitter"]
    -- | Check that all fields are valid and there are no duplicates
    fieldsMap <- checkFieldNames fields' validNames
    -- | Type emitter field
    emitterId <- case M.lookup "emitter" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> do
            typeEmitterObjectExpr expr
        Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "emitter"
        Nothing -> throwError . annotateError (getLocation ann) $ EMissingEventField "emitter"
    -- | Type transactions field
    transactionId <- case M.lookup "transaction" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> do
            transaction <- typeTransactionObjectExpr expr
            case transaction of
                SAST.RTTransaction ident (SAST.RTTransStepAction _ targetCmp targetAction _ _ _) _ ->
                    checkEmitterConnection (getLocation ann) emitterId targetCmp targetAction
                    >> return ident
                SAST.RTTransaction {} -> throwError . annotateError Internal $ EInvalidTransaction
                SAST.RTSituation {} -> throwError . annotateError Internal $ EInvalidTransactionMap
        Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "transaction"
        Nothing -> throwError . annotateError (getLocation ann) $ EMissingEventField "transaction"
    -- | Type deadlines field
    deadlinesMap <- case M.lookup "deadlines" fieldsMap of
        Just (ConstFieldAssignment _ (ConstStructFieldValue structInit) _) ->
            typeDeadlinesMap transactionId structInit
        Just (ConstFieldAssignment _ (ConstStructSimpleValue _) _) ->
            throwError . annotateError (getLocation ann) $ EInvalidEventFieldType "deadlines"
        Nothing ->
            throwError . annotateError (getLocation ann) $ EMissingEventField "deadlines"
    return $ SAST.RTEventPeriodic eventId emitterId transactionId deadlinesMap (RTEventTy (getLocation ann))