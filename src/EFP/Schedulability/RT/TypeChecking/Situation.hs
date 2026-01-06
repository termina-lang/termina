module EFP.Schedulability.RT.TypeChecking.Situation where

import qualified Data.Map.Strict as M

import EFP.Schedulability.RT.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.AST
import Control.Monad
import qualified Data.Set as S
import EFP.Schedulability.Core.Types
import EFP.Schedulability.RT.Monad
import EFP.Schedulability.RT.TypeChecking.ConstExpression
import EFP.Schedulability.RT.Utils


typeSituationInitializer :: ConstStructInitializer ParserAnn -> RTMonad (RTEventMap RTSemAnn, ConstStructInitializer RTSemAnn)
typeSituationInitializer (ConstStructInitializer fields ann) = do
    evMap <- foldM typeEventDefinition M.empty fields 
    -- | Return the typed situation initializer
    -- We empty the fields as they are not needed anymore
    let typedSitInit = map genTypedStructInitializer (M.toList evMap)
    return (evMap, ConstStructInitializer typedSitInit (RTStructInitTy (getLocation ann)))

    where

    typeEventDefinition :: RTEventMap RTSemAnn -> ConstFieldAssignment ParserAnn -> RTMonad (RTEventMap RTSemAnn)
    typeEventDefinition acc (ConstFieldAssignment emitterId (ConstStructFieldValue evInit) ann') = do
        -- | Check that the field has not been initialized before
        case M.lookup emitterId acc of
            Just prev -> throwError . annotateError (getLocation ann') $ EDuplicateEventDefinition emitterId (getLocation . getAnnotation $ prev)
            Nothing -> return ()
        -- | Search for the emitter in the architecture
        arch <- ST.gets progArch
        case M.lookup emitterId (emitters arch) of
            Nothing -> throwError . annotateError (getLocation ann') $ EUnknownEventEmitter emitterId
            Just (TPInterruptEmittter {}) -> typeInterruptEvent emitterId evInit >>= \typedEv ->
                return $ M.insert emitterId typedEv acc
            Just (TPPeriodicTimerEmitter {}) -> typePeriodicEvent emitterId evInit >>= \typedEv ->
                return $ M.insert emitterId typedEv acc
            _ -> throwError . annotateError (getLocation ann') $ EUnsupportedEmitterType
    typeEventDefinition _acc (ConstFieldAssignment _ _ ann') = do
        throwError . annotateError (getLocation ann') $ EInvalidEventDefinitionType
    
    -- | Type transaction object expression
    -- It must refer to a valid transaction identifier
    typeTransactionObjectExpr :: ConstExpression ParserAnn -> RTMonad (RTElement RTSemAnn)
    typeTransactionObjectExpr (ConstObject ident ann') = do
        trans <- ST.gets transactions
        case M.lookup ident trans of
            Just tr -> return tr 
            Nothing -> throwError . annotateError (getLocation ann') $ EUnknownTransaction ident
    typeTransactionObjectExpr expr =
        throwError . annotateError (getLocation . getAnnotation $ expr) $ EInvalidTransactionFieldType

    -- | This function recursively extracts all step names from a transaction step
    extractStepNames :: S.Set Identifier -> RTTransStep RTSemAnn -> S.Set Identifier
    extractStepNames acc (RTTransStepAction stepName _ _ _ nextStep _) = 
        let acc' = S.insert stepName acc in
        case nextStep of
            Just next -> extractStepNames acc' next
            Nothing -> acc'
    extractStepNames acc (RTTransStepMuticast transSteps _) =
        foldl extractStepNames acc transSteps
    extractStepNames acc (RTTransStepConditional condSteps _) =
        foldl (\a (_, step) -> extractStepNames a step) acc condSteps
    
    typeDeadlinesMap :: Identifier -> ConstStructInitializer ParserAnn -> RTMonad RTDeadlineMap
    typeDeadlinesMap transactionId (ConstStructInitializer fields' ann') = do
        trans <- ST.gets transactions
        -- | Get all the step identifiers in the transaction
        transaction <- case M.lookup transactionId trans of
            Just t -> return t
            Nothing -> throwError . annotateError (getLocation ann') $ EUnknownTransaction transactionId
        steps <- case transaction of
            RTTransaction _ firstStep _ -> return $ extractStepNames S.empty firstStep
            RTSituation {} -> throwError . annotateError Internal $ EInvalidTransactionMap
        foldM (\acc (ConstFieldAssignment fname fvalue ann'') -> do
            -- Check that the name is a valid step name in the transaction
            unless (S.member fname steps) $
                throwError . annotateError (getLocation ann'') $ EUnknownTransactionStep fname (transactionId, getLocation . getAnnotation $ transaction)
            case fvalue of
                ConstStructSimpleValue expr -> do
                    typedExpr <- typeConstExpression expr
                    return $ M.insert fname typedExpr acc
                ConstStructFieldValue _ -> 
                    throwError . annotateError (getLocation ann'') $ EInvalidDeadlineFieldType) M.empty fields'

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
    
    checkEmitterConnection :: Identifier -> Identifier -> Identifier -> RTMonad ()
    checkEmitterConnection emitterId targetCmp targetAction = do
        arch <- ST.gets progArch
        case M.lookup emitterId (emitterTargets arch) of
            Nothing -> throwError . annotateError Internal $ EInvalidEventEmitter emitterId
            Just (cmp, port, _) -> do
                -- | If the component is not the target component, throw an error
                unless (cmp == targetCmp) $
                    throwError . annotateError (getLocation ann) $ EEmitterTargetMismatch emitterId targetCmp cmp
                -- | Get the component class
                tpCls <- case M.lookup cmp (tasks arch) of
                    Just tsk -> case M.lookup (taskClass tsk) (taskClasses arch) of
                        Just cls -> return cls
                        Nothing -> throwError . annotateError (getLocation ann) $ EInvalidTaskClass (taskClass tsk)
                    Nothing -> case M.lookup cmp (handlers arch) of
                        Just hdl -> case M.lookup (handlerClass hdl) (handlerClasses arch) of
                            Just cls -> return cls
                            Nothing -> throwError . annotateError (getLocation ann) $ EInvalidHandlerClass (handlerClass hdl)
                        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownComponent cmp
                -- | Obtain the target action from the sink port
                case M.lookup port (sinkPorts tpCls) of
                    Nothing -> throwError . annotateError Internal $ EInvalidSinkPort port
                    Just (_, act) -> 
                        unless (act == targetAction) $
                            throwError . annotateError (getLocation ann) $ EEmitterActionMismatch emitterId targetCmp targetAction (port, act, getLocation . classAnns $ tpCls)
                return ()

    typeInterruptEvent :: Identifier -> ConstStructInitializer ParserAnn -> RTMonad (RTEvent RTSemAnn)
    typeInterruptEvent emitterId (ConstStructInitializer fields' ann') = do
        let validNames = S.fromList ["interval", "arrivals", "deadlines", "transaction"]
        -- | Check that all fields are valid and there are no duplicates
        fieldsMap <- checkFieldNames fields' validNames
        -- | Type interval field
        intervalExpr <- case M.lookup "interval" fieldsMap of
            Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> 
                typeConstExpression expr
            Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) -> 
                throwError . annotateError (getLocation ann') $ EInvalidEventFieldType "interval"
            Nothing -> throwError . annotateError (getLocation ann') $ EMissingEventField "interval"
        -- | Type arrivals field
        arrivalsExpr <- case M.lookup "arrivals" fieldsMap of
            Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> 
                typeConstExpression expr
            Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) -> 
                throwError . annotateError (getLocation ann') $ EInvalidEventFieldType "arrivals"
            Nothing -> throwError . annotateError (getLocation ann') $ EMissingEventField "arrivals"
        -- | Type transactions field
        transactionId <- case M.lookup "transaction" fieldsMap of
            Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> do
                transaction <- typeTransactionObjectExpr expr
                case transaction of
                    RTTransaction ident (RTTransStepAction _ targetCmp targetAction _ _ _) _ -> 
                        checkEmitterConnection emitterId targetCmp targetAction
                        >> return ident
                    RTTransaction {} -> throwError . annotateError Internal $ EInvalidTransaction
                    RTSituation {} -> throwError . annotateError Internal $ EInvalidTransactionMap
            Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) -> 
                throwError . annotateError (getLocation ann') $ EInvalidEventFieldType "transaction"
            Nothing -> throwError . annotateError (getLocation ann') $ EMissingEventField "transaction"
        -- | Type deadlines field
        deadlinesMap <- case M.lookup "deadlines" fieldsMap of
            Just (ConstFieldAssignment _ (ConstStructFieldValue structInit) _) -> 
                typeDeadlinesMap transactionId structInit
            Just (ConstFieldAssignment _ (ConstStructSimpleValue _) _) -> 
                throwError . annotateError (getLocation ann') $ EInvalidEventFieldType "deadlines"
            Nothing ->
                throwError . annotateError (getLocation ann') $ EMissingEventField "deadlines"
        return $ RTEventInterrupt transactionId intervalExpr arrivalsExpr deadlinesMap (RTEventTy (getLocation ann))

    typePeriodicEvent :: Identifier -> ConstStructInitializer ParserAnn -> RTMonad (RTEvent RTSemAnn)
    typePeriodicEvent emitterId (ConstStructInitializer fields' ann') = do
        let validNames = S.fromList ["deadlines", "transaction"]
        -- | Check that all fields are valid and there are no duplicates
        fieldsMap <- checkFieldNames fields' validNames
        -- | Type transactions field
        transactionId <- case M.lookup "transaction" fieldsMap of
            Just (ConstFieldAssignment _ (ConstStructSimpleValue expr) _) -> do
                transaction <- typeTransactionObjectExpr expr
                case transaction of
                    RTTransaction ident (RTTransStepAction _ targetCmp targetAction _ _ _) _ -> 
                        checkEmitterConnection emitterId targetCmp targetAction
                        >> return ident
                    RTTransaction {} -> throwError . annotateError Internal $ EInvalidTransaction
                    RTSituation {} -> throwError . annotateError Internal $ EInvalidTransactionMap
            Just (ConstFieldAssignment _ (ConstStructFieldValue _) _) -> 
                throwError . annotateError (getLocation ann') $ EInvalidEventFieldType "transaction"
            Nothing -> throwError . annotateError (getLocation ann') $ EMissingEventField "transaction"
        -- | Type deadlines field
        deadlinesMap <- case M.lookup "deadlines" fieldsMap of
            Just (ConstFieldAssignment _ (ConstStructFieldValue structInit) _) -> 
                typeDeadlinesMap transactionId structInit
            Just (ConstFieldAssignment _ (ConstStructSimpleValue _) _) -> 
                throwError . annotateError (getLocation ann') $ EInvalidEventFieldType "deadlines"
            Nothing -> 
                throwError . annotateError (getLocation ann') $ EMissingEventField "deadlines"
        return $ RTEventPeriodic transactionId deadlinesMap (RTEventTy (getLocation ann))
   