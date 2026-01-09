module EFP.Schedulability.TransPath.Generator
    (runTransPathGenerator) where

import EFP.Schedulability.RT.Semantic.AST
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.Monad
import EFP.Schedulability.WCEPath.AST
import Control.Monad
import EFP.Schedulability.TransPath.Errors
import Control.Monad.Except
import Utils.Annotations
import qualified Data.Map.Strict as M
import EFP.Schedulability.WCEPath.Types
import ControlFlow.Architecture.Types
import Control.Monad.State
import EFP.Schedulability.TransPath.Types
import EFP.Schedulability.TransPath.Utils
import EFP.Schedulability.RT.Semantic.Types
import Configuration.Configuration
import qualified Data.Text as T
import EFP.Schedulability.WCET.AST
import Data.Functor
import Semantic.Types
import EFP.Schedulability.WCET.Types
import qualified Control.Monad.State as ST

-- | Follows an access port invocation to determine the target component.
--
-- This function resolves which component is connected to a given access port
-- of a source component. It searches through tasks, handlers, and resources
-- in the program architecture to find the connection mapping.
--
-- ==== Parameters
-- * 'componentName' - Name of the component invoking through the port
-- * 'portName' - Name of the access port being invoked
--
-- ==== Returns
-- The identifier of the target component connected to the access port
--
-- ==== Errors
-- Throws an error if:
-- * The component is not found in the architecture
-- * The access port is not found in the component's connections
followInvoke :: Identifier -> Identifier -> TRPGenMonad Identifier
followInvoke componentName portName = do
  progArchitecture <- gets progArch
  case M.lookup componentName (tasks progArchitecture) of
    Just task -> do
        maybe (throwError $ annotateError Internal (EUnknownAccessPort componentName portName)) (return . fst) (M.lookup portName (taskAPConnections task))
    Nothing ->
      case M.lookup componentName (handlers progArchitecture) of
        Just handler -> do
            maybe (throwError $ annotateError Internal (EUnknownAccessPort componentName portName)) (return . fst) $ M.lookup portName (handlerAPConnections handler)
        Nothing ->
            case M.lookup componentName (resources progArchitecture) of
              Just resource -> do
                    maybe (throwError $ annotateError Internal (EUnknownAccessPort componentName portName)) (return . fst) $ M.lookup portName (resAPConnections resource)
              Nothing -> throwError $ annotateError Internal (EUnknownComponent componentName)

-- | Merges a new transaction path block with an existing sequence of blocks.
--
-- This function combines a new block and its execution time with a previously
-- accumulated list of blocks and their total execution time. The new block is
-- prepended to the existing list, and the execution times are summed.
--
-- ==== Parameters
-- * First tuple: New block's WCET and the block itself
-- * Second tuple: Previous accumulated WCET and list of blocks
--
-- ==== Returns
-- A tuple containing the total WCET and the updated block list
mergeTPath :: (WCETime, TransPathBlock TRPSemAnn)
    -> (WCETime, [TransPathBlock TRPSemAnn])
    -> (WCETime, [TransPathBlock TRPSemAnn])
mergeTPath (newWCET, newBlock) (prevWCET, blocks) =
    (newWCET + prevWCET, newBlock : blocks)

-- | Retrieves the class identifier for a given component.
--
-- This function looks up a component in the program architecture and returns
-- its associated class. It searches through tasks, handlers, and resources
-- to find the component and extract its class identifier.
--
-- ==== Parameters
-- * 'componentName' - Name of the component to look up
--
-- ==== Returns
-- The class identifier of the component
--
-- ==== Errors
-- Throws an error if the component is not found in the architecture
getComponentClass :: Identifier -> TRPGenMonad Identifier
getComponentClass componentName = do
    progArchitecture <- gets progArch
    case M.lookup componentName (tasks progArchitecture) of
        Just task -> return $ taskClass task
        Nothing ->
            case M.lookup componentName (handlers progArchitecture) of
                Just handler -> return $ handlerClass handler
                Nothing ->
                    case M.lookup componentName (resources progArchitecture) of
                        Just resource -> return $ resourceClass resource
                        Nothing -> throwError . annotateError Internal $ EUnknownComponent componentName

-- | Determines whether a given component is a task.
--
-- This function checks if the specified component exists in the tasks
-- collection of the program architecture.
--
-- ==== Parameters
-- * 'componentName' - Name of the component to check
--
-- ==== Returns
-- 'True' if the component is a task, 'False' otherwise
isTask :: Identifier -> TRPGenMonad Bool
isTask componentName = do
    progArchitecture <- gets progArch
    return $ M.member componentName (tasks progArchitecture)

-- | Retrieves the worst-case execution time (WCET) for a specific execution path.
--
-- This function looks up the WCET for a given path in a specific component class
-- and function, on a particular platform. It navigates through nested maps and
-- evaluates the WCET expression to obtain a numeric time value.
--
-- ==== Parameters
-- * 'loc' - Source location for error reporting
-- * 'platformId' - Platform identifier
-- * 'componentClass' - Class of the component
-- * 'memberName' - Name of the function containing the path
-- * 'pathId' - Identifier of the specific execution path
--
-- ==== Returns
-- The worst-case execution time as a numeric value
--
-- ==== Errors
-- Throws an error if:
-- * The platform is not found in the WCET database
-- * The component class or function is not found
-- * The path ID is not found
-- * The WCET expression cannot be evaluated to a numeric value
getWCETForPath :: Location -> Identifier -> Identifier -> Identifier -> Identifier -> TRPGenMonad WCETime
getWCETForPath loc platformId componentClass memberName pathId = do
    wcets <- gets transWCETs
    case M.lookup platformId wcets of
        Nothing -> throwError . annotateError loc $ ENoWCETForPath componentClass memberName pathId platformId
        Just platformWCETs -> do
            case M.lookup (componentClass, memberName) platformWCETs of
                Nothing -> throwError . annotateError loc $ ENoWCETForPath componentClass memberName pathId platformId
                Just pathWCETs -> do
                    case M.lookup pathId pathWCETs of
                        Nothing -> throwError . annotateError loc $ ENoWCETForPath componentClass memberName pathId platformId
                        Just (TransactionalWCET _ _ _ _params expr _) -> do
                            evalExpr <- evalConstExpression expr
                            case evalExpr of
                                ConstInt (TInteger val _) _ -> return (fromIntegral val)
                                ConstDouble val _ -> return val
                                _ -> throwError . annotateError Internal $ EInvalidWCETExpression

genPaths :: Identifier
    -> WCEPathBlock WCEPSemAnn
    -> TRPGenMonad [(WCETime, TransPathBlock TRPSemAnn)]
genPaths componentName (WCEPathForLoop initExpr finalExpr innerBlocks pos _ann) = do
    let iterations = ConstBinOp Subtraction finalExpr initExpr (WCEPExprTy TConstInt Internal)
    iterationCount <- evalConstExpression iterations >>= \case
        ConstInt (TInteger val _) _ -> return val
        _ -> throwError . annotateError Internal $ EInvalidForLoop
    innerPaths <- genTPaths componentName [(0, [])] innerBlocks
    return $ map (\(wcet, blocks) -> (wcet * fromIntegral iterationCount, TPBlockForLoop iterationCount (reverse blocks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathMemberFunctionCall memberName constArgs pos ann) = do
    -- | Get current platform
    platformId <- gets (T.unpack . platform . configParams)
    -- | Evaluate constant arguments
    evaluatedArgs <- mapM evalConstExpression constArgs
    -- | Get the WCE paths for the called function
    transPathsMap <- gets transPaths
    componentClass <- getComponentClass componentName
    case M.lookup (componentClass, memberName) transPathsMap of
        Just pathsMap -> (do
            forM (M.elems pathsMap) $ \(WCEPath _ _ pathId params innerBlocks _) -> do
                localInputScope $ do
                    -- | Pass arguments to the called function's local environment
                    passArguments params evaluatedArgs
                    -- | Obtain the worst-case execution time for the path
                    wcet <- getWCETForPath (getLocation ann) platformId componentClass memberName pathId
                    paths <- genTPaths componentName [(wcet, [])] innerBlocks
                    return $ map (\(wcet', blocks) ->
                        let act = TRPResourceOperation componentName memberName pathId (reverse blocks) wcet' TRPOperationTy
                        in
                            (wcet', TPBlockMemberFunctionCall evaluatedArgs act pos TRPBlockTy)) paths)
            <&> concat
        Nothing -> throwError . annotateError (getLocation ann) $ ENoPathsFound componentClass memberName
genPaths componentName (WCEPProcedureInvoke portName procedureName constArgs pos ann) = do
    -- | Get current platform
    platformId <- gets (T.unpack . platform . configParams)
    -- | Evaluate constant arguments
    evaluatedArgs <- mapM evalConstExpression constArgs
    transPathsMap <- gets transPaths
    targetComponent <- followInvoke componentName portName
    componentClass <- getComponentClass targetComponent
    case M.lookup (componentClass, procedureName) transPathsMap of
        Just pathsMap ->
            forM (M.elems pathsMap) (\(WCEPath _ _ pathId params innerBlocks _) -> do
                localInputScope $ do
                    -- | Pass arguments to the called function's local environment
                    passArguments params evaluatedArgs
                    -- | Obtain the worst-case execution time for the path
                    wcet <- getWCETForPath (getLocation ann) platformId componentClass procedureName pathId
                    paths <- genTPaths targetComponent [(wcet, [])] innerBlocks
                    return $ map (\(wcet', blocks) ->
                        let act = TRPResourceOperation targetComponent procedureName pathId (reverse blocks) wcet' TRPOperationTy
                        in
                            (wcet', TPBlockProcedureInvoke evaluatedArgs act pos TRPBlockTy)) paths)
            <&> concat
        Nothing -> throwError . annotateError (getLocation ann) $ ENoPathsFound componentClass procedureName
genPaths componentName (WCEPathCondIf innerBlocks pos _ann) = do
    innerPaths <- genTPaths componentName [(0, [])] innerBlocks
    case innerPaths of
        [(0, [])] -> return []
        _ -> return $ map (\(wcet, blocks) -> (wcet, TPBlockCondIf (reverse blocks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathCondElseIf innerBlocks pos _ann) = do
    innerPaths <- genTPaths componentName [(0, [])] innerBlocks
    case innerPaths of
        [(0, [])] -> return []
        _ -> return $ map (\(wcet, blocks) -> (wcet, TPBlockCondElseIf (reverse blocks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathCondElse innerBlocks pos _ann) = do
    innerPaths <- genTPaths componentName [(0, [])] innerBlocks
    case innerPaths of
        [(0, [])] -> return []
        _ ->
            return $ map (\(wcet, blocks) -> (wcet, TPBlockCondElse (reverse blocks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathMatchCase innerBlocks pos _ann) = do
    innerPaths <- genTPaths componentName [(0, [])] innerBlocks
    case innerPaths of
        [(0, [])] -> return []
        _ -> return $ map (\(wcet, blocks) -> (wcet, TPBlockMatchCase (reverse blocks) pos TRPBlockTy)) innerPaths
genPaths _ (WCEPSendMessage _portName _pos _ann) =
    -- | Continuations are directly defined by the user when defining the transaction
    return []
genPaths componentName (WCEPAllocBox portName pos _ann) = do
    targetComponent <- followInvoke componentName portName
    -- | TODO: We set the wcet to 0 for the time being.
    -- We should later obtain the WCET of the allocation operation from the
    -- platform model.
    return [(0, TPBlockAllocBox targetComponent pos TRPBlockTy)]
genPaths componentName (WCEPFreeBox poolName pos _ann) = do
    targetComponent <- followInvoke componentName poolName
    return [(0, TPBlockFreeBox targetComponent pos TRPBlockTy)]
genPaths _ (WCEPRegularBlock _pos _ann) =
    -- | Regular blocks do not contribute to the transitional path
    return []
genPaths _ (WCEPSystemCall sysCallName constArgs pos _ann) = do
    evaluatedArgs <- mapM evalConstExpression constArgs
    -- | TODO: We set the wcet to 0 for the time being.
    -- We should later obtain the WCET of the system call from the
    -- platform model.
    return [(0, TPBlockSystemCall sysCallName evaluatedArgs pos TRPBlockTy)]
genPaths _ (WCEPReturn pos _ann) = return [(0, TPBlockReturn pos TRPBlockTy)]
genPaths _ (WCEPContinue _actionName _pos _ann) =
    -- | Continuations are directly defined by the user when defining the transaction
    return []
genPaths _ (WCEPReboot pos _ann) = return [(0, TPBlockReboot pos TRPBlockTy)]

genTPaths :: Identifier -> [(WCETime, [TransPathBlock TRPSemAnn])]
    -> [WCEPathBlock WCEPSemAnn]
    -> TRPGenMonad [(WCETime, [TransPathBlock TRPSemAnn])]
genTPaths _ acc []  = return acc
genTPaths componentName paths (block : remainingBlocks) = do
    newPaths <- genPaths componentName block
    if null newPaths then genTPaths componentName paths remainingBlocks
    else do
        let appendedPaths = if null paths then map (\(nwcet, nblocks) -> (nwcet, [nblocks])) newPaths else
                concatMap (\prevPath -> map (`mergeTPath` prevPath) newPaths) paths
        genTPaths componentName appendedPaths remainingBlocks

genTPActivitiesFromAction :: RTTransStep RTSemAnn -> TRPGenMonad ()
genTPActivitiesFromAction (RTTransStepAction stepName componentName actionName pathName nextStep ann) = do
    nextStepNames <- case nextStep of
        RTTransStepEnd lastStepName _ -> return [lastStepName]
        (RTTransStepAction nextStepName _ _ _ _ _) ->
            genTPActivitiesFromAction nextStep >> return [nextStepName]
        (RTTransStepMuticast multicastSteps _) -> do
            mapM_ genTPActivitiesFromAction multicastSteps
            getNextStepNames multicastSteps
        _ -> throwError . annotateError Internal $ EInvalidTransStepType
    -- | Get current platform
    platformId <- gets (T.unpack . platform . configParams)
    transPathsMap <- gets transPaths
    componentClass <- getComponentClass componentName
    -- | Find the path and generate the operations
    case M.lookup (componentClass, actionName) transPathsMap of
        Just pathsMap ->
            case M.lookup pathName pathsMap of
                Just (WCEPath _ _ _ _ innerBlocks _) -> do
                    operations <- localInputScope $ do
                        -- | Obtain the worst-case execution time for the path
                        wcet <- getWCETForPath (getLocation ann) platformId componentClass actionName pathName
                        paths <- genTPaths componentName [(wcet, [])] innerBlocks
                        isTaskComponent <- isTask componentName
                        let mk = if isTaskComponent then TRPTaskOperation else TRPHandlerOperation
                        let createActivity (wcet', blocks) =
                                mk stepName componentName actionName pathName (reverse blocks) nextStepNames wcet' TRPOperationTy
                        return $ map createActivity paths
                    -- | Store generated operations in the map
                    ST.modify (\st -> st { operationMap = M.insert stepName operations (operationMap st) })
                Nothing -> throwError . annotateError (getLocation ann) $ ENoPathsFound componentClass pathName
        Nothing -> throwError . annotateError (getLocation ann) $ ENoPathsFound componentClass actionName

    where

        getNextStepNames = mapM (\case
            (RTTransStepAction n _ _ _ _ _) -> return n
            _ -> throwError . annotateError Internal $ EInvalidTransStepType)

genTPActivitiesFromAction _ = throwError . annotateError Internal $ EInvalidTransStepType

runTransPathGenerator :: TerminaProgArch SemanticAnn
    -> TerminaConfig
    -> WCEPathMap WCEPSemAnn
    -> WCETimesMap WCETSemAnn
    -> RTElement RTSemAnn
    -> Either TRPGenErrors (TransactionPath TRPSemAnn)
runTransPathGenerator arch config wcepMap wcetMap (RTTransaction _ initialStep@(RTTransStepAction stepName _ _ _ _ _) _) =
    let initialState = TRPGenState arch config wcepMap wcetMap M.empty M.empty in
    case ST.runState (runExceptT (genTPActivitiesFromAction initialStep)) initialState of
        (Left err, _) -> Left err
        (_, st) -> Right $ SimpleTransactionPath stepName (operationMap st) TRPTransactionsPathTy
runTransPathGenerator arch config wcepMap wcetMap (RTTransaction _ (RTTransStepConditional branches _) _) =
    flip CondTransactionPath TRPTransactionsPathTy <$> generateBranches branches

    where

        generateBranches ::
            [(TInteger, RTTransStep RTSemAnn)]
            -> Either TRPGenErrors [(TInteger, Identifier, TRPOperationMap TRPSemAnn)]
        generateBranches [] = Right []
        generateBranches ((condExpr, initialStep@(RTTransStepAction stepName _ _ _ _ _)) : xs) = do
            rest <- generateBranches xs
            let initialState = TRPGenState arch config wcepMap wcetMap M.empty M.empty
            case ST.runState (runExceptT (genTPActivitiesFromAction initialStep)) initialState of
                (Left err, _) -> Left err
                (Right _, st) -> Right $ (condExpr, stepName, operationMap st) : rest
        generateBranches (_ : _) = Left $ annotateError Internal EInvalidRTElementForTransPath
runTransPathGenerator _ _ _ _ _ =
    Left $ annotateError Internal EInvalidRTElementForTransPath