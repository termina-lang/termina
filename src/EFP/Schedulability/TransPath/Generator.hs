module EFP.Schedulability.TransPath.Generator where

import EFP.Schedulability.RT.AST
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.Monad
import EFP.Schedulability.WCEPath.AST
import Control.Monad
import EFP.Schedulability.TransPath.Errors
import Control.Monad.Except
import Utils.Annotations
import qualified Data.Map as M
import EFP.Schedulability.WCEPath.Types
import ControlFlow.Architecture.Types
import Control.Monad.State
import EFP.Schedulability.TransPath.Types
import EFP.Schedulability.TransPath.Utils
import EFP.Schedulability.RT.Types
import Configuration.Configuration
import qualified Data.Text as T
import EFP.Schedulability.WCET.AST
import Data.Functor

getWCEPaths :: Location -> Identifier -> Identifier -> TRPGenMonad (M.Map Identifier (WCEPath WCEPSemAnn))
getWCEPaths loc componentName memberName = do
    trPaths <- gets transPaths
    case M.lookup (componentName, memberName) trPaths of
        Just paths -> return paths
        Nothing -> throwError . annotateError loc $ ENoPathsFound componentName memberName

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

genActivity :: Identifier -> Identifier -> Identifier -> TRPGenMonad (TransPathActivity TRPSemAnn)
genActivity componentName actionName pathName = do
    arch <- gets progArch
    if M.member componentName (tasks arch)
        then return $ TRPTaskActivity componentName actionName pathName [] [] 0 TRPActivityTy
    else if M.member componentName (handlers arch)
        then return $ TRPHandlerActivity componentName actionName pathName [] [] 0 TRPActivityTy
    else if M.member componentName (resources arch)
        then return $ TRPResourceActivity componentName actionName pathName [] 0 TRPActivityTy
    else throwError . annotateError Internal $ EUnknownComponent componentName

mergeTPath :: (WCETime, TransPathBlock TRPSemAnn)
    -> (WCETime, [TransPathBlock TRPSemAnn])
    -> (WCETime, [TransPathBlock TRPSemAnn])
mergeTPath (newWCET, newBlock) (prevWCET, blks) =
    (newWCET + prevWCET, newBlock : blks)

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

getWCETForPath :: Location -> Identifier -> Identifier -> Identifier -> Identifier -> TRPGenMonad WCETime
getWCETForPath loc plt componentClass funcName pathId = do
    wcets <- gets transWCETs
    case M.lookup plt wcets of
        Nothing -> throwError . annotateError loc $ ENoWCETForPath componentClass funcName pathId plt
        Just platformWCETs -> do
            case M.lookup (componentClass, funcName) platformWCETs of
                Nothing -> throwError . annotateError loc $ ENoWCETForPath componentClass funcName pathId plt
                Just pathWCETs -> do
                    case M.lookup pathId pathWCETs of
                        Nothing -> throwError . annotateError loc $ ENoWCETForPath componentClass funcName pathId plt
                        Just (TransactionalWCET _ _ _ _params expr _) -> do
                            evalExpr <- evalConstExpression expr
                            case evalExpr of
                                ConstInt (TInteger val _) _ -> return (fromIntegral val)
                                ConstDouble val _ -> return val
                                _ -> throwError . annotateError Internal $ EInvalidWCETExpression

genPaths :: Identifier
    -> WCEPathBlock WCEPSemAnn
    -> TRPGenMonad [(WCETime, TransPathBlock TRPSemAnn)]
genPaths componentName (WCEPathForLoop initExpr finalExpr blocks pos _ann) = do
    let iterations = ConstBinOp Subtraction finalExpr initExpr (WCEPExprTy TConstInt Internal)
    evIterations <- evalConstExpression iterations >>= \case
        ConstInt (TInteger val _) _ -> return val
        _ -> throwError . annotateError Internal $ EInvalidForLoop
    innerPaths <- genTPaths componentName [] blocks
    return $ map (\(wcet, blks) -> (wcet * fromIntegral evIterations, TPBlockForLoop evIterations (reverse blks) pos TRPBlockTy)) innerPaths
genPaths resName (WCEPathMemberFunctionCall funcName constArgs pos ann) = do
    -- | Get current platform
    plt <- gets (T.unpack . platform . configParams)
    -- | Evaluate constant arguments
    evalArgs <- mapM evalConstExpression constArgs
    -- | Get the WCE paths for the called function
    trPaths <- gets transPaths
    case M.lookup (resName, funcName) trPaths of
        Just wcePaths -> (do
            forM (M.elems wcePaths) $ \(WCEPath _ _ pathId params blks _) -> do
                componentClass <- getComponentClass resName
                localInputScope $ do
                    -- | Pass arguments to the called function's local environment
                    passArguments params evalArgs
                    -- | Obtain the worst-case execution time for the path
                    wcet <- getWCETForPath (getLocation ann) plt componentClass funcName pathId
                    paths <- genTPaths resName [(wcet, [])] blks
                    return $ map (\(wcet', trblks) ->
                        let act = TRPResourceActivity resName funcName pathId trblks wcet' TRPActivityTy
                        in
                            (wcet', TPBlockMemberFunctionCall evalArgs act pos TRPBlockTy)) paths)
            <&> concat
        Nothing -> throwError . annotateError (getLocation ann) $ ENoPathsFound resName funcName
genPaths componentName (WCEPProcedureInvoke portName procName constArgs pos ann) = do
    -- | Get current platform
    plt <- gets (T.unpack . platform . configParams)
    -- | Evaluate constant arguments
    evalArgs <- mapM evalConstExpression constArgs
    trPaths <- gets transPaths
    targetResource <- followInvoke componentName portName
    case M.lookup (targetResource, procName) trPaths of
        Just wcePaths -> (do
            forM (M.elems wcePaths) $ \(WCEPath _ _ pathId params blks _) -> do
                componentClass <- getComponentClass targetResource
                localInputScope $ do
                    -- | Pass arguments to the called function's local environment
                    passArguments params evalArgs
                    -- | Obtain the worst-case execution time for the path
                    wcet <- getWCETForPath (getLocation ann) plt componentClass procName pathId
                    paths <- genTPaths targetResource [(wcet, [])] blks
                    return $ map (\(wcet', trblks) ->
                        let act = TRPResourceActivity targetResource procName pathId trblks wcet' TRPActivityTy
                        in
                            (wcet', TPBlockProcedureInvoke evalArgs act pos TRPBlockTy)) paths)
            <&> concat
        Nothing -> throwError . annotateError (getLocation ann) $ ENoPathsFound targetResource procName
genPaths componentName (WCEPathCondIf blocks pos _ann) = do
    innerPaths <- genTPaths componentName [] blocks
    return $ map (\(wcet, blks) -> (wcet, TPBlockCondIf (reverse blks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathCondElseIf blocks pos _ann) = do
    innerPaths <- genTPaths componentName [] blocks
    return $ map (\(wcet, blks) -> (wcet, TPBlockCondElseIf (reverse blks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathCondElse blocks pos _ann) = do
    innerPaths <- genTPaths componentName [] blocks
    return $ map (\(wcet, blks) -> (wcet, TPBlockCondElse (reverse blks) pos TRPBlockTy)) innerPaths
genPaths componentName (WCEPathMatchCase blocks pos _ann) = do
    innerPaths <- genTPaths componentName [] blocks
    return $ map (\(wcet, blks) -> (wcet, TPBlockMatchCase (reverse blks) pos TRPBlockTy)) innerPaths
genPaths _ (WCEPSendMessage _portName _pos _ann) =
    -- | Continuations are directly defined by the user when defining the transaction
    return []
genPaths componentName (WCEPAllocBox portName pos _ann) = do
    targetResource <- followInvoke componentName portName
    -- | TODO: We set the wcet to 0 for the time being.
    -- We should later obtain the WCET of the allocation operation from the
    -- platform model.
    return [(0, TPBlockAllocBox targetResource pos TRPBlockTy)]
genPaths componentName (WCEPFreeBox poolName pos _ann) = do
    targetResource <- followInvoke componentName poolName
    return [(0, TPBlockFreeBox targetResource pos TRPBlockTy)]
genPaths _ (WCEPRegularBlock _pos _ann) =
    -- | Regular blocks do not contribute to the transitional path
    return []
genPaths _ (WCEPSystemCall sysCallName constArgs pos _ann) = do
    evalArgs <- mapM evalConstExpression constArgs
    -- | TODO: We set the wcet to 0 for the time being.
    -- We should later obtain the WCET of the system call from the
    -- platform model.
    return [(0, TPBlockSystemCall sysCallName evalArgs pos TRPBlockTy)]
genPaths _ (WCEPReturn pos _ann) = return [(0, TPBlockReturn pos TRPBlockTy)]
genPaths _ (WCEPContinue _actionName _pos _ann) =
    -- | Continuations are directly defined by the user when defining the transaction
    return []
genPaths _ (WCEPReboot pos _ann) = return [(0, TPBlockReboot pos TRPBlockTy)]

genTPaths :: Identifier -> [(WCETime, [TransPathBlock TRPSemAnn])]
    -> [WCEPathBlock WCEPSemAnn]
    -> TRPGenMonad [(WCETime, [TransPathBlock TRPSemAnn])]
genTPaths _ acc []  = return acc
genTPaths componentName paths (blk : xs) = do
    newPaths <- genPaths componentName blk
    let appendedPaths = concatMap (\prevPath -> map (`mergeTPath` prevPath) newPaths) paths
    genTPaths componentName appendedPaths xs

genTPActivitiesFromStep :: RTTransStep RTSemAnn -> TRPGenMonad [TransPathActivity TRPSemAnn]
genTPActivitiesFromStep (RTTransStepAction name task action path Nothing a) = undefined
genTPActivitiesFromStep (RTTransStepAction name task action path (Just next) a) = undefined
genTPActivitiesFromStep (RTTransStepMuticast steps a) = undefined
genTPActivitiesFromStep (RTTransStepConditional _conds _ann) = throwError . annotateError Internal $ EInvalidTransStepType