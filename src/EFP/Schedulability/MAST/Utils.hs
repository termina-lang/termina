module EFP.Schedulability.MAST.Utils where
import EFP.Schedulability.Core.AST
import EFP.Schedulability.MAST.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import ControlFlow.Architecture.Types
import Utils.Annotations
import EFP.Schedulability.MAST.Errors
import Control.Monad.State
import Control.Monad
import EFP.Schedulability.MAST.AST
import qualified Semantic.AST as SAST
import Data.Bits

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) i1 i2 = i1 ++ "__" ++ i2

getMASTTransactionId :: Identifier -> Identifier -> Identifier -> Identifier
getMASTTransactionId  eventId emitterId transId = "__trans" <::> eventId <::> emitterId <::> transId

getMASTSchedulingServerId :: Identifier -> Identifier
getMASTSchedulingServerId componentId = "__server" <::> componentId

getMASTExternalEventId :: Identifier -> Identifier -> Identifier
getMASTExternalEventId eventId emitterId = "__ext_evt" <::> eventId <::> emitterId

getMASTInternalEventId :: Identifier -> Identifier
getMASTInternalEventId stepName = "__int_evt" <::> stepName

getAllocBoxMASTOperationId :: Identifier -> Identifier
getAllocBoxMASTOperationId poolId = "__alloc_box_op" <::> poolId

getFreeBoxMASTOperationId :: Identifier -> Identifier
getFreeBoxMASTOperationId poolId = "__free_box_op" <::> poolId

getSystemCallMASTOperationId :: Identifier -> Identifier
getSystemCallMASTOperationId sysCallName = "__syscall_op" <::> sysCallName

timerTopHalfMASTOperationId :: Identifier
timerTopHalfMASTOperationId = "__timer_top_half__op"

timerTopHalfSchedulingServerId :: Identifier
timerTopHalfSchedulingServerId = "__timer_top_half__server"

getIrqEmitterTopHalfOperationId :: Identifier -> Identifier
getIrqEmitterTopHalfOperationId emitterId = "__irq_top_half_op" <::> emitterId

getResourceMASTOperationId :: Identifier -> Identifier -> Identifier -> [ConstExpression a] -> MASTGenMonad Identifier
getResourceMASTOperationId targetComponent targetAction targetPath postfix = do
    postfixStr <- concat <$> mapM (showConstExpression >=> return . ("__" ++)) postfix
    return $ "__op" <::> targetComponent <::> targetAction <::> targetPath <> postfixStr

    where

        showConstExpression :: ConstExpression a -> MASTGenMonad Identifier
        showConstExpression (ConstInt (TInteger val _) _) = return $ show val
        showConstExpression _ = throwError . annotateError Internal $ EUnsupportedConstExpression

getTargetAction :: Identifier -> Identifier -> MASTGenMonad Identifier
getTargetAction componentName sinkPort = do
    arch <- gets progArch
    cmpCls <- case M.lookup componentName (tasks arch) of
            Just tsk -> return $ taskClasses arch M.! taskClass tsk
            Nothing -> case M.lookup componentName (handlers arch) of
                Just hdl -> return $ handlerClasses arch M.! handlerClass hdl
                Nothing -> throwError . annotateError Internal $ EUnknownComponent componentName
    case M.lookup sinkPort (sinkPorts cmpCls) of
        Just (_, actionId) -> return actionId
        Nothing -> throwError . annotateError Internal $ EUnknownSinkPort componentName sinkPort

getMASTOperationId :: MASTOperation -> Identifier
getMASTOperationId (MASTSimpleOperation opId _ _ _) = opId
getMASTOperationId (MASTCompositeOperation opId _ ) = opId
getMASTOperationId (MASTEnclosingOperation opId _ _) = opId

bodyMASTOperationId :: Identifier -> Identifier
bodyMASTOperationId parentOpId = parentOpId <::> "body"

getTaskMASTOperationId :: Identifier -> Identifier -> Identifier -> Identifier
getTaskMASTOperationId taskId actionId pathId = "__op" <::> taskId <::> actionId <::> pathId

getHandlerMASTOperationId :: Identifier -> Identifier -> Identifier -> Identifier
getHandlerMASTOperationId handlerId actionId pathId = "__op" <::> handlerId <::> actionId <::> pathId

getIrqLockMASTOperationId :: Identifier -> Identifier
getIrqLockMASTOperationId resourceId = "__irq_lock_op" <::> resourceId

getIrqUnlockMASTOperationId :: Identifier -> Identifier
getIrqUnlockMASTOperationId resourceId = "__irq_unlock_op" <::> resourceId

getMutexLockMASTOperationId :: Identifier -> Identifier
getMutexLockMASTOperationId resourceId = "__mutex_lock_op" <::> resourceId

getMutexUnlockMASTOperationId :: Identifier -> Identifier
getMutexUnlockMASTOperationId resourceId = "__mutex_unlock_op" <::> resourceId

insertOperation :: MASTOperation -> MASTGenMonad ()
insertOperation operation = do
    ops <- gets operations
    let opId = getMASTOperationId operation
    modify $ \s -> s { operations = M.insert opId operation ops }

insertSharedResource :: MASTSharedResource -> MASTGenMonad ()
insertSharedResource shRes@(MASTImmediateCeilingResource resName _ceilingPriority) = do
    shres <- gets sharedResources
    modify $ \s -> s { sharedResources = M.insert resName shRes shres }

insertSchedulingServer :: MASTSchedulingServer -> MASTGenMonad ()
insertSchedulingServer server@(MASTRegularSchedulingServer serverId _ _) = do
    servers <- gets schedulingServers
    modify $ \s -> s { schedulingServers = M.insert serverId server servers }

getTimerPeriod :: SAST.Expression a -> MASTGenMonad Time
getTimerPeriod (SAST.StructInitializer [FieldValueAssignment "period" (SAST.StructInitializer fields _) _] _) = do
    fieldMap <- foldM (\acc field ->
        case field of 
            FieldValueAssignment fname fexpr _ -> return $ M.insert fname fexpr acc
            _ -> throwError . annotateError Internal $ EInvalidTimerPeriodExpression
        )  M.empty fields
    seconds <- case M.lookup "tv_sec" fieldMap of
        Just expr -> evalExpression expr
        Nothing -> throwError . annotateError Internal $ EInvalidTimerPeriodExpression
    microseconds <- case M.lookup "tv_usec" fieldMap of
        Just expr -> evalExpression expr
        Nothing -> throwError . annotateError Internal $ EInvalidTimerPeriodExpression
    return $ fromIntegral seconds * 1000 + (fromIntegral microseconds / 1000)

    where

        evalExpression :: SAST.Expression a -> MASTGenMonad TInteger
        evalExpression (SAST.Constant (I val _) _) = return val
        evalExpression (SAST.AccessObject (SAST.Variable obj _)) = do
            glbMap <- gets (globalConstants . progArch)
            case M.lookup obj glbMap of
                Just (TPGlobalConstant _ _ (SAST.Constant (I val _) _) _) -> return val
                _ -> throwError . annotateError Internal $ EInvalidTimerPeriodExpression
        evalExpression (SAST.BinOp op lhs rhs _) = do
            lval <- evalExpression lhs
            rval <- evalExpression rhs
            case op of
                SAST.Addition -> return $ lval + rval
                SAST.Subtraction -> return $ lval - rval
                SAST.Multiplication -> return $ lval * rval
                SAST.Division -> return $ lval `div` rval
                SAST.Modulo -> return $ lval `mod` rval
                SAST.BitwiseAnd -> return $ lval .&. rval
                SAST.BitwiseOr -> return $ lval .|. rval
                SAST.BitwiseXor -> return $ lval `xor` rval
                SAST.BitwiseRightShift -> return $ lval `shiftR` fromIntegral rval
                SAST.BitwiseLeftShift -> return $ lval `shiftL` fromIntegral rval
                _ -> throwError . annotateError Internal $ EInvalidTimerPeriodExpression
        evalExpression _ = throwError . annotateError Internal $ EInvalidTimerPeriodExpression
    
getTimerPeriod _ = throwError . annotateError Internal $ EInvalidTimerPeriodExpression
