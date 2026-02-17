module EFP.Schedulability.TransPath.Utils where
import EFP.Schedulability.TransPath.AST
import EFP.Schedulability.TransPath.Monad
import Control.Monad.Except
import Utils.Annotations
import Control.Monad.State
import qualified Data.Map.Strict as M
import EFP.Schedulability.TransPath.Types
import Data.Bits
import EFP.Schedulability.TransPath.Errors
import Data.Foldable
import qualified Data.Set as S
import ControlFlow.Architecture.Types

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

getResourceLockSet ::
    S.Set (Identifier, Identifier, Identifier)
    -> TransPathBlock TRPSemAnn 
    -> TRPGenMonad (S.Set (Identifier, Identifier, Identifier))
getResourceLockSet currentLockSet (TPBlockProcedureInvoke _ (TRPResourceOperation _ _ _ _  _ (TRPOperationTy lockSet)) _ _) =
    return $ S.union currentLockSet lockSet
getResourceLockSet currentLockSet (TPBlockMemberFunctionCall _ (TRPResourceOperation _ _ _ _  _ (TRPOperationTy lockSet)) _ _) =
    return $ S.union currentLockSet lockSet
getResourceLockSet currentLockSet (TPBlockForLoop _ blks _ _) =
    foldlM getResourceLockSet currentLockSet blks
getResourceLockSet currentLockSet (TPBlockCondIf blks _ _) =
    foldlM getResourceLockSet currentLockSet blks
getResourceLockSet currentLockSet (TPBlockCondElseIf blks _ _) =
    foldlM getResourceLockSet currentLockSet blks
getResourceLockSet currentLockSet (TPBlockCondElse blks _ _) =
    foldlM getResourceLockSet currentLockSet blks
getResourceLockSet currentLockSet (TPBlockMatchCase blks _ _) =
    foldlM getResourceLockSet currentLockSet blks
getResourceLockSet currentLockSet (TPBlockAllocBox targetComponent _ _) = do
    resLockMap <- gets resourceLockingMap
    isLocked <- case M.lookup targetComponent resLockMap of
        Just ResourceLockNone -> return False
        Just _ -> return True
        Nothing -> throwError . annotateError Internal $ EUnknownComponent targetComponent
    if isLocked then
        return $ S.insert (targetComponent, "alloc", "path0") currentLockSet
    else
        return currentLockSet
getResourceLockSet currentLockSet (TPBlockFreeBox targetComponent _ _) = do
    resLockMap <- gets resourceLockingMap
    isLocked <- case M.lookup targetComponent resLockMap of
        Just ResourceLockNone -> return False
        Just _ -> return True
        Nothing -> throwError . annotateError Internal $ EUnknownComponent targetComponent
    if isLocked then
        return $ S.insert (targetComponent, "free", "path0") currentLockSet
    else
        return currentLockSet
getResourceLockSet currentLockSet _ = return currentLockSet

sameResourceLocking ::
    TRPOperation TRPSemAnn
    -> TRPOperation TRPSemAnn
    -> Bool
sameResourceLocking prevOp newOp = 
    case (getAnnotation prevOp, getAnnotation newOp) of
        (TRPOperationTy resLock1, TRPOperationTy resLock2) ->
            resLock1 == resLock2
        _ -> False

filterOperations :: 
    [TRPOperation TRPSemAnn]
    -> TRPOperation TRPSemAnn
    -> [TRPOperation TRPSemAnn]
filterOperations prevOps newOp@(TRPTaskOperation _ _ _ _ _ _ newWcet _) =
    let differentOps = filter (not . sameResourceLocking newOp) prevOps
        matchingOps = filter (sameResourceLocking newOp) prevOps
    in case matchingOps of
        [] -> differentOps ++ [newOp]
        (TRPTaskOperation _ _ _ _ _ _ oldWcet _):_ ->
            if newWcet > oldWcet
                then differentOps ++ [newOp]
                else prevOps
        _ -> prevOps -- ^ This should not happen (mixed operation types)
filterOperations prevOps newOp@(TRPHandlerOperation _ _ _ _ _ _ newWcet _) =
    let differentOps = filter (not . sameResourceLocking newOp) prevOps
        matchingOps = filter (sameResourceLocking newOp) prevOps
    in case matchingOps of
        [] -> differentOps ++ [newOp]
        (TRPHandlerOperation _ _ _ _ _ _ oldWcet _):_ ->
            if newWcet > oldWcet
                then differentOps ++ [newOp]
                else prevOps
        _ -> prevOps -- ^ This should not happen (mixed operation types)
filterOperations prevOps _ = prevOps -- ^ This should not happen
