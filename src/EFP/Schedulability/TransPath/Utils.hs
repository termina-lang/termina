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

getOpSet :: TRPOperation TRPSemAnn -> S.Set (Identifier, Identifier, Identifier)
getOpSet = \case
    TRPTaskOperation _ _ _ _ _ _ _ (TRPOperationTy lockSet) -> lockSet
    TRPHandlerOperation _ _ _ _ _ _ _ (TRPOperationTy lockSet) -> lockSet
    op -> error $ "getOpSet: unsupported operation type for extracting resource locking set: " ++ show op

getOpWCET :: TRPOperation TRPSemAnn -> WCETime
getOpWCET (TRPTaskOperation _ _ _ _ _ _ wcet _) = wcet
getOpWCET (TRPHandlerOperation _ _ _ _ _ _ wcet _) = wcet
getOpWCET op = error $ "getOpWCET: unsupported operation type for extracting WCET: " ++ show op

setOpWCET :: WCETime -> TRPOperation TRPSemAnn -> TRPOperation TRPSemAnn
setOpWCET newWcet (TRPTaskOperation s t a p blks cont _ ann) = TRPTaskOperation s t a p blks cont newWcet ann
setOpWCET newWcet (TRPHandlerOperation s h a p blks cont _ ann) = TRPHandlerOperation s h a p blks cont newWcet ann
setOpWCET _ op = error $ "setOpWCET: unsupported operation type for setting WCET: " ++ show op

getOpStepName :: TRPOperation TRPSemAnn -> Identifier
getOpStepName (TRPTaskOperation stepName _ _ _ _ _ _ _) = stepName
getOpStepName (TRPHandlerOperation stepName _ _ _ _ _ _ _) = stepName
getOpStepName op = error $ "getOpStepName: unsupported operation type for extracting step name: " ++ show op

groupOperations :: [TRPOperation TRPSemAnn] -> [TRPOperation TRPSemAnn]
groupOperations = foldl' filterOp []

    where

        filterOp acc x =
            case extractEqual x acc of
                Nothing -> x : acc
                Just (match, rest) -> 
                    let matchWcet = getOpWCET match 
                        newWcet = getOpWCET x
                        newOp = if newWcet > matchWcet then x else match
                    in
                        newOp : rest

        extractEqual _ [] = Nothing
        extractEqual x (y:ys) =
            if sameResourceLocking x y then Just (y, ys)
            else 
                case extractEqual x ys of
                    Nothing        -> Nothing
                    Just (m, rest) -> Just (m, y : rest)

        sameResourceLocking op1 op2 = getOpSet op1 == getOpSet op2

mergeOperations :: [TRPOperation TRPSemAnn] -> [TRPOperation TRPSemAnn]
mergeOperations = foldl' insertOrMerge []

    where

        insertOrMerge acc x =
            case extractComparable x acc of
                Nothing           -> x : acc
                Just (match, rest) -> insertOrMerge rest (merge x match)

        extractComparable _ [] = Nothing
        extractComparable x (y:ys) =
            if comparable x y then Just (y, ys)
            else 
                case extractComparable x ys of
                    Nothing        -> Nothing
                    Just (m, rest) -> Just (m, y : rest)

        comparable a b =
            let sa = getOpSet a
                sb = getOpSet b
            in  sa `S.isSubsetOf` sb || sb `S.isSubsetOf` sa

        merge a b =
            let bigger = if getOpSet a `S.isSubsetOf` getOpSet b then b else a
                maxWCET = max (getOpWCET a) (getOpWCET b)
            in  setOpWCET maxWCET bigger
