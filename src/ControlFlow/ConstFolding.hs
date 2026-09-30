module ControlFlow.ConstFolding where

import Semantic.Types
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import ControlFlow.ConstFolding.Monad
import Lowering.AST
import ControlFlow.Traversal (Rewriter(..), rewriteObject, rewriteExpression)
import Utils.Annotations
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import ControlFlow.ConstFolding.Utils
import Control.Monad
import Configuration.Platform (maxTimerPeriodTicks)
import Core.Utils
import Command.Types
import Modules.Modules

-- | The folded initializer of a constant object: a named constant, or a field
-- or an element of one, reached down through the initializers. A name that
-- holds a value gives it as a literal of the type of the object.
constInitializerOf :: Object SemanticAnn -> ConstFoldMonad (Expression SemanticAnn)
constInitializerOf obj@(Variable ident ann) = do
  globalEnv <- ST.gets constEnv
  case M.lookup ident globalEnv of
    Just (ConstValue value) -> do
      ty <- getObjType obj
      return $ Constant value (buildExpAnn (getLocation ann) ty)
    Just (ConstInitializer initializer) -> return initializer
    Nothing -> throwError $ annotateError Internal (EUnknownIdentifier ident)
constInitializerOf (MemberAccess obj field _) = do
  initializer <- constInitializerOf obj
  case initializer of
    StructInitializer fields _ ->
      case [expr | FieldValueAssignment name expr _ <- fields, name == field] of
        [expr] -> return expr
        _ -> throwError $ annotateError Internal ENotConstant
    _ -> throwError $ annotateError Internal ENotConstant
constInitializerOf (ArrayIndexExpression obj index ann) = do
  initializer <- constInitializerOf obj
  indexValue <- evalConstExpression index
  case (initializer, indexValue) of
    (ArrayExprListInitializer elements _, I (TInteger i _) _) -> do
      inBounds (toInteger (length elements)) i
      return $ elements !! fromInteger i
    (ArrayInitializer value size _, I (TInteger i _) _) -> do
      sizeValue <- evalConstExpression size
      case sizeValue of
        I (TInteger n _) _ -> inBounds n i >> return value
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> throwError $ annotateError Internal ENotConstant

  where

    inBounds :: Integer -> Integer -> ConstFoldMonad ()
    inBounds size i =
      unless (0 <= i && i < size) $
        throwError $ annotateError (getLocation ann) (EArrayIndexOutOfBounds size i)
constInitializerOf _ = throwError $ annotateError Internal ENotConstant

evalConstObject :: Object SemanticAnn -> ConstFoldMonad (Const SemanticAnn)
evalConstObject obj = do
  initializer <- constInitializerOf obj
  case initializer of
    Constant value _ -> return value
    _ -> throwError $ annotateError Internal ENotConstant

evalConstExpression :: Expression SemanticAnn -> ConstFoldMonad (Const SemanticAnn)
evalConstExpression (AccessObject obj) = do
  evalConstObject obj
evalConstExpression (Constant c _) = return c
evalConstExpression expr@(BinOp op lhs rhs ann) = do
  lhs' <- evalConstExpression lhs
  rhs' <- evalConstExpression rhs
  ty <- getExprType expr
  plt <- ST.gets targetPlatform
  case (lhs', rhs') of
    (c1, c2) -> evalBinOp plt (getLocation ann) op c1 c2 ty
evalConstExpression (Casting expr' ty ann) = do
  constExpr <- evalConstExpression expr'
  plt <- ST.gets targetPlatform
  case constExpr of
    (I constValue@(TInteger i _) _) ->
      if intTy ty && not (memberIntCons plt i ty)
        then throwError $ annotateError (getLocation ann) (EConstIntegerOverflow i ty)
        else return $ I constValue (Just ty)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
evalConstExpression (IsEnumVariantExpression obj _ variant _) = do
  initializer <- constInitializerOf obj
  case initializer of
    EnumVariantInitializer _ held _ _ -> return $ B (held == variant)
    _ -> throwError $ annotateError Internal ENotConstant
evalConstExpression (IsMonadicVariantExpression obj label _) = do
  initializer <- constInitializerOf obj
  case initializer of
    MonadicVariantInitializer held _ -> return $ B (holds label held)
    _ -> throwError $ annotateError Internal ENotConstant

  where

    holds NoneLabel None = True
    holds SomeLabel (Some _) = True
    holds OkLabel (Ok _) = True
    holds ErrorLabel (Error _) = True
    holds SuccessLabel Success = True
    holds FailureLabel (Failure _) = True
    holds _ _ = False
evalConstExpression _ = throwError $ annotateError Internal ENotConstant

-- | Evaluates a type. This basically only applies to arrays. The function
-- returns the same expression but with the types (i.e., the array sizes)
-- evaluated.
foldType :: Location -> TerminaType SemanticAnn -> ConstFoldMonad (TerminaType SemanticAnn)
foldType loc (TArray ty arraySize)= do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- foldType loc ty
  return (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize)))
foldType loc (TAtomicArray ty arraySize) = do
  arraySizeValue <- evalConstExpression arraySize
  return (TAtomicArray ty (Constant arraySizeValue (buildExpAnn loc TUSize)))
foldType loc (TFixedLocation (TArray ty arraySize)) = do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- foldType loc ty
  return (TFixedLocation (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize))))
foldType loc (TConstSubtype ty) = TConstSubtype <$> foldType loc ty
foldType _ ty = return ty

foldParam :: Location -> Parameter SemanticAnn -> ConstFoldMonad (Parameter SemanticAnn)
foldParam loc (Parameter name ty ann) = 
  (\ty' -> Parameter name ty' ann) <$> foldType loc ty

foldInterfaceMember :: InterfaceMember SemanticAnn -> ConstFoldMonad (InterfaceMember SemanticAnn)
foldInterfaceMember (InterfaceProcedure ak procId params mods ann) = do
  params' <- mapM (foldParam (getLocation ann)) params
  ann' <- foldAnnotation ann
  return $ InterfaceProcedure ak procId params' mods ann'

foldAnnotation :: SemanticAnn -> ConstFoldMonad SemanticAnn
foldAnnotation (SemanticAnn (ETy (SimpleType ty)) exprLoc) = do
  ty' <- foldType exprLoc ty
  return $ SemanticAnn (ETy (SimpleType ty')) exprLoc
foldAnnotation (SemanticAnn (ETy (ObjectType ak ty)) exprLoc) = do
  ty' <- foldType exprLoc ty
  return $ SemanticAnn (ETy (ObjectType ak ty')) exprLoc
foldAnnotation (SemanticAnn (ETy (AccessPortObjType ak fields ty)) exprLoc) = do
  ty' <- foldType exprLoc ty
  return $ SemanticAnn (ETy (AccessPortObjType ak fields ty')) exprLoc
foldAnnotation (SemanticAnn (ETy (AppType params ty)) exprLoc) = do
  ty' <- foldType exprLoc ty
  params' <- mapM (foldParam exprLoc) params
  return $ SemanticAnn (ETy (AppType params' ty')) exprLoc
foldAnnotation ann@(SemanticAnn (FTy SimpleField) _) = return ann
foldAnnotation (SemanticAnn (FTy (AccessPortField ifaces)) exprLoc) = do
  ifaces' <- mapM foldInterfaceMember ifaces
  return $ SemanticAnn (FTy (AccessPortField ifaces')) exprLoc
foldAnnotation ann@(SemanticAnn (STy SimpleStmtType) _) = return ann
foldAnnotation (SemanticAnn (STy (MatchCaseStmtType tys)) exprLoc) = do
  tys' <- mapM (foldType exprLoc) tys
  return $ SemanticAnn (STy (MatchCaseStmtType tys')) exprLoc
foldAnnotation (SemanticAnn (STy (PortConnection (APConnTy pty resTy procs))) loc) = do
  pty' <- foldType loc pty
  resTy' <- foldType loc resTy
  procs' <- mapM (\(ProcedureSeman ident params mods) -> do
    params' <- mapM (foldParam loc) params
    return $ ProcedureSeman ident params' mods) procs
  return $ SemanticAnn (STy (PortConnection (APConnTy pty' resTy' procs'))) loc
foldAnnotation (SemanticAnn (STy (PortConnection (APAtomicConnTy ty))) loc) = do
  ty' <- foldType loc ty
  return $ SemanticAnn (STy (PortConnection (APAtomicConnTy ty'))) loc
foldAnnotation (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ty arrSize arrPortSize))) loc) = do
  ty' <- foldType loc ty
  arraySizeValue <- evalConstExpression arrSize
  arrayPortSizeValue <- evalConstExpression arrPortSize
  let arrSizeConst = Constant arraySizeValue (buildExpAnn loc TUSize)
      arrPortSizeValue = Constant arrayPortSizeValue (buildExpAnn loc TUSize)
  return $ SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ty' arrSizeConst arrPortSizeValue))) loc
foldAnnotation (SemanticAnn (STy (PortConnection (APPoolConnTy ty poolSize))) loc) = do
  ty' <- foldType loc ty
  poolSizeValue <- evalConstExpression poolSize
  let poolSizeConst = Constant poolSizeValue (buildExpAnn loc TUSize)
  return $ SemanticAnn (STy (PortConnection (APPoolConnTy ty' poolSizeConst))) loc
foldAnnotation (SemanticAnn (STy (PortConnection (SPConnTy ty ident))) loc) = do
  ty' <- foldType loc ty
  return $ SemanticAnn (STy (PortConnection (SPConnTy ty' ident))) loc
foldAnnotation (SemanticAnn (STy (PortConnection (InPConnTy ty ident))) loc) = do
  ty' <- foldType loc ty
  return $ SemanticAnn (STy (PortConnection (InPConnTy ty' ident))) loc
foldAnnotation (SemanticAnn (STy (PortConnection (OutPConnTy ty))) loc) = do
  ty' <- foldType loc ty
  return $ SemanticAnn (STy (PortConnection (OutPConnTy ty'))) loc
foldAnnotation (SemanticAnn (GTy ty) loc) = do
  ty' <- foldType loc ty
  return $ SemanticAnn (GTy ty') loc
foldAnnotation ann@(SemanticAnn TTy _) = return ann
foldAnnotation (SemanticAnn (FnTy (FunctionSeman params rty)) loc) = do
  rty' <- foldType loc rty
  params' <- mapM (foldParam loc) params
  return $ SemanticAnn (FnTy (FunctionSeman params' rty')) loc

foldFieldValueAssignment :: FieldAssignment SemanticAnn -> ConstFoldMonad (FieldAssignment SemanticAnn)
foldFieldValueAssignment (FieldValueAssignment ident expr ann) = do
  ann' <- foldAnnotation ann
  expr' <- foldExpression expr
  case ann' of 
    (SemanticAnn (ETy (SimpleType exprTy)) exprLoc) -> 
      checkType exprLoc exprTy expr'
    _ -> return ()
  return $ FieldValueAssignment ident expr' ann'
foldFieldValueAssignment (FieldAddressAssignment ident expr ann) = do
  ann' <- foldAnnotation ann
  expr' <- foldExpression expr
  return $ FieldAddressAssignment ident expr' ann'
foldFieldValueAssignment (FieldPortConnection kind id1 id2 ann) = do
  let connLoc = getLocation ann
  ann' <- foldAnnotation ann
  case ann' of
    (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy _ portSize glbSize))) _) -> do
      case (portSize, glbSize) of
        (Constant (I (TInteger portSizeValue _) _) _, Constant (I (TInteger glbSizeValue _) _) _) -> do
          if portSizeValue == glbSizeValue then
            return ()
          else
            throwError $ annotateError connLoc (EAtomicArrayConnectionSizeMismatch portSizeValue glbSizeValue)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return ()
  return $ FieldPortConnection kind id1 id2 ann'

getArraySizeValue :: TerminaType SemanticAnn -> ConstFoldMonad Integer
getArraySizeValue (TArray _ arraySize) = do
  case arraySize of
    Constant (I (TInteger lhs _) _) _ -> return lhs
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
getArraySizeValue (TConstSubtype ty) = getArraySizeValue ty
getArraySizeValue _ = throwError $ annotateError Internal EInvalidConstantEvaluation

-- | This function checks the initialization expression of an array against its type.
-- The function assumes that the annotations have already been folded.
checkType :: Location -> TerminaType SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad ()
checkType loc array@(TArray ty _) initExpr@(ArrayInitializer assignmentExpr _ _) = do
  arraySizeValue <- getArraySizeValue array
  initExprType <- getExprType initExpr
  initExprSizeValue <- case initExprType of
    (TArray _ initExprSize) -> case initExprSize of
      Constant (I (TInteger rhs _) _) _ -> return rhs
      _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  if arraySizeValue == initExprSizeValue then
    return ()
  else
    throwError $ annotateError loc (EArrayInitializerSizeMismatch arraySizeValue initExprSizeValue)
  checkType loc ty assignmentExpr
checkType loc array@(TArray ty _) initExpr@(ArrayExprListInitializer assignmentExprs _) = do
  arraySizeValue <- getArraySizeValue array
  initExprType <- getExprType initExpr
  initExprSizeValue <- case initExprType of
    (TArray _ initExprSize) -> case initExprSize of
      Constant (I (TInteger rhs _) _) _ -> return rhs
      _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  if arraySizeValue == initExprSizeValue then
    return ()
  else
    throwError $ annotateError loc (EArrayInitializerSizeMismatch arraySizeValue initExprSizeValue)
  mapM_ (checkType loc ty) assignmentExprs
checkType loc array@(TArray _ _) initExpr@(StringInitializer {}) = do
  arraySizeValue <- getArraySizeValue array
  initExprType <- getExprType initExpr
  initExprSizeValue <- case initExprType of
    (TArray _ initExprSize) -> case initExprSize of
      Constant (I (TInteger rhs _) _) _ -> return rhs
      _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  if arraySizeValue >= initExprSizeValue then
    return ()
  else
    throwError $ annotateError loc (EStringInitializerInvalidSize arraySizeValue initExprSizeValue)
checkType loc ty expr = do
  exprType <- getExprType expr
  checkSameTy loc ty exprType

  where

    checkSameTy :: Location -> TerminaType SemanticAnn -> TerminaType SemanticAnn -> ConstFoldMonad ()
    checkSameTy loc' (TReference _ lhsArray@(TArray lhsTy _)) (TReference _ rhsArray@(TArray rhsTy _)) = do
      lhsArraySizeValue <- getArraySizeValue lhsArray
      rhsArraySizeValue <- getArraySizeValue rhsArray
      if lhsArraySizeValue == rhsArraySizeValue then
        checkSameTy loc' lhsTy rhsTy
      else
        throwError $ annotateError loc' (EReferencedArraySizeMismatch lhsArraySizeValue rhsArraySizeValue)
    checkSameTy loc' lhsArray@(TArray lhsTy _) rhsArray@(TArray rhsTy _) = do
      lhsArraySizeValue <- getArraySizeValue lhsArray
      rhsArraySizeValue <- getArraySizeValue rhsArray
      if lhsArraySizeValue == rhsArraySizeValue then
        checkSameTy loc' lhsTy rhsTy
      else
        throwError $ annotateError loc' (EReferencedArraySizeMismatch lhsArraySizeValue rhsArraySizeValue)
    checkSameTy _ _ _ = return ()

-- | Folding rebuilds the AST, so it gives the shared rewriter what to do with
-- the three things a node holds and only writes out the nodes it checks.
folding :: Rewriter ConstFoldMonad SemanticAnn
folding = Rewriter foldExpression foldObject foldAnnotation

foldObject :: Object SemanticAnn -> ConstFoldMonad (Object SemanticAnn)
-- | An index that is a constant expression is checked against the size of the
-- array, which is what the rest of the access path does not need.
foldObject (ArrayIndexExpression obj index ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  index' <- foldExpression index
  objType <- getObjType obj'
  indexExprType <- getExprType index'
  case (withoutConst objType, indexExprType) of
    (array@(TArray {}), TConstSubtype _) -> do
      arraySizeValue <- getArraySizeValue array
      exprValue <- evalConstExpression index'
      case exprValue of
        (I (TInteger indexValue _) _) -> do
          when (indexValue >= arraySizeValue) $ 
            throwError $ annotateError (getLocation . getAnnotation $ obj) (EArrayIndexOutOfBounds arraySizeValue indexValue)
          return $ ArrayIndexExpression obj' index' ann'
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return $ ArrayIndexExpression obj' index' ann'
foldObject obj = rewriteObject folding obj


foldExpression :: Expression SemanticAnn -> ConstFoldMonad (Expression SemanticAnn)
-- | Two constant operands are folded into the constant they produce.
foldExpression e@(BinOp op (Constant lConst@(I {}) _) (Constant rConst@(I {}) _) ann) = do
  ann' <- foldAnnotation ann
  ty <- getExprType e
  plt <- ST.gets targetPlatform
  fConst <- evalBinOp plt (getLocation ann) op lConst rConst ty
  return $ Constant fConst ann'
foldExpression e@(BinOp op lhs rhs ann) = do
  ann' <- foldAnnotation ann
  lhs' <- foldExpression lhs
  rhs' <- foldExpression rhs
  case (lhs', rhs') of
    (Constant lConst@(I {}) _, Constant rConst@(I {}) _) -> do
      ty <- getExprType e
      plt <- ST.gets targetPlatform
      fConst <- evalBinOp plt (getLocation ann) op lConst rConst ty
      return $ Constant fConst ann'
    _ -> do
      checkComparison (getLocation ann) op lhs' rhs'
      case (op, rhs') of
        (BitwiseLeftShift, Constant (I (TInteger k _) _) _)  -> checkShiftAmount k
        (BitwiseRightShift, Constant (I (TInteger k _) _) _) -> checkShiftAmount k
        -- | A constant divisor of zero is rejected whatever the dividend.
        (Division, Constant (I (TInteger 0 _) _) _) ->
          throwError $ annotateError (getLocation ann) EConstDivisionByZero
        (Modulo, Constant (I (TInteger 0 _) _) _) ->
          throwError $ annotateError (getLocation ann) EConstDivisionByZero
        _ -> return ()
      return $ BinOp op lhs' rhs' ann'
  where
    checkShiftAmount :: Integer -> ConstFoldMonad ()
    checkShiftAmount k = do
      ty <- getExprType lhs
      plt <- ST.gets targetPlatform
      when (k >= shiftWidth plt ty) $
        throwError $ annotateError (getLocation ann) (EShiftAmountOutOfBounds (shiftWidth plt ty) k)
foldExpression (Casting expr ty ann) = do
  ann' <- foldAnnotation ann
  expr' <- foldExpression expr
  plt <- ST.gets targetPlatform
  case expr' of
    -- | We only fold integer-to-integer casts, where the result is exact and
    -- thus trivially uniform with C. Any cast involving a floating-point type
    -- (int->float or float->int) is left for the C compiler, so we never
    -- compute a float value statically that could diverge from the target's
    -- IEEE-754 behaviour.
    Constant (I (TInteger i repr) _) _ | intTy ty ->
      if memberIntCons plt i ty then
        return $ Constant (I (TInteger i repr) (Just ty)) ann'
      else
        throwError $ annotateError (getLocation ann) (EConstIntegerOverflow i ty)
    _ -> return $ Casting expr' ty ann'
foldExpression (FunctionCall ident args ann) = do
  ann' <- foldAnnotation ann
  args' <- mapM foldExpression args
  checkCallArgs ann' args'
  return $ FunctionCall ident args' ann'
foldExpression (MemberFunctionCall obj ident args ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  args' <- mapM foldExpression args
  checkCallArgs ann' args'
  return $ MemberFunctionCall obj' ident args' ann'
foldExpression (DerefMemberFunctionCall obj ident args ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  args' <- mapM foldExpression args
  checkCallArgs ann' args'
  return $ DerefMemberFunctionCall obj' ident args' ann'
foldExpression (ArraySliceExpression ak obj lower upper ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  exprTy <- case ann' of
    (SemanticAnn (ETy (SimpleType ty)) _) -> return ty
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  objTy <- getObjType obj'
  lower' <- foldExpression lower
  upper' <- foldExpression upper
  lowerType <- getExprType lower
  upperType <- getExprType upper
  case (exprTy, objTy, lowerType, upperType) of
    -- | If both lower and upper are constant expressions, check if the slice is valid.
    (TReference _ expectedSlice, array, TConstSubtype _, TConstSubtype _) -> do
      arraySizeValue <- getArraySizeValue array
      expectedSliceSizeValue <- getArraySizeValue expectedSlice
      lowerValue <- evalConstExpression lower
      upperValue <- evalConstExpression upper
      case (lowerValue, upperValue) of
        (I (TInteger lowerIndex _) _, I (TInteger upperIndex _) _) -> do
          if upperIndex > arraySizeValue then
            throwError $ annotateError (getLocation ann) (EArraySliceOutOfBounds arraySizeValue upperIndex)
          else if lowerIndex > upperIndex then
            throwError $ annotateError (getLocation ann) (EArraySliceNegativeRange lowerIndex upperIndex)
          else when (expectedSliceSizeValue /= (upperIndex - lowerIndex)) $ throwError $ annotateError (getLocation ann) (EArraySliceInvalidRange arraySizeValue lowerIndex upperIndex)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return ()
  return $ ArraySliceExpression ak obj' lower' upper' ann'
-- | A struct initializer checks the value of each field against its type.
foldExpression (StructInitializer fvas ann) = do
  ann' <- foldAnnotation ann
  fvas' <- mapM foldFieldValueAssignment fvas
  return $ StructInitializer fvas' ann'
-- | Every other expression is rebuilt from its folded children.
foldExpression expr = rewriteExpression folding expr

foldStatement :: Statement SemanticAnn -> ConstFoldMonad (Statement SemanticAnn)
foldStatement (Declaration ident ak ty initExpr ann) = do
  let stmtLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType (getLocation ann) ty
  initExpr' <- mapM foldExpression initExpr
  mapM_ (checkType stmtLoc ty') initExpr'
  return $ Declaration ident ak ty' initExpr' ann'
foldStatement (AssignmentStmt obj expr ann) = do
  let stmtLoc = getLocation ann
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  expr' <- foldExpression expr
  objType <- getObjType obj'
  checkType stmtLoc objType expr'
  return $ AssignmentStmt obj' expr' ann'
foldStatement (SingleExpStmt expr ann) = do
  ann' <- foldAnnotation ann
  expr' <- foldExpression expr
  return $ SingleExpStmt expr' ann'

-- | Checks that the result of a relational comparison between an integer
-- expression and a constant depends on the value of the expression. It does
-- not when the constant is at or beyond the limits of the range of the type of
-- the expression, e.g., when an unsigned expression is checked to be less than
-- zero.
checkComparison :: Location -> Op -> Expression SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad ()
checkComparison loc op lhs rhs =
  case op of
    RelationalLT -> againstBounds
    RelationalLTE -> againstBounds
    RelationalGT -> againstBounds
    RelationalGTE -> againstBounds
    _ -> return ()

  where

    againstBounds = do
      lhsType <- getExprType lhs
      rhsType <- getExprType rhs
      case (lhsType, rhsType) of
        (TConstSubtype _, TConstSubtype _) -> return ()
        (_, TConstSubtype _) -> checkBounds op lhsType rhs
        (TConstSubtype _, _) -> checkBounds (swapOperands op) rhsType lhs
        _ -> return ()

    -- | Operator that yields the same result when the operands are swapped.
    swapOperands :: Op -> Op
    swapOperands RelationalLT = RelationalGT
    swapOperands RelationalLTE = RelationalGTE
    swapOperands RelationalGT = RelationalLT
    swapOperands RelationalGTE = RelationalLTE
    swapOperands o = o

    -- | Checks the comparison (e op' constExpr), where e is of type ty.
    checkBounds :: Op -> TerminaType SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad ()
    checkBounds op' ty constExpr = do
      plt <- ST.gets targetPlatform
      case intRange plt ty of
        Nothing -> return ()
        Just (lo, hi) -> do
          value <- evalConstExpression constExpr
          case value of
            I (TInteger c _) _ ->
              mapM_
                (throwError . annotateError loc . EInvariantComparison c ty)
                (fixedResult op' lo hi c)
            _ -> return ()

    -- | Result of (e op' c) for every value of e in [lo, hi], if it is the same
    -- for all of them.
    fixedResult :: Op -> Integer -> Integer -> Integer -> Maybe Bool
    fixedResult op' lo hi c =
      case op' of
        RelationalLT -> decide (c <= lo) (c > hi)
        RelationalLTE -> decide (c < lo) (c >= hi)
        RelationalGT -> decide (c >= hi) (c < lo)
        RelationalGTE -> decide (c > hi) (c <= lo)
        _ -> Nothing

    -- | The two ways the comparison can turn out the same for every value of
    -- the range, and the silence that is left when neither of them holds.
    decide :: Bool -> Bool -> Maybe Bool
    decide alwaysFalse alwaysTrue =
      case (alwaysFalse, alwaysTrue) of
        (True, _) -> Just False
        (_, True) -> Just True
        _ -> Nothing

foldBasicBlock :: BasicBlock SemanticAnn -> ConstFoldMonad (BasicBlock SemanticAnn)
foldBasicBlock (RegularBlock stmts) =
  RegularBlock <$> mapM foldStatement stmts
foldBasicBlock (IfElseBlock ifCond elifs mElse ann) = do
  ann' <- foldAnnotation ann
  ifCond' <- consSimplIfBlock ifCond
  elifs' <- mapM foldElseIfBlock elifs
  mElse' <- mapM foldElseBlock mElse
  return $ IfElseBlock ifCond' elifs' mElse' ann'

  where


    consSimplIfBlock :: CondIf SemanticAnn -> ConstFoldMonad (CondIf SemanticAnn)
    consSimplIfBlock (CondIf cond blk ann') = do
      ann'' <- foldAnnotation ann'
      cond' <- foldExpression cond
      blk' <- foldBasicBlocks blk
      return $ CondIf cond' blk' ann''
    
    foldElseIfBlock :: CondElseIf SemanticAnn -> ConstFoldMonad (CondElseIf SemanticAnn)
    foldElseIfBlock (CondElseIf elifCond blk ann') = do
      ann'' <- foldAnnotation ann'
      blk' <- foldBasicBlocks blk
      elifCond' <- foldExpression elifCond
      return $ CondElseIf elifCond' blk' ann''
    
    foldElseBlock :: CondElse SemanticAnn -> ConstFoldMonad (CondElse SemanticAnn)
    foldElseBlock (CondElse blk ann') = do
      ann'' <- foldAnnotation ann'
      blk' <- foldBasicBlocks blk
      return $ CondElse blk' ann''

foldBasicBlock (ForLoopBlock iter ty from_expr to_expr mWhile body_stmt ann) = do
  let stmtLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType stmtLoc ty
  from_expr' <- foldExpression from_expr
  to_expr' <- foldExpression to_expr
  body_stmt' <- foldBasicBlocks body_stmt
  mWhile' <- mapM foldExpression mWhile
  fromValue <- evalConstExpression from_expr
  toValue <- evalConstExpression to_expr
  case (fromValue, toValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        throwError $ annotateError stmtLoc EForLoopStatementZeroIterations
      else when (lhs > rhs)
        (throwError $ annotateError stmtLoc (EForLoopStatementNegativeIterations lhs rhs))
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  return $ ForLoopBlock iter ty' from_expr' to_expr' mWhile' body_stmt' ann'
foldBasicBlock (MatchBlock expr cases mDefaultCase ann) = do
  expr' <- foldExpression expr
  cases' <- mapM foldCase cases
  mDefaultCase' <- maybe (return Nothing) foldDefaultCase mDefaultCase
  return $ MatchBlock expr' cases' mDefaultCase' ann

  where

    foldDefaultCase :: DefaultCase SemanticAnn -> ConstFoldMonad (Maybe (DefaultCase SemanticAnn))
    foldDefaultCase (DefaultCase blk ann') = do
      ann'' <- foldAnnotation ann'
      blk' <- foldBasicBlocks blk
      return . Just $ DefaultCase blk' ann''

    foldCase :: MatchCase SemanticAnn -> ConstFoldMonad (MatchCase SemanticAnn)
    foldCase (MatchCase variantId vars blk ann') = do
      blk' <- foldBasicBlocks blk
      return $ MatchCase variantId vars blk' ann'

foldBasicBlock (SendMessage obj expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  expr' <- foldExpression expr
  return $ SendMessage obj' expr' ann'
foldBasicBlock (ProcedureInvoke obj procName exprs ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  exprs' <- mapM foldExpression exprs
  checkCallArgs ann' exprs'
  return $ ProcedureInvoke obj' procName exprs' ann'
foldBasicBlock (AtomicLoad obj expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  expr' <- foldExpression expr
  return $ AtomicLoad obj' expr' ann'
foldBasicBlock (AtomicStore obj expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  expr' <- foldExpression expr
  return $ AtomicStore obj' expr' ann'
foldBasicBlock (AtomicArrayLoad obj indexExpr expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  index' <- foldExpression indexExpr
  expr' <- foldExpression expr
  objType <- getObjType obj'
  indexExprType <- getExprType index'
  case (objType, indexExprType) of
    (TAccessPort (TAtomicArrayAccess _ arraySizeExpr), TConstSubtype _) -> do
      arraySizeExprValue <- evalConstExpression arraySizeExpr
      indexExprValue <- evalConstExpression index'
      case (arraySizeExprValue, indexExprValue) of
        (I (TInteger arraySizeValue _) _, I (TInteger indexValue _) _) -> do
          when (indexValue >= arraySizeValue) $ 
            throwError $ annotateError (getLocation . getAnnotation $ obj) (EAtomicArrayIndexOutOfBounds arraySizeValue indexValue)
          return $ AtomicArrayLoad obj' index' expr' ann'
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return $ AtomicArrayLoad obj' index' expr' ann'
foldBasicBlock (AtomicArrayStore obj indexExpr expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  index' <- foldExpression indexExpr
  expr' <- foldExpression expr
  objType <- getObjType obj'
  indexExprType <- getExprType index'
  case (objType, indexExprType) of
    (TAccessPort (TAtomicArrayAccess _ arraySizeExpr), TConstSubtype _) -> do
      arraySizeExprValue <- evalConstExpression arraySizeExpr
      indexExprValue <- evalConstExpression index'
      case (arraySizeExprValue, indexExprValue) of
        (I (TInteger arraySizeValue _) _, I (TInteger indexValue _) _) -> do
          when (indexValue >= arraySizeValue) $ 
            throwError $ annotateError (getLocation . getAnnotation $ obj) (EAtomicArrayIndexOutOfBounds arraySizeValue indexValue)
          return $ AtomicArrayStore obj' index' expr' ann'
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return $ AtomicArrayStore obj' index' expr' ann'
foldBasicBlock (AllocBox obj expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  expr' <- foldExpression expr
  return $ AllocBox obj' expr' ann'
foldBasicBlock (FreeBox obj expr ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  expr' <- foldExpression expr
  return $ FreeBox obj' expr' ann'
foldBasicBlock (ReturnBlock Nothing ann) = do
  ann' <- foldAnnotation ann
  return $ ReturnBlock Nothing ann'
foldBasicBlock (ReturnBlock (Just expr) ann) = do
  ann' <- foldAnnotation ann
  expr' <- foldExpression expr
  return $ ReturnBlock (Just expr') ann'
foldBasicBlock (ContinueBlock expr ann) = do
  ann' <- foldAnnotation ann
  expr' <- foldExpression expr
  return $ ContinueBlock expr' ann'
foldBasicBlock (RebootBlock ann) = do
  ann' <- foldAnnotation ann
  return $ RebootBlock ann'
foldBasicBlock (SystemCall obj ident exprs ann) = do
  ann' <- foldAnnotation ann
  obj' <- foldObject obj
  exprs' <- mapM foldExpression exprs
  checkCallArgs ann' exprs'
  return $ SystemCall obj' ident exprs' ann'

-- | Check the arguments of a call against the parameters its annotation
-- carries. A call to a function or a member is an expression; a procedure
-- called through an access port, including the system port, is a block of its
-- own, so both foldExpression and foldBasicBlock call this.
checkCallArgs :: SemanticAnn -> [Expression SemanticAnn] -> ConstFoldMonad ()
checkCallArgs (SemanticAnn (ETy (AppType params _ty)) exprLoc) args =
  zipWithM_ (\param arg -> case param of
    Parameter _ paramTy _ -> checkType exprLoc paramTy arg) params args
checkCallArgs _ _ = throwError $ annotateError Internal EInvalidConstantEvaluation

foldBasicBlocks :: Block SemanticAnn -> ConstFoldMonad (Block SemanticAnn)
foldBasicBlocks (Block body ann) = do
  body' <- mapM foldBasicBlock body
  return $ Block body' ann


foldFieldDefinition :: FieldDefinition SemanticAnn -> ConstFoldMonad (FieldDefinition SemanticAnn)
foldFieldDefinition (FieldDefinition name ty ann) = do
  let defLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType defLoc ty
  return $ FieldDefinition name ty' ann'

foldTypeDef :: Location -> TypeDef SemanticAnn -> ConstFoldMonad (TypeDef SemanticAnn)
foldTypeDef _loc (Struct ident fieldDefs mods) = do
  fieldDefs' <- mapM foldFieldDefinition fieldDefs
  return $ Struct ident fieldDefs' mods
foldTypeDef loc (Enum ident variants mods) = do
  variants' <- mapM foldEnumVariant variants
  return $ Enum ident variants' mods

  where 

    foldEnumVariant :: EnumVariant SemanticAnn -> ConstFoldMonad (EnumVariant SemanticAnn)
    foldEnumVariant (EnumVariant variantId tys) = do
      tys' <- mapM (foldType loc) tys
      return $ EnumVariant variantId tys'

foldTypeDef loc (Class ck classId members provides mods) = do
  members' <- mapM foldClassMember members
  return $ Class ck classId members' provides mods

  where

    foldClassMember :: ClassMember SemanticAnn -> ConstFoldMonad (ClassMember SemanticAnn)
    foldClassMember (ClassField fdef) = 
      ClassField <$> foldFieldDefinition fdef
    foldClassMember (ClassMethod ak ident params mrty body ann) = do
      params' <- mapM (foldParam loc) params
      mrty' <- mapM (foldType loc) mrty
      body' <- foldBasicBlocks body
      return $ ClassMethod ak ident params' mrty' body' ann
    foldClassMember (ClassProcedure ak ident params body ann) = do
      params' <- mapM (foldParam loc) params
      body' <- foldBasicBlocks body
      return $ ClassProcedure ak ident params' body' ann
    foldClassMember (ClassViewer ident params mrty body ann) = do
      params' <- mapM (foldParam loc) params
      mrty' <- mapM (foldType loc) mrty
      body' <- foldBasicBlocks body
      return $ ClassViewer ident params' mrty' body' ann
    foldClassMember (ClassAction ak ident params rty body ann) = do
      params' <- mapM (foldParam loc) params
      rty' <- foldType loc rty
      body' <- foldBasicBlocks body
      return $ ClassAction ak ident params' rty' body' ann
    
foldTypeDef _loc (Interface ik ident extends procs mods) = do
  procs' <- mapM foldInterfaceMember procs
  return $ Interface ik ident extends procs' mods

foldGlobal :: Global SemanticAnn -> ConstFoldMonad (Global SemanticAnn)
foldGlobal (Resource ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType glbLoc ty
  mInitExpr' <- mapM foldExpression mInitExpr
  return $ Resource ident ty' mInitExpr' mods ann'
foldGlobal (Task ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType glbLoc ty
  mInitExpr' <- mapM foldExpression mInitExpr
  mods' <- mapM foldModifier mods
  forM_ mods' $ \case
    Modifier "priority" (Just (Constant (I (TInteger priority _) _) _)) ->
      unless (minTaskPriority <= priority && priority <= maxTaskPriority) $
        throwError $ annotateError glbLoc (ETaskPriorityOutOfRange ident priority)
    _ -> return ()
  return $ Task ident ty' mInitExpr' mods' ann'
foldGlobal (Handler ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType glbLoc ty
  mInitExpr' <- mapM foldExpression mInitExpr
  return $ Handler ident ty' mInitExpr' mods ann'
foldGlobal (Const identifier ty expr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType glbLoc ty
  expr' <- foldConstInitializer ty' expr
  -- | Record the constant in the environment so that later elements (and
  -- later modules, since the environment is threaded across them) can resolve
  -- references to it.
  let scalar = withoutConst ty'
      entry = case expr' of
        Constant value _ ->
          if intTy scalar || boolTy scalar
            then ConstValue value
            else ConstInitializer expr'
        _ -> ConstInitializer expr'
  ST.modify $ \st -> st { constEnv = M.insert identifier entry (constEnv st) }
  return $ Const identifier ty' expr' mods ann'
foldGlobal (Channel ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType glbLoc ty
  mInitExpr' <- mapM foldExpression mInitExpr
  return $ Channel ident ty' mInitExpr' mods ann'
foldGlobal (Emitter ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- foldAnnotation ann
  ty' <- foldType glbLoc ty
  mInitExpr' <- mapM foldExpression mInitExpr
  case (ty', mInitExpr') of
    (TGlobal EmitterClass "PeriodicTimer",
     Just (StructInitializer [FieldValueAssignment "period" (StructInitializer fields _) _] _)) -> do
      seconds <- periodField "tv_sec" fields
      microseconds <- periodField "tv_usec" fields
      tick <- ST.gets tickMicroseconds
      plt <- ST.gets targetPlatform
      let period = seconds * 1000000 + microseconds
      unless (period > 0 && period `mod` tick == 0) $
        throwError $ annotateError glbLoc (ETimerPeriodNotInTicks ident period tick)
      forM_ (maxTimerPeriodTicks plt) $ \maxTicks ->
        when (period `div` tick > maxTicks) $
          throwError $ annotateError glbLoc (ETimerPeriodTooLong ident (period `div` tick) maxTicks)
    _ -> return ()
  return $ Emitter ident ty' mInitExpr' mods ann'

  where

    -- | The value of a field of the period, which the type checker requires
    -- to be a constant expression of type u32.
    periodField name fields =
      case [expr | FieldValueAssignment field expr _ <- fields, field == name] of
        [expr] -> evalConstExpression expr >>= \case
          I (TInteger value _) _ -> return value
          _ -> throwError $ annotateError Internal EInvalidTimerPeriod
        _ -> throwError $ annotateError Internal EInvalidTimerPeriod
foldGlobal g = return g -- This should not happen

-- | The initializer of a global constant, worked out down to each scalar it
-- holds, since C only admits a constant expression as the initializer of an
-- object with static storage. An integer or a boolean is computed here, with
-- the checks of its operations; a floating-point value or a character is left
-- to the C compiler. A reference to another constant is replaced by its value
-- or by its folded initializer.
foldConstInitializer :: TerminaType SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad (Expression SemanticAnn)
foldConstInitializer (TConstSubtype ty) expr = foldConstInitializer ty expr
foldConstInitializer (TArray elemTy _) (ArrayInitializer value size ann) = do
  ann' <- foldAnnotation ann
  value' <- foldConstInitializer elemTy value
  size' <- foldExpression size
  return $ ArrayInitializer value' size' ann'
foldConstInitializer (TArray elemTy _) (ArrayExprListInitializer exprs ann) = do
  ann' <- foldAnnotation ann
  exprs' <- mapM (foldConstInitializer elemTy) exprs
  return $ ArrayExprListInitializer exprs' ann'
foldConstInitializer _ (StructInitializer fields ann) = do
  ann' <- foldAnnotation ann
  fields' <- forM fields $ \case
    FieldValueAssignment ident expr fieldAnn -> do
      fieldAnn' <- foldAnnotation fieldAnn
      expr' <- foldConstComponent expr
      return $ FieldValueAssignment ident expr' fieldAnn'
    field -> return field
  return $ StructInitializer fields' ann'
foldConstInitializer _ (EnumVariantInitializer enum variant args ann) = do
  ann' <- foldAnnotation ann
  args' <- mapM foldConstComponent args
  return $ EnumVariantInitializer enum variant args' ann'
foldConstInitializer _ (MonadicVariantInitializer variant ann) = do
  ann' <- foldAnnotation ann
  variant' <- case variant of
    Some expr -> Some <$> foldConstComponent expr
    Ok expr -> Ok <$> foldConstComponent expr
    Error expr -> Error <$> foldConstComponent expr
    Failure expr -> Failure <$> foldConstComponent expr
    None -> return None
    Success -> return Success
  return $ MonadicVariantInitializer variant' ann'
foldConstInitializer ty expr =
  if intTy ty || boolTy ty
    then do
      value <- evalConstExpression expr
      return $ Constant value (buildExpAnn (getLocation (getAnnotation expr)) ty)
    else foldExpression expr >>= substituteConstants

-- | A component of an aggregate constant initializer, folded with its own type.
foldConstComponent :: Expression SemanticAnn -> ConstFoldMonad (Expression SemanticAnn)
foldConstComponent expr = do
  ty <- getExprType expr
  foldConstInitializer ty expr

-- | Replaces every reference to a constant in an expression by its value or by
-- its folded initializer.
substituteConstants :: Expression SemanticAnn -> ConstFoldMonad (Expression SemanticAnn)
substituteConstants (AccessObject obj) = constInitializerOf obj
substituteConstants expr = rewriteExpression (Rewriter substituteConstants return return) expr

-- | Replaces the argument of a modifier by the value it folds to, so the
-- architecture reads a literal whether the source wrote a literal, a constant
-- or an expression of constants.
foldModifier :: Modifier SemanticAnn -> ConstFoldMonad (Modifier SemanticAnn)
foldModifier (Modifier ident (Just expr)) = do
  value <- evalConstExpression expr
  return $ Modifier ident (Just (Constant value (getAnnotation expr)))
foldModifier modifier = return modifier

foldElement :: AnnASTElement SemanticAnn -> ConstFoldMonad (AnnASTElement SemanticAnn)
foldElement (GlobalDeclaration g) =
  GlobalDeclaration <$> foldGlobal g
foldElement (TypeDefinition td ann) = do
  let tdLoc = getLocation ann
  ann' <- foldAnnotation ann
  td' <- foldTypeDef tdLoc td
  return $ TypeDefinition td' ann'
foldElement (Function ident params mrty body mods ann) = do
  let funLoc = getLocation ann
  ann' <- foldAnnotation ann
  params' <- mapM (foldParam funLoc) params
  mrty' <- mapM (foldType funLoc) mrty
  body' <- foldBasicBlocks body
  return $ Function ident params' mrty' body' mods ann'

constFoldModule :: BasicBlocksModule -> ConstFoldMonad BasicBlocksModule
constFoldModule (TerminaModuleData modQualifiedName modFullPath 
    modModificationTime modImportedModules modVisibleModules modSourcecode (BasicBlockData ast)) =
    TerminaModuleData modQualifiedName modFullPath 
        modModificationTime modImportedModules modVisibleModules modSourcecode . BasicBlockData <$> mapM foldElement ast

runConstFolding
  :: ConstFoldEnv
  -> ConstFoldMonad a
  -> Either ConstFoldError (a, ConstFoldEnv)
runConstFolding initSt m = case flip ST.runState initSt . runExceptT $ m of
  (Left err, _) -> Left err
  (Right output, st) -> Right (output, st)
