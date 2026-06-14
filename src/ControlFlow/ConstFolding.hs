module ControlFlow.ConstFolding where

import Semantic.Types
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import ControlFlow.ConstFolding.Monad
import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import ControlFlow.ConstFolding.Utils
import Control.Monad
import Core.Utils
import Command.Types
import Modules.Modules

evalConstObject :: Object SemanticAnn -> ConstFoldMonad (Const SemanticAnn)
evalConstObject (Variable ident _) = do
  globalEnv <- ST.gets constEnv
  case M.lookup ident globalEnv of
    Just expr -> return expr
    Nothing -> throwError $ annotateError Internal (EUnknownIdentifier ident)
evalConstObject _ = throwError $ annotateError Internal ENotConstant

evalConstExpression :: Expression SemanticAnn -> ConstFoldMonad (Const SemanticAnn)
evalConstExpression (AccessObject obj) = do
  evalConstObject obj
evalConstExpression (Constant c _) = return c
evalConstExpression expr@(BinOp op lhs rhs ann) = do
  lhs' <- evalConstExpression lhs
  rhs' <- evalConstExpression rhs
  ty <- getExprType expr
  case (lhs', rhs') of
    (c1, c2) -> evalBinOp (getLocation ann) op c1 c2 ty
evalConstExpression (Casting expr' ty _) = do
  constExpr <- evalConstExpression expr'
  case constExpr of
    (I constValue _) -> do
      return $ I constValue (Just ty)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
evalConstExpression _ = throwError $ annotateError Internal ENotConstant

-- | Evaluates a type. This basically only applies to arrays. The function
-- returns the same expression but with the types (i.e., the array sizes)
-- evaluated.
constFoldType :: Location -> TerminaType SemanticAnn -> ConstFoldMonad (TerminaType SemanticAnn)
constFoldType loc (TArray ty arraySize)= do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- constFoldType loc ty
  return (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize)))
constFoldType loc (TAtomicArray ty arraySize) = do
  arraySizeValue <- evalConstExpression arraySize
  return (TAtomicArray ty (Constant arraySizeValue (buildExpAnn loc TUSize)))
constFoldType loc (TFixedLocation (TArray ty arraySize)) = do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- constFoldType loc ty
  return (TFixedLocation (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize))))
constFoldType _ ty = return ty

constFoldParam :: Location -> Parameter SemanticAnn -> ConstFoldMonad (Parameter SemanticAnn)
constFoldParam loc (Parameter name ty) = 
  Parameter name <$> constFoldType loc ty

constFoldInterfaceMember :: InterfaceMember SemanticAnn -> ConstFoldMonad (InterfaceMember SemanticAnn)
constFoldInterfaceMember (InterfaceProcedure ak procId params mods ann) = do
  params' <- mapM (constFoldParam (getLocation ann)) params
  ann' <- constFoldAnnotation ann
  return $ InterfaceProcedure ak procId params' mods ann'

constFoldAnnotation :: SemanticAnn -> ConstFoldMonad SemanticAnn
constFoldAnnotation (SemanticAnn (ETy (SimpleType ty)) exprLoc) = do
  ty' <- constFoldType exprLoc ty
  return $ SemanticAnn (ETy (SimpleType ty')) exprLoc
constFoldAnnotation (SemanticAnn (ETy (ObjectType ak ty)) exprLoc) = do
  ty' <- constFoldType exprLoc ty
  return $ SemanticAnn (ETy (ObjectType ak ty')) exprLoc
constFoldAnnotation (SemanticAnn (ETy (AccessPortObjType ak fields ty)) exprLoc) = do
  ty' <- constFoldType exprLoc ty
  return $ SemanticAnn (ETy (AccessPortObjType ak fields ty')) exprLoc
constFoldAnnotation (SemanticAnn (ETy (AppType params ty)) exprLoc) = do
  ty' <- constFoldType exprLoc ty
  params' <- mapM (constFoldParam exprLoc) params
  return $ SemanticAnn (ETy (AppType params' ty')) exprLoc
constFoldAnnotation ann@(SemanticAnn (FTy SimpleField) _) = return ann
constFoldAnnotation (SemanticAnn (FTy (AccessPortField ifaces)) exprLoc) = do
  ifaces' <- mapM constFoldInterfaceMember ifaces
  return $ SemanticAnn (FTy (AccessPortField ifaces')) exprLoc
constFoldAnnotation ann@(SemanticAnn (STy SimpleStmtType) _) = return ann
constFoldAnnotation (SemanticAnn (STy (MatchCaseStmtType tys)) exprLoc) = do
  tys' <- mapM (constFoldType exprLoc) tys
  return $ SemanticAnn (STy (MatchCaseStmtType tys')) exprLoc
constFoldAnnotation (SemanticAnn (STy (PortConnection (APConnTy pty resTy procs))) loc) = do
  pty' <- constFoldType loc pty
  resTy' <- constFoldType loc resTy
  procs' <- mapM (\(ProcedureSeman ident params mods) -> do
    params' <- mapM (constFoldParam loc) params
    return $ ProcedureSeman ident params' mods) procs
  return $ SemanticAnn (STy (PortConnection (APConnTy pty' resTy' procs'))) loc
constFoldAnnotation (SemanticAnn (STy (PortConnection (APAtomicConnTy ty))) loc) = do
  ty' <- constFoldType loc ty
  return $ SemanticAnn (STy (PortConnection (APAtomicConnTy ty'))) loc
constFoldAnnotation (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ty arrSize arrPortSize))) loc) = do
  ty' <- constFoldType loc ty
  arraySizeValue <- evalConstExpression arrSize
  arrayPortSizeValue <- evalConstExpression arrPortSize
  let arrSizeConst = Constant arraySizeValue (buildExpAnn loc TUSize)
      arrPortSizeValue = Constant arrayPortSizeValue (buildExpAnn loc TUSize)
  return $ SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ty' arrSizeConst arrPortSizeValue))) loc
constFoldAnnotation (SemanticAnn (STy (PortConnection (APPoolConnTy ty poolSize))) loc) = do
  ty' <- constFoldType loc ty
  poolSizeValue <- evalConstExpression poolSize
  let poolSizeConst = Constant poolSizeValue (buildExpAnn loc TUSize)
  return $ SemanticAnn (STy (PortConnection (APPoolConnTy ty' poolSizeConst))) loc
constFoldAnnotation (SemanticAnn (STy (PortConnection (SPConnTy ty ident))) loc) = do
  ty' <- constFoldType loc ty
  return $ SemanticAnn (STy (PortConnection (SPConnTy ty' ident))) loc
constFoldAnnotation (SemanticAnn (STy (PortConnection (InPConnTy ty ident))) loc) = do
  ty' <- constFoldType loc ty
  return $ SemanticAnn (STy (PortConnection (InPConnTy ty' ident))) loc
constFoldAnnotation (SemanticAnn (STy (PortConnection (OutPConnTy ty))) loc) = do
  ty' <- constFoldType loc ty
  return $ SemanticAnn (STy (PortConnection (OutPConnTy ty'))) loc
constFoldAnnotation (SemanticAnn (GTy ty) loc) = do
  ty' <- constFoldType loc ty
  return $ SemanticAnn (GTy ty') loc
constFoldAnnotation ann@(SemanticAnn TTy _) = return ann
constFoldAnnotation (SemanticAnn (FnTy (FunctionSeman params rty)) loc) = do
  rty' <- constFoldType loc rty
  params' <- mapM (constFoldParam loc) params
  return $ SemanticAnn (FnTy (FunctionSeman params' rty')) loc

constFoldFieldValueAssignment :: FieldAssignment SemanticAnn -> ConstFoldMonad (FieldAssignment SemanticAnn)
constFoldFieldValueAssignment (FieldValueAssignment ident expr ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  case ann' of 
    (SemanticAnn (ETy (SimpleType exprTy)) exprLoc) -> 
      constFoldCheckType exprLoc exprTy expr'
    _ -> return ()
  return $ FieldValueAssignment ident expr' ann'
constFoldFieldValueAssignment (FieldAddressAssignment ident expr ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  return $ FieldAddressAssignment ident expr' ann'
constFoldFieldValueAssignment (FieldPortConnection kind id1 id2 ann) = do
  let connLoc = getLocation ann
  ann' <- constFoldAnnotation ann
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
getArraySizeValue _ = throwError $ annotateError Internal EInvalidConstantEvaluation

-- | This function checks the initialization expression of an array against its type.
-- The function assumes that the annotations have already been folded.
constFoldCheckType :: Location -> TerminaType SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad ()
constFoldCheckType loc array@(TArray ty _) initExpr@(ArrayInitializer assignmentExpr _ _) = do
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
  constFoldCheckType loc ty assignmentExpr
constFoldCheckType loc array@(TArray ty _) initExpr@(ArrayExprListInitializer assignmentExprs _) = do
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
  mapM_ (constFoldCheckType loc ty) assignmentExprs
constFoldCheckType loc array@(TArray _ _) initExpr@(StringInitializer {}) = do
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
constFoldCheckType loc ty expr = do
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
    checkSameTy _ _ _ = return ()

constFoldObject :: Object SemanticAnn -> ConstFoldMonad (Object SemanticAnn)
constFoldObject (Variable ident ann) = do
  ann' <- constFoldAnnotation ann
  return $ Variable ident ann'
constFoldObject (ArrayIndexExpression obj index ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  index' <- constFoldExpression index
  objType <- getObjType obj'
  indexExprType <- getExprType index'
  case (objType, indexExprType) of
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
constFoldObject (MemberAccess obj ident ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ MemberAccess obj' ident ann'
constFoldObject (Dereference obj ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ Dereference obj' ann'
constFoldObject (DereferenceMemberAccess obj ident ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ DereferenceMemberAccess obj' ident ann'
constFoldObject (Unbox obj ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ Unbox obj' ann'


constFoldExpression :: Expression SemanticAnn -> ConstFoldMonad (Expression SemanticAnn)
constFoldExpression (Constant c ann) = do
  ann' <- constFoldAnnotation ann
  return $ Constant c ann'
constFoldExpression (StringInitializer str ann) = do
  ann' <- constFoldAnnotation ann
  return $ StringInitializer str ann'
constFoldExpression (IsEnumVariantExpression obj enum var ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ IsEnumVariantExpression obj' enum var ann'
constFoldExpression (IsMonadicVariantExpression obj label ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ IsMonadicVariantExpression obj' label ann'
constFoldExpression (MonadicVariantInitializer variant ann) = do
  ann' <- constFoldAnnotation ann
  variant' <- case variant of
    Some expr -> Some <$> constFoldExpression expr
    Ok expr -> Ok <$> constFoldExpression expr
    Error expr -> Error <$> constFoldExpression expr
    Failure expr -> Failure <$> constFoldExpression expr
    v -> return v
  return $ MonadicVariantInitializer variant' ann'
constFoldExpression e@(BinOp op (Constant lConst@(I {}) _) (Constant rConst@(I {}) _) ann) = do
  ann' <- constFoldAnnotation ann
  ty <- getExprType e
  fConst <- evalBinOp (getLocation ann) op lConst rConst ty
  return $ Constant fConst ann'
constFoldExpression e@(BinOp op lhs rhs ann) = do
  ann' <- constFoldAnnotation ann
  lhs' <- constFoldExpression lhs
  rhs' <- constFoldExpression rhs
  case (lhs', rhs') of
    (Constant lConst@(I {}) _, Constant rConst@(I {}) _) -> do
      ty <- getExprType e
      fConst <- evalBinOp (getLocation ann) op lConst rConst ty
      return $ Constant fConst ann
    _ -> return $ BinOp op lhs' rhs' ann'
constFoldExpression (Casting expr ty ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  case expr' of
    -- | We only fold integer-to-integer casts, where the result is exact and
    -- thus trivially uniform with C. Any cast involving a floating-point type
    -- (int->float or float->int) is left for the C compiler, so we never
    -- compute a float value statically that could diverge from the target's
    -- IEEE-754 behaviour.
    Constant (I (TInteger i repr) _) _ | intTy ty ->
      if memberIntCons i ty then
        return $ Constant (I (TInteger i repr) (Just ty)) ann
      else
        throwError $ annotateError (getLocation ann) (EConstIntegerOverflow i ty)
    _ -> return $ Casting expr' ty ann'
constFoldExpression (FunctionCall ident args ann) = do
  ann' <- constFoldAnnotation ann
  args' <- mapM constFoldExpression args
  case ann' of
    (SemanticAnn (ETy (AppType params _ty)) exprLoc) ->
      zipWithM_ (\param arg -> case param of
        Parameter _ paramTy -> constFoldCheckType exprLoc paramTy arg) params args'
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  return $ FunctionCall ident args' ann'
constFoldExpression (MemberFunctionCall obj ident args ann) = do
  ann' <- constFoldAnnotation ann
  args' <- mapM constFoldExpression args
  case ann' of
    (SemanticAnn (ETy (AppType params _ty)) exprLoc) ->
      zipWithM_ (\param arg -> case param of
        Parameter _ paramTy -> constFoldCheckType exprLoc paramTy arg) params args'
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  return $ MemberFunctionCall obj ident args' ann'
constFoldExpression (DerefMemberFunctionCall obj ident args ann) = do
  ann' <- constFoldAnnotation ann
  args' <- mapM constFoldExpression args
  case ann' of
    (SemanticAnn (ETy (AppType params _ty)) exprLoc) ->
      zipWithM_ (\param arg -> case param of
        Parameter _ paramTy -> constFoldCheckType exprLoc paramTy arg) params args'
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  return $ DerefMemberFunctionCall obj ident args' ann'
constFoldExpression (ArraySliceExpression ak obj lower upper ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  exprTy <- case ann' of
    (SemanticAnn (ETy (SimpleType ty)) _) -> return ty
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  objTy <- getObjType obj'
  lower' <- constFoldExpression lower
  upper' <- constFoldExpression upper
  lowerType <- getExprType lower
  upperType <- getExprType upper
  case (exprTy, objTy, lowerType, upperType) of
    -- | If both lower and upper are constant expressions, check if the slice is valid.
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
  return $ ArraySliceExpression ak obj lower' upper' ann'
constFoldExpression (EnumVariantInitializer enumId variantId exprs ann) = do
  ann' <- constFoldAnnotation ann
  exprs' <- mapM constFoldExpression exprs
  return $ EnumVariantInitializer enumId variantId exprs' ann'
constFoldExpression (StructInitializer fvas ann) = do
  ann' <- constFoldAnnotation ann
  fvas' <- mapM constFoldFieldValueAssignment fvas
  return $ StructInitializer fvas' ann'
constFoldExpression (ArrayInitializer expr ty ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  return $ ArrayInitializer expr' ty ann'
constFoldExpression (ArrayExprListInitializer exprs ann) = do
  ann' <- constFoldAnnotation ann
  exprs' <- mapM constFoldExpression exprs
  return $ ArrayExprListInitializer exprs' ann'
constFoldExpression (AccessObject obj) = AccessObject <$> constFoldObject obj
constFoldExpression (ReferenceExpression ak obj ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  return $ ReferenceExpression ak obj' ann'

constFoldStatement :: Statement SemanticAnn -> ConstFoldMonad (Statement SemanticAnn)
constFoldStatement (Declaration ident ak ty initExpr ann) = do
  let stmtLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType (getLocation ann) ty
  initExpr' <- constFoldExpression initExpr
  constFoldCheckType stmtLoc ty' initExpr'
  return $ Declaration ident ak ty' initExpr' ann'
constFoldStatement (AssignmentStmt obj expr ann) = do
  let stmtLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  expr' <- constFoldExpression expr
  objType <- getObjType obj'
  constFoldCheckType stmtLoc objType expr'
  return $ AssignmentStmt obj' expr' ann'
constFoldStatement (SingleExpStmt expr ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  return $ SingleExpStmt expr' ann'

constFoldCheckCondition :: Expression SemanticAnn -> ConstFoldMonad()
constFoldCheckCondition cond = do
  condExprType <- getExprType cond
  case condExprType of
    (TConstSubtype _) -> do
      value <- evalConstExpression cond
      throwError $ annotateError (getLocation . getAnnotation $ cond) (EConstCondition value)
    _ -> return ()

constFoldBasicBlock :: BasicBlock SemanticAnn -> ConstFoldMonad (BasicBlock SemanticAnn)
constFoldBasicBlock (RegularBlock stmts) =
  RegularBlock <$> mapM constFoldStatement stmts
constFoldBasicBlock (IfElseBlock ifCond elifs mElse ann) = do
  ann' <- constFoldAnnotation ann
  ifCond' <- consSimplIfBlock ifCond
  elifs' <- mapM constFoldElseIfBlock elifs
  mElse' <- maybe (return Nothing) (fmap Just . constFoldElseBlock) mElse
  return $ IfElseBlock ifCond' elifs' mElse' ann'

  where


    consSimplIfBlock :: CondIf SemanticAnn -> ConstFoldMonad (CondIf SemanticAnn)
    consSimplIfBlock (CondIf cond blk ann') = do
      ann'' <- constFoldAnnotation ann'
      cond' <- constFoldExpression cond
      blk' <- constFoldBasicBlocks blk
      constFoldCheckCondition cond'
      return $ CondIf cond' blk' ann''
    
    constFoldElseIfBlock :: CondElseIf SemanticAnn -> ConstFoldMonad (CondElseIf SemanticAnn)
    constFoldElseIfBlock (CondElseIf elifCond blk ann') = do
      ann'' <- constFoldAnnotation ann'
      blk' <- constFoldBasicBlocks blk
      elifCond' <- constFoldExpression elifCond
      constFoldCheckCondition elifCond'
      return $ CondElseIf elifCond' blk' ann''
    
    constFoldElseBlock :: CondElse SemanticAnn -> ConstFoldMonad (CondElse SemanticAnn)
    constFoldElseBlock (CondElse blk ann') = do
      ann'' <- constFoldAnnotation ann'
      blk' <- constFoldBasicBlocks blk
      return $ CondElse blk' ann''

constFoldBasicBlock (ForLoopBlock iter ty from_expr to_expr mWhile body_stmt ann) = do
  let stmtLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType stmtLoc ty
  from_expr' <- constFoldExpression from_expr
  to_expr' <- constFoldExpression to_expr
  body_stmt' <- constFoldBasicBlocks body_stmt
  mWhile' <- maybe (return Nothing) (fmap Just . constFoldExpression) mWhile
  fromValue <- evalConstExpression from_expr
  toValue <- evalConstExpression to_expr
  case (fromValue, toValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        throwError $ annotateError stmtLoc EForLoopStatementZeroIterations
      else if lhs > rhs then
        throwError $ annotateError stmtLoc (EForLoopStatementNegativeIterations lhs rhs)
      else do
        maybe (return ()) constFoldCheckCondition mWhile'
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  return $ ForLoopBlock iter ty' from_expr' to_expr' mWhile' body_stmt' ann'
constFoldBasicBlock (MatchBlock expr cases mDefaultCase ann) = do
  expr' <- constFoldExpression expr
  cases' <- mapM constFoldCase cases
  mDefaultCase' <- maybe (return Nothing) constFoldDefaultCase mDefaultCase
  return $ MatchBlock expr' cases' mDefaultCase' ann

  where

    constFoldDefaultCase :: DefaultCase SemanticAnn -> ConstFoldMonad (Maybe (DefaultCase SemanticAnn))
    constFoldDefaultCase (DefaultCase blk ann') = do
      ann'' <- constFoldAnnotation ann'
      blk' <- constFoldBasicBlocks blk
      return . Just $ DefaultCase blk' ann''

    constFoldCase :: MatchCase SemanticAnn -> ConstFoldMonad (MatchCase SemanticAnn)
    constFoldCase (MatchCase variantId vars blk ann') = do
      blk' <- constFoldBasicBlocks blk
      return $ MatchCase variantId vars blk' ann'

constFoldBasicBlock (SendMessage obj expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  expr' <- constFoldExpression expr
  return $ SendMessage obj' expr' ann'
constFoldBasicBlock (ProcedureInvoke obj procName exprs ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  exprs' <- mapM constFoldExpression exprs
  return $ ProcedureInvoke obj' procName exprs' ann'
constFoldBasicBlock (AtomicLoad obj expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  expr' <- constFoldExpression expr
  return $ AtomicLoad obj' expr' ann'
constFoldBasicBlock (AtomicStore obj expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  expr' <- constFoldExpression expr
  return $ AtomicStore obj' expr' ann'
constFoldBasicBlock (AtomicArrayLoad obj indexExpr expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  index' <- constFoldExpression indexExpr
  expr' <- constFoldExpression expr
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
constFoldBasicBlock (AtomicArrayStore obj indexExpr expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  index' <- constFoldExpression indexExpr
  expr' <- constFoldExpression expr
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
constFoldBasicBlock (AllocBox obj expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  expr' <- constFoldExpression expr
  return $ AllocBox obj' expr' ann'
constFoldBasicBlock (FreeBox obj expr ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  expr' <- constFoldExpression expr
  return $ FreeBox obj' expr' ann'
constFoldBasicBlock (ReturnBlock Nothing ann) = do
  ann' <- constFoldAnnotation ann
  return $ ReturnBlock Nothing ann'
constFoldBasicBlock (ReturnBlock (Just expr) ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  return $ ReturnBlock (Just expr') ann'
constFoldBasicBlock (ContinueBlock expr ann) = do
  ann' <- constFoldAnnotation ann
  expr' <- constFoldExpression expr
  return $ ContinueBlock expr' ann'
constFoldBasicBlock (RebootBlock ann) = do
  ann' <- constFoldAnnotation ann
  return $ RebootBlock ann'
constFoldBasicBlock (SystemCall obj ident exprs ann) = do
  ann' <- constFoldAnnotation ann
  obj' <- constFoldObject obj
  exprs' <- mapM constFoldExpression exprs
  return $ SystemCall obj' ident exprs' ann'

constFoldBasicBlocks :: Block SemanticAnn -> ConstFoldMonad (Block SemanticAnn)
constFoldBasicBlocks (Block body ann) = do
  body' <- mapM constFoldBasicBlock body
  return $ Block body' ann


constFoldFieldDefinition :: FieldDefinition SemanticAnn -> ConstFoldMonad (FieldDefinition SemanticAnn)
constFoldFieldDefinition (FieldDefinition name ty ann) = do
  let defLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType defLoc ty
  return $ FieldDefinition name ty' ann'

constFoldTypeDef :: Location -> TypeDef SemanticAnn -> ConstFoldMonad (TypeDef SemanticAnn)
constFoldTypeDef _loc (Struct ident fieldDefs mods) = do
  fieldDefs' <- mapM constFoldFieldDefinition fieldDefs
  return $ Struct ident fieldDefs' mods
constFoldTypeDef loc (Enum ident variants mods) = do
  variants' <- mapM constFoldEnumVariant variants
  return $ Enum ident variants' mods

  where 

    constFoldEnumVariant :: EnumVariant SemanticAnn -> ConstFoldMonad (EnumVariant SemanticAnn)
    constFoldEnumVariant (EnumVariant variantId tys) = do
      tys' <- mapM (constFoldType loc) tys
      return $ EnumVariant variantId tys'

constFoldTypeDef loc (Class ck classId members provides mods) = do
  members' <- mapM constFoldClassMember members
  return $ Class ck classId members' provides mods

  where

    constFoldClassMember :: ClassMember SemanticAnn -> ConstFoldMonad (ClassMember SemanticAnn)
    constFoldClassMember (ClassField fdef) = 
      ClassField <$> constFoldFieldDefinition fdef
    constFoldClassMember (ClassMethod ak ident params mrty body ann) = do
      params' <- mapM (constFoldParam loc) params
      mrty' <- maybe (return Nothing) (fmap Just . constFoldType loc) mrty
      body' <- constFoldBasicBlocks body
      return $ ClassMethod ak ident params' mrty' body' ann
    constFoldClassMember (ClassProcedure ak ident params body ann) = do
      params' <- mapM (constFoldParam loc) params
      body' <- constFoldBasicBlocks body
      return $ ClassProcedure ak ident params' body' ann
    constFoldClassMember (ClassViewer ident params mrty body ann) = do
      params' <- mapM (constFoldParam loc) params
      mrty' <- maybe (return Nothing) (fmap Just . constFoldType loc) mrty
      body' <- constFoldBasicBlocks body
      return $ ClassViewer ident params' mrty' body' ann
    constFoldClassMember (ClassAction ak ident params rty body ann) = do
      params' <- mapM (constFoldParam loc) params
      rty' <- constFoldType loc rty
      body' <- constFoldBasicBlocks body
      return $ ClassAction ak ident params' rty' body' ann
    
constFoldTypeDef _loc (Interface ik ident extends procs mods) = do
  procs' <- mapM constFoldInterfaceMember procs
  return $ Interface ik ident extends procs' mods

constFoldGlobal :: Global SemanticAnn -> ConstFoldMonad (Global SemanticAnn)
constFoldGlobal (Resource ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType glbLoc ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constFoldExpression) mInitExpr
  return $ Resource ident ty' mInitExpr' mods ann'
constFoldGlobal (Task ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType glbLoc ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constFoldExpression) mInitExpr
  return $ Task ident ty' mInitExpr' mods ann'
constFoldGlobal (Handler ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType glbLoc ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constFoldExpression) mInitExpr
  return $ Handler ident ty' mInitExpr' mods ann'
constFoldGlobal (Const identifier ty expr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType glbLoc ty
  expr' <- constFoldExpression expr
  -- | Record scalar constants in the environment so that later elements (and
  -- later modules, since the environment is threaded across them) can resolve
  -- references to them. Aggregate constants (arrays, structs) have no scalar
  -- 'Const' representation and are never folded into a value, so they are not
  -- recorded; a reference to one keeps accessing the emitted object.
  case expr' of
    Constant constValue _ ->
      ST.modify $ \st -> st { constEnv = M.insert identifier constValue (constEnv st) }
    _ -> return ()
  return $ Const identifier ty' expr' mods ann'
constFoldGlobal (Channel ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType glbLoc ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constFoldExpression) mInitExpr
  return $ Channel ident ty' mInitExpr' mods ann'
constFoldGlobal (Emitter ident ty mInitExpr mods ann) = do
  let glbLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  ty' <- constFoldType glbLoc ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constFoldExpression) mInitExpr
  return $ Emitter ident ty' mInitExpr' mods ann'
constFoldGlobal g = return g -- This should not happen

constFoldElement :: AnnASTElement SemanticAnn -> ConstFoldMonad (AnnASTElement SemanticAnn)
constFoldElement (GlobalDeclaration g) =
  GlobalDeclaration <$> constFoldGlobal g
constFoldElement (TypeDefinition td ann) = do
  let tdLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  td' <- constFoldTypeDef tdLoc td
  return $ TypeDefinition td' ann'
constFoldElement (Function ident params mrty body mods ann) = do
  let funLoc = getLocation ann
  ann' <- constFoldAnnotation ann
  params' <- mapM (constFoldParam funLoc) params
  mrty' <- maybe (return Nothing) (fmap Just . constFoldType funLoc) mrty
  body' <- constFoldBasicBlocks body
  return $ Function ident params' mrty' body' mods ann'

constFoldModule :: BasicBlocksModule -> ConstFoldMonad BasicBlocksModule
constFoldModule (TerminaModuleData modQualifiedName modFullPath 
    modModificationTime modImportedModules modVisibleModules modSourcecode (BasicBlockData ast)) =
    TerminaModuleData modQualifiedName modFullPath 
        modModificationTime modImportedModules modVisibleModules modSourcecode . BasicBlockData <$> mapM constFoldElement ast

runConstFolding
  :: ConstFoldEnv
  -> ConstFoldMonad a
  -> Either ConstFoldError (a, ConstFoldEnv)
runConstFolding initSt m = case flip ST.runState initSt . runExceptT $ m of
  (Left err, _) -> Left err
  (Right output, st) -> Right (output, st)
