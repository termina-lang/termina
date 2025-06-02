module ControlFlow.ConstFolding where

import qualified Data.Map as M
import Semantic.Types
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST
import ControlFlow.Architecture.Types
import Utils.Annotations
import ControlFlow.BasicBlocks.AST
import Core.Utils
import ControlFlow.ConstFolding.Utils
import Data.Bits
import Control.Monad
import Command.Types
import Modules.Modules

data ConstFoldSt = ConstFoldSt
  {
    localConstEnv :: M.Map Identifier (Const SemanticAnn),
    globalConstEnv :: M.Map Identifier (Const SemanticAnn),
    currentElement :: Maybe Identifier,
    progArch :: TerminaProgArch SemanticAnn
  }

type ConstFoldMonad = ExceptT ConstFoldError (ST.State ConstFoldSt)

localInputScope :: ConstFoldMonad a -> ConstFoldMonad a
localInputScope comp = do
  prevst <- ST.get
  res <- comp
  currst <- ST.get
  ST.put (currst { localConstEnv = localConstEnv prevst })
  return res

switchInputScope :: Identifier -> ConstFoldMonad a -> ConstFoldMonad a
switchInputScope target comp = do
  prevst <- ST.get
  ST.put (prevst { currentElement = Just target })
  res <- comp
  ST.put prevst
  return res

data PassedArgument a =
  ConstantArgument (Const a) a
  | VariableArgument (TerminaType a) a
  deriving Show

evalConstObject :: Object SemanticAnn -> ConstFoldMonad (Const SemanticAnn)
evalConstObject (Variable ident _) = do
  localEnv <- ST.gets localConstEnv
  case M.lookup ident localEnv of
    Just expr -> return expr
    Nothing -> do
      globalEnv <- ST.gets globalConstEnv
      case M.lookup ident globalEnv of
        Just expr -> return expr
        Nothing -> throwError $ annotateError Internal (EUnknownIdentifier ident)
evalConstObject _ = throwError $ annotateError Internal ENotConstant

evalBinOp :: Location
  -> Op -> Const SemanticAnn
  -> Const SemanticAnn
  -> TerminaType SemanticAnn -> ConstFoldMonad (Const SemanticAnn)
evalBinOp loc Multiplication (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs * rhs in
  if memberIntCons result ty then
    return $ I (TInteger result repr) (Just ty)
  else
    throwError $ annotateError loc (EConstIntegerOverflow result ty)
evalBinOp loc Division (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  if rhs == 0 then
    throwError $ annotateError loc EConstDivisionByZero
  else
  let result = lhs `div` rhs in
  return $ I (TInteger result repr) (Just ty)
evalBinOp loc Addition (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs + rhs in
  if memberIntCons result ty then
    return $ I (TInteger result repr) (Just ty)
  else
    throwError $ annotateError loc (EConstIntegerOverflow result ty)
evalBinOp loc Subtraction (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs - rhs in
  if posTy ty && result < 0 then
    throwError $ annotateError loc (EConstIntegerUnderflow result ty)
  else
    return $ I (TInteger result repr) (Just ty)
evalBinOp loc Modulo (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  if rhs == 0 then
    throwError $ annotateError loc EConstDivisionByZero
  else
  let result = lhs `mod` rhs in
  return $ I (TInteger result repr) (Just ty)
evalBinOp loc BitwiseLeftShift (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs `shiftL` fromIntegral rhs in
  if memberIntCons result ty then
    return $ I (TInteger result repr) (Just ty)
  else
    throwError $ annotateError loc (EConstIntegerOverflow result ty)
evalBinOp loc BitwiseRightShift (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs `shiftR` fromIntegral rhs in
  if memberIntCons result ty then
    return $ I (TInteger result repr) (Just ty)
  else
    throwError $ annotateError loc (EConstIntegerOverflow result ty)
evalBinOp _ RelationalLT (I (TInteger lhs _) _) (I (TInteger rhs _) _) _ =
  if lhs < rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ RelationalLTE (I (TInteger lhs _) _) (I (TInteger rhs _) _) _ =
  if lhs <= rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ RelationalGT (I (TInteger lhs _) _) (I (TInteger rhs _) _) _ =
  if lhs > rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ RelationalGTE (I (TInteger lhs _) _) (I (TInteger rhs _) _) _ =
  if lhs >= rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ RelationalEqual (I (TInteger lhs _) _) (I (TInteger rhs _) _) _ =
  if lhs == rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ RelationalNotEqual (I (TInteger lhs _) _) (I (TInteger rhs _) _) _ =
  if lhs /= rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ BitwiseAnd (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty = do
  let result = lhs .&. rhs
  return $ I (TInteger result repr) (Just ty)
evalBinOp _ BitwiseOr (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs .|. rhs in
  return $ I (TInteger result repr) (Just ty)
evalBinOp _ BitwiseXor (I (TInteger lhs repr) _) (I (TInteger rhs _) _) ty =
  let result = lhs `xor` rhs in
  return $ I (TInteger result repr) (Just ty)
evalBinOp _ LogicalAnd (B lhs) (B rhs) _ =
  if lhs && rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ LogicalOr (B lhs) (B rhs) _ =
  if lhs || rhs then
    return $ B True
  else
    return $ B False
evalBinOp _ _ _ _ _ =
  throwError $ annotateError Internal (EInvalidExpression "invalid bin op")

-- | Evaluates the type of an expression. This basically only applies to
-- arrays. The function returns the same expression but with the types
-- (i.e., the array sizes) evaluated.
evalExpressionType :: Location -> TerminaType SemanticAnn -> ConstFoldMonad (TerminaType SemanticAnn)
evalExpressionType loc (TArray ty arraySize)= do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- evalExpressionType loc ty
  return (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize)))
evalExpressionType loc (TAtomicArray ty arraySize) = do
  arraySizeValue <- evalConstExpression arraySize
  return (TAtomicArray ty (Constant arraySizeValue (buildExpAnn loc TUSize)))
evalExpressionType _ ty = return ty

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

constFoldFunction :: TPFunction SemanticAnn -> ConstFoldMonad ()
constFoldFunction (TPFunction _ _ _ body _) = do
  constFoldBasicBlocks body

functionHasConstParams :: TPFunction SemanticAnn -> Bool
functionHasConstParams (TPFunction _ params _ _ _) = do
  any isConstParam params

  where

    isConstParam :: Parameter SemanticAnn -> Bool
    isConstParam (Parameter _ (TConstSubtype _)) = True
    isConstParam _ = False

constFoldPassArguments :: Location -> [Parameter SemanticAnn] -> [PassedArgument SemanticAnn] -> ConstFoldMonad ()
constFoldPassArguments _ [] [] = return ()
-- | This is a special case. This can only happen when we are sending a message
-- using a message queue of unit type. In this case, the argument to be passed
-- must be a null constant.
constFoldPassArguments _ [] [ConstantArgument Null _] = return ()
constFoldPassArguments loc (param:params) (arg:args) = do
  case (paramType param, arg) of
    (TConstSubtype _, ConstantArgument value _) -> do
      ST.modify (\s -> s { localConstEnv = M.insert (paramIdentifier param) value (localConstEnv s) })
      constFoldPassArguments loc params args
    (ty, VariableArgument ty' ann) ->
      checkSameTy (getLocation ann) ty ty'
    _ -> constFoldPassArguments loc params args
  
  where

    checkSameTy :: Location -> TerminaType SemanticAnn -> TerminaType SemanticAnn -> ConstFoldMonad ()
    checkSameTy loc' (TReference _ (TArray ty arraySize)) (TReference _ (TArray ty' arraySize')) = do
      arraySizeValue <- evalConstExpression arraySize
      arraySizeValue' <- evalConstExpression arraySize'
      case (arraySizeValue, arraySizeValue') of
        (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
          if lhs == rhs then
            checkSameTy loc' ty ty'
          else
            throwError $ annotateError loc' (EReferencedArraySizeMismatch lhs rhs)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    checkSameTy _ _ _ = return ()
constFoldPassArguments _ _ _ = throwError $ annotateError Internal EInvalidParameterList

constFoldObject :: Object SemanticAnn -> ConstFoldMonad ()
constFoldObject (Variable {}) = return ()
constFoldObject (ArrayIndexExpression obj indexExpr _) = do
  constFoldObject obj
  constFoldExpression indexExpr
  objType <- getObjType obj
  indexExprType <- getExprType indexExpr
  case (objType, indexExprType) of
    (TArray _ arraySize, TConstSubtype _) -> do
      arraySizeValue <- evalConstExpression arraySize
      exprValue <- evalConstExpression indexExpr
      case (arraySizeValue, exprValue) of
        (I (TInteger size _) _, I (TInteger index _) _) -> do
          when (index >= size) $ throwError $ annotateError (getLocation . getAnnotation $ obj) (EArrayIndexOutOfBounds size index)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return ()
constFoldObject (MemberAccess obj _ _) = constFoldObject obj
constFoldObject (Dereference obj _) = constFoldObject obj
constFoldObject (DereferenceMemberAccess obj _ _) = constFoldObject obj
constFoldObject (Unbox obj _) = constFoldObject obj

-- | Folds constant expressions in the AST, evaluating constant expressions
-- and validating their results. This function traverses the expression tree
-- and performs constant folding where possible.
constFoldExpression :: Expression SemanticAnn -> ConstFoldMonad ()
constFoldExpression (AccessObject obj) = constFoldObject obj
constFoldExpression (Constant _ _) = return ()
constFoldExpression expr@(BinOp op lhs rhs ann) = do
  constFoldExpression lhs
  constFoldExpression rhs
  lhs_type <- getExprType lhs
  rhs_type <- getExprType rhs
  ty <- getExprType expr
  case (lhs_type, rhs_type) of
    (TConstSubtype _, TConstSubtype _) -> do
      -- If both lhs and rhs are constant expressions, evaluate the bin op.
      -- The evaluation will trigger an error if the result is not valid.
      lhs_value <- evalConstExpression lhs
      rhs_value <- evalConstExpression rhs
      void $ evalBinOp (getLocation ann) op lhs_value rhs_value ty
    _ -> return ()
constFoldExpression (ReferenceExpression _ obj _) = constFoldObject obj
constFoldExpression (Casting expr _ _) = constFoldExpression expr
constFoldExpression (FunctionCall ident args _) = do
  progArchitecture <- ST.gets progArch
  let func = functions progArchitecture M.! ident
  constFoldFlowTransfer args func
constFoldExpression (MemberFunctionCall obj ident args _) = do
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      memberFuncs <- getFunctionMembers progArchitecture e
      case M.lookup ident memberFuncs of
        Just func -> do
          constFoldFlowTransfer args func
          constFoldObject obj
        Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid member function call")
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid member function call no current member")
constFoldExpression (DerefMemberFunctionCall obj ident args _) = do
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      memberFuncs <- getFunctionMembers progArchitecture e
      case M.lookup ident memberFuncs of
        Just func -> do
          constFoldFlowTransfer args func
          constFoldObject obj
        Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid deref member function call")
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid deref member function call no current member")
constFoldExpression (IsEnumVariantExpression obj _ _ _) = constFoldObject obj
constFoldExpression (IsMonadicVariantExpression obj _ _) = constFoldObject obj
constFoldExpression expr@(ArraySliceExpression _ obj lower upper ann) = do
  constFoldObject obj
  constFoldExpression lower
  constFoldExpression upper
  exprType <- getExprType expr
  lowerType <- getExprType lower
  upperType <- getExprType upper
  case (exprType, lowerType, upperType) of
    -- | If both lower and upper are constant expressions, check if the slice is valid.
    (TArray _ arraySize, TConstSubtype _, TConstSubtype _) -> do
      arraySizeValue <- evalConstExpression arraySize
      lowerValue <- evalConstExpression lower
      upperValue <- evalConstExpression upper
      case (arraySizeValue, lowerValue, upperValue) of
        (I (TInteger size _) _, I (TInteger lowerIndex _) _, I (TInteger upperIndex _) _) -> do
          if upperIndex >= size then
            throwError $ annotateError (getLocation ann) (EArraySliceOutOfBounds size upperIndex)
          else if lowerIndex > upperIndex then
            throwError $ annotateError (getLocation ann) (EArraySliceNegativeRange lowerIndex upperIndex)
          else when (size /= (upperIndex - lowerIndex)) $ throwError $ annotateError (getLocation ann) (EArraySliceInvalidRange size lowerIndex upperIndex)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return ()

constFoldExpression (MonadicVariantInitializer None _) = return ()
constFoldExpression (MonadicVariantInitializer (Some expr) _) = do
  constFoldExpression expr
constFoldExpression (MonadicVariantInitializer Success _) = return ()
constFoldExpression (MonadicVariantInitializer (Failure expr) _) = do
  constFoldExpression expr
constFoldExpression (MonadicVariantInitializer (Ok expr) _) = do
  constFoldExpression expr
constFoldExpression (MonadicVariantInitializer (Error expr) _) = do
  constFoldExpression expr
constFoldExpression (EnumVariantInitializer _ _ exprs _) =
  mapM_ constFoldExpression exprs
constFoldExpression e = throwError $ annotateError Internal (EInvalidExpression $ "invalid expression: " ++ show e)

constFoldFlowTransfer :: [Expression SemanticAnn] -> TPFunction SemanticAnn -> ConstFoldMonad ()
constFoldFlowTransfer exprs func@(TPFunction _ params _ _ ann) = do
  args <- mapM (\expr -> do
    exprType <- getExprType expr
    case exprType of
      (TConstSubtype _) -> flip ConstantArgument (getAnnotation expr) <$> evalConstExpression expr 
      _ -> do
        exprType' <- evalExpressionType (getLocation . getAnnotation $ expr) exprType
        return $ VariableArgument exprType' (getAnnotation expr)) exprs
  localInputScope $ 
    constFoldPassArguments (getLocation ann) params args >> 
    -- If the function has constant parameters, we need to evaluate the function
    when (functionHasConstParams func) (constFoldFunction func)
    

constFoldCheckExprType :: Location -> TerminaType SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad ()
constFoldCheckExprType loc (TArray ty arraySize) initExpr@(ArrayInitializer assignmentExpr _ _) = do
  arraySizeValue <- evalConstExpression arraySize
  initExprType <- getExprType initExpr
  initExprSizeValue <- case initExprType of
    (TArray _ initExprSize) -> evalConstExpression initExprSize
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  case (arraySizeValue, initExprSizeValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        return ()
      else
        throwError $ annotateError loc (EArrayInitializerSizeMismatch lhs rhs)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  constFoldCheckExprType loc ty assignmentExpr
constFoldCheckExprType loc (TArray ty arraySize) initExpr@(ArrayExprListInitializer assignmentExprs _) = do
  arraySizeValue <- evalConstExpression arraySize
  initExprType <- getExprType initExpr
  initExprSizeValue <- case initExprType of
    (TArray _ initExprSize) -> evalConstExpression initExprSize
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  case (arraySizeValue, initExprSizeValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        return ()
      else
        throwError $ annotateError loc (EArrayExprListInitializerSizeMismatch lhs rhs)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  mapM_ (constFoldCheckExprType loc ty) assignmentExprs
constFoldCheckExprType loc (TArray _ arraySize) initExpr@(StringInitializer {}) = do
  arraySizeValue <- evalConstExpression arraySize
  initExprType <- getExprType initExpr
  initExprSizeValue <- case initExprType of
    (TArray _ initExprSize) -> evalConstExpression initExprSize
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
  case (arraySizeValue, initExprSizeValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        return ()
      else
        throwError $ annotateError loc (EStringInitializerSizeMismatch lhs rhs)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
constFoldCheckExprType _ (TStruct _) (StructInitializer fas _) = do
  mapM_ constFoldFieldAssignment fas

  where

    constFoldFieldAssignment :: FieldAssignment SemanticAnn -> ConstFoldMonad ()
    constFoldFieldAssignment (FieldValueAssignment _ expr (SemanticAnn (ETy (SimpleType ty)) loc')) = do
      constFoldCheckExprType loc' ty expr
    constFoldFieldAssignment (FieldValueAssignment _ _ ann) = do
      throwError $ annotateError (getLocation ann) EInvalidFieldValueAssignmentAnnotation
    constFoldFieldAssignment (FieldAddressAssignment {}) = return ()
    constFoldFieldAssignment (FieldPortConnection AccessPortConnection _ _ (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy _ portSize glbSize))) loc')) = do
      portSizeValue <- evalConstExpression portSize
      glbSizeValue <- evalConstExpression glbSize
      case (portSizeValue, glbSizeValue) of
        (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
          if lhs == rhs then
            return ()
          else
            throwError $ annotateError loc' (EAtomicArrayConnectionSizeMismatch lhs rhs)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    constFoldFieldAssignment (FieldPortConnection {}) = return ()
constFoldCheckExprType _ _ expr = constFoldExpression expr

constFoldStatement :: Statement SemanticAnn -> ConstFoldMonad ()
constFoldStatement (Declaration _ _ ty initExpr ann) =
  constFoldCheckExprType (getLocation ann) ty initExpr
constFoldStatement (AssignmentStmt obj expr ann) = do
  constFoldObject obj
  constFoldExpression expr
  objType <- getObjType obj
  constFoldCheckExprType (getLocation ann) objType expr
constFoldStatement (SingleExpStmt expr _) = do
  constFoldExpression expr

constFoldBasicBlock :: BasicBlock SemanticAnn -> ConstFoldMonad ()
constFoldBasicBlock (RegularBlock stmts) =
  mapM_ constFoldStatement stmts
constFoldBasicBlock (IfElseBlock ifCond ifBlk elifs mElse _) = do
  constFoldBasicBlocks ifBlk
  mapM_ constFoldElseIfBlock elifs
  maybe (return ()) constFoldBasicBlocks mElse
  condExprType <- getExprType ifCond
  case condExprType of
    (TConstSubtype _) -> do
      value <- evalConstExpression ifCond
      throwError $ annotateError (getLocation . getAnnotation $ ifCond) (EConstCondition value)
    _ -> return ()

  where

    constFoldElseIfBlock :: ElseIf SemanticAnn -> ConstFoldMonad ()
    constFoldElseIfBlock (ElseIf elifCond blk _) = do
      constFoldBasicBlocks blk
      condExprType <- getExprType elifCond
      case condExprType of
        (TConstSubtype _) -> do
          value <- evalConstExpression elifCond
          throwError $ annotateError (getLocation . getAnnotation $ elifCond) (EConstCondition value)
        _ -> return ()

constFoldBasicBlock (ForLoopBlock _ _ from_expr to_expr mWhile body_stmt ann) = do
  constFoldBasicBlocks body_stmt
  fromValue <- evalConstExpression from_expr
  toValue <- evalConstExpression to_expr
  case (fromValue, toValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        throwError $ annotateError (getLocation ann) EForLoopStatementZeroIterations
      else if lhs > rhs then
        throwError $ annotateError (getLocation ann) (EForLoopStatementNegativeIterations lhs rhs)
      else do
        case mWhile of
          Just cond -> do
            condExprType <- getExprType cond
            case condExprType of
              (TConstSubtype _) -> do
                value <- evalConstExpression cond
                throwError $ annotateError (getLocation . getAnnotation $ cond) (EConstCondition value)
              _ -> return ()
          Nothing -> return ()
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
constFoldBasicBlock (MatchBlock expr cases mDefaultCase _) =
  constFoldExpression expr >>
  mapM_ (\(MatchCase _ _ blk _) -> do
    constFoldBasicBlocks blk) cases >>
  case mDefaultCase of
    Just (DefaultCase blk _) -> do
      constFoldBasicBlocks blk
    Nothing -> return ()
constFoldBasicBlock (SendMessage obj expr _) = do
  constFoldObject obj
  constFoldExpression expr
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      outPort <- getPortName obj
      (targetGlb, targetFunction) <- followSendMessage progArchitecture e outPort
      switchInputScope targetGlb $ constFoldFlowTransfer [expr] targetFunction
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid send message")
constFoldBasicBlock (ProcedureCall obj procName exprs _) = do
  constFoldObject obj
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      outPort <- getPortName obj
      (targetRes, targetFunction) <- followProcedureCall progArchitecture e outPort procName
      switchInputScope targetRes $ constFoldFlowTransfer exprs targetFunction
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid procedure call")
constFoldBasicBlock (AtomicLoad obj expr _) =
  constFoldObject obj >> constFoldExpression expr
constFoldBasicBlock (AtomicStore obj expr _) =
  constFoldObject obj >> constFoldExpression expr
constFoldBasicBlock (AtomicArrayLoad obj indexExpr expr _) = do
  constFoldObject obj
  constFoldExpression indexExpr
  constFoldExpression expr
  objType <- getObjType obj
  indexExprType <- getExprType indexExpr
  case (objType, indexExprType) of
    (TArray _ arraySize, TConstSubtype _) -> do
      arraySizeValue <- evalConstExpression arraySize
      indexExprValue <- evalConstExpression expr
      case (arraySizeValue, indexExprValue) of
        (I (TInteger size _) _, I (TInteger index _) _) -> do
          if size < index then
            return ()
          else
            throwError $ annotateError (getLocation . getAnnotation $ obj) (EAtomicArrayIndexOutOfBounds size index)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return ()
constFoldBasicBlock (AtomicArrayStore obj indexExpr expr _) = do
  constFoldObject obj
  constFoldExpression indexExpr
  constFoldExpression expr
  objType <- getObjType obj
  indexExprType <- getExprType indexExpr
  case (objType, indexExprType) of
    (TArray _ arraySize, TConstSubtype _) -> do
      arraySizeValue <- evalConstExpression arraySize
      indexExprValue <- evalConstExpression expr
      case (arraySizeValue, indexExprValue) of
        (I (TInteger size _) _, I (TInteger index _) _) -> do
          if size < index then
            return ()
          else
            throwError $ annotateError (getLocation . getAnnotation $ obj) (EAtomicArrayIndexOutOfBounds size index)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    _ -> return ()
constFoldBasicBlock (AllocBox obj expr _) =
  constFoldObject obj >>
  constFoldExpression expr
constFoldBasicBlock (FreeBox obj expr _) =
  constFoldObject obj >>
  constFoldExpression expr
constFoldBasicBlock (ReturnBlock Nothing _) =
  return ()
constFoldBasicBlock (ReturnBlock (Just expr) _) = do
  constFoldExpression expr
constFoldBasicBlock (ContinueBlock expr _) =
  constFoldExpression expr
constFoldBasicBlock (RebootBlock _) = return ()
constFoldBasicBlock (SystemCall obj _ exprs (SemanticAnn (ETy (AppType params TUnit)) loc)) = do
  constFoldObject obj
  args <- mapM (\expr -> do
    exprType <- getExprType expr
    case exprType of
      (TConstSubtype _) -> flip ConstantArgument (getAnnotation expr) <$> evalConstExpression expr 
      _ -> do
        exprType' <- evalExpressionType (getLocation . getAnnotation $ expr) exprType
        return $ VariableArgument exprType' (getAnnotation expr)) exprs
  localInputScope $ 
    constFoldPassArguments loc params args
constFoldBasicBlock (SystemCall {}) = throwError $ annotateError Internal EInvalidSystemCallAnnotation

constFoldBasicBlocks :: Block SemanticAnn -> ConstFoldMonad ()
constFoldBasicBlocks (Block body _) = localInputScope $ mapM_ constFoldBasicBlock body

loadGlobalConstEvironment :: ConstFoldMonad ()
loadGlobalConstEvironment = do
  progArchitecture <- ST.gets progArch
  let glbConstants = globalConstants progArchitecture
  -- Load the global constants into the environment
  mapM_ (\(TPGlobalConstant identifier _ expr _) -> do
    constExpr <- evalConstExpression expr
    ST.modify (\s -> s { globalConstEnv = M.insert identifier constExpr (globalConstEnv s) })) (M.elems glbConstants)

evalFieldType :: FieldDefinition SemanticAnn -> ConstFoldMonad (FieldDefinition SemanticAnn)
evalFieldType (FieldDefinition name ty (SemanticAnn (FTy SimpleField) loc)) = do
  ty' <- evalExpressionType loc ty
  return $ FieldDefinition name ty' (SemanticAnn (FTy SimpleField) loc)
evalFieldType f = return f

evalDefinitions ::  AnnASTElement SemanticAnn -> ConstFoldMonad (AnnASTElement SemanticAnn)
evalDefinitions (TypeDefinition (Struct name fields mods) ann) = do
  fields' <- mapM evalFieldType fields
  return $ TypeDefinition (Struct name fields' mods) ann
evalDefinitions (TypeDefinition (Class clsKind name members ifaces mods) ann) = do
  members' <- mapM evalMember members
  return $ TypeDefinition (Class clsKind name members' ifaces mods) ann

  where

    evalMember :: ClassMember SemanticAnn -> ConstFoldMonad (ClassMember SemanticAnn)
    evalMember (ClassField fieldDef) = do
      fieldDef' <- evalFieldType fieldDef
      return $ ClassField fieldDef'
    evalMember m = return m
evalDefinitions (GlobalDeclaration (Resource name ty@(TAtomicArray {}) initExpr mods ann)) = do
  ty' <- evalExpressionType (getLocation ann) ty
  return $ GlobalDeclaration (Resource name ty' initExpr mods ann)

evalDefinitions e = return e

evalModuleTypeDefinitions :: BasicBlocksModule -> ConstFoldMonad BasicBlocksModule
evalModuleTypeDefinitions (TerminaModuleData modQualifiedName modFullPath 
    modModificationTime modImportedModules modSourcecode (BasicBlockData ast)) =
    TerminaModuleData modQualifiedName modFullPath 
        modModificationTime modImportedModules modSourcecode . BasicBlockData <$> mapM evalDefinitions ast
    

runConstFolding ::
  BasicBlocksProject
  -> TerminaProgArch SemanticAnn
  -> Either ConstFoldError BasicBlocksProject
runConstFolding bbProject progArchitecture =
  let env = ConstFoldSt
        {
          localConstEnv = M.empty,
          globalConstEnv = M.empty,
          currentElement = Nothing,
          progArch = progArchitecture
        } in
  case flip ST.runState env . runExceptT $
    loadGlobalConstEvironment >>
    mapM_ (\func -> unless (functionHasConstParams func) $
        constFoldFunction func) (M.elems $ functions progArchitecture) >>
    forM_ (tasks progArchitecture) (\task -> do
          taskCls <- case M.lookup (taskClass task) (taskClasses progArchitecture) of
            Just cls -> return cls
            Nothing -> throwError $ annotateError Internal (EUnknownTaskClass (taskClass task))
          switchInputScope (taskName task) $ 
            forM_ (classMemberFunctions taskCls) (\func -> unless (functionHasConstParams func) $
                constFoldFunction func)) >>
    forM_ (handlers progArchitecture) (\handler -> do
          handlerCls <- case M.lookup (handlerClass handler) (handlerClasses progArchitecture) of
            Just cls -> return cls
            Nothing -> throwError $ annotateError Internal (EUnknownHandlerClass (handlerClass handler))
          switchInputScope (handlerName handler) $ 
            mapM_ (\func -> unless (functionHasConstParams func) $
                constFoldFunction func) (classMemberFunctions handlerCls)) >>
    forM_ (resources progArchitecture) (\resource -> do
          -- We don't want to fold the system_entry resource
          -- The implementation of the procedures of this reosurce is done
          -- outside Termina
          unless (resourceName resource == "system_entry") $ do
           resourceCls <- case M.lookup (resourceClass resource) (resourceClasses progArchitecture) of
             Just cls -> return cls
             Nothing -> throwError $ annotateError Internal (EUnknownResourceClass (resourceClass resource))
           switchInputScope (resourceName resource) $
             mapM_ (\func -> unless (functionHasConstParams func) $
                 constFoldFunction func) (classMemberFunctions resourceCls)) >>
    mapM evalModuleTypeDefinitions bbProject of
      (Left err, _) -> Left err
      (Right bbProject', _) -> Right bbProject'