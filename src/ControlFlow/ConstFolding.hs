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

data ConstFoldSt = ConstFoldSt
  {
    localConstEnv :: M.Map Identifier (Const SemanticAnn),
    globalConstEnv :: M.Map Identifier (Const SemanticAnn),
    currentElement :: Maybe Identifier,
    maxDepth :: Integer,
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

constFoldParams :: [Parameter SemanticAnn] -> [Expression SemanticAnn] -> ConstFoldMonad ()
constFoldParams [] [] = return ()
constFoldParams (param:params) (expr:exprs) = do
  case paramType param of
    (TConstSubtype _) -> do
      constValue <- evalConstExpression expr
      ST.modify (\s -> s { localConstEnv = M.insert (paramIdentifier param) constValue (localConstEnv s) })
      constFoldParams params exprs
    _ -> constFoldParams params exprs
constFoldParams _ _ = throwError $ annotateError Internal EInvalidParameterList

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
constFoldExpression (IsOptionVariantExpression obj _ _) = constFoldObject obj
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

constFoldExpression (OptionVariantInitializer None _) = return ()
constFoldExpression (OptionVariantInitializer (Some expr) _) = do
  constFoldExpression expr
constFoldExpression (EnumVariantInitializer _ _ exprs _) =
  mapM_ constFoldExpression exprs
constFoldExpression e = throwError $ annotateError Internal (EInvalidExpression $ "invalid expression: " ++ show e)

constFoldFlowTransfer :: [Expression SemanticAnn] -> TPFunction SemanticAnn -> ConstFoldMonad ()
constFoldFlowTransfer exprs func@(TPFunction _ params _ _ _) =
  when (functionHasConstParams func) $
    localInputScope $ constFoldParams params exprs >>
      constFoldFunction func

constFoldAssignment :: Location -> TerminaType SemanticAnn -> Expression SemanticAnn -> ConstFoldMonad ()
constFoldAssignment loc (TArray ty arraySize) initExpr@(ArrayInitializer assignmentExpr _ _) = do
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
  constFoldAssignment loc ty assignmentExpr
constFoldAssignment loc (TArray ty arraySize) initExpr@(ArrayExprListInitializer assignmentExprs _) = do
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
  mapM_ (constFoldAssignment loc ty) assignmentExprs
constFoldAssignment loc (TArray _ arraySize) initExpr@(StringInitializer {}) = do
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
constFoldAssignment loc (TStruct _) (StructInitializer fas _) = do
  mapM_ constFoldFieldAssignment fas

  where

    constFoldFieldAssignment :: FieldAssignment SemanticAnn -> ConstFoldMonad ()
    constFoldFieldAssignment (FieldValueAssignment _ expr (SemanticAnn (ETy (SimpleType ty)) _)) = do
      constFoldAssignment loc ty expr
    constFoldFieldAssignment (FieldValueAssignment {}) = do
      throwError $ annotateError loc EInvalidFieldValueAssignmentAnnotation
    constFoldFieldAssignment (FieldAddressAssignment {}) = return ()
    constFoldFieldAssignment (FieldPortConnection AccessPortConnection _ _ (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy _ portSize glbSize))) _)) = do
      portSizeValue <- evalConstExpression portSize
      glbSizeValue <- evalConstExpression glbSize
      case (portSizeValue, glbSizeValue) of
        (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
          if lhs == rhs then
            return ()
          else
            throwError $ annotateError loc (EAtomicArrayConnectionSizeMismatch lhs rhs)
        _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
    constFoldFieldAssignment (FieldPortConnection {}) = return ()
constFoldAssignment _ _ expr = constFoldExpression expr

constFoldStatement :: Statement SemanticAnn -> ConstFoldMonad ()
constFoldStatement (Declaration _ _ ty initExpr ann) =
  constFoldAssignment (getLocation ann) ty initExpr
constFoldStatement (AssignmentStmt obj expr ann) = do
  constFoldObject obj
  constFoldExpression expr
  objType <- getObjType obj
  constFoldAssignment (getLocation ann) objType expr
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
constFoldBasicBlock (MatchBlock expr cases _) =
  constFoldExpression expr >>
  mapM_ (\(MatchCase _ _ blk _) -> do
    constFoldBasicBlocks blk) cases
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
constFoldBasicBlock (SystemCall {}) =
  return ()

constFoldBasicBlocks :: Block SemanticAnn -> ConstFoldMonad ()
constFoldBasicBlocks (Block body _) = localInputScope $ mapM_ constFoldBasicBlock body

constFoldEmitter :: TPEmitter SemanticAnn -> ConstFoldMonad ()
constFoldEmitter (TPInterruptEmittter ident _) = do
  progArchitecture <- ST.gets progArch
  let (glb, portName, _) =
        case M.lookup ident (emitterTargets progArchitecture) of
          Just o -> o
          Nothing -> error $ "Emitter not found: " ++ show ident
  case M.lookup glb (tasks progArchitecture) of
    Just (TPTask task clsId _ _ _ _ _ _ _) -> do
      -- Get the task class id
      let taskCls =
            case M.lookup clsId (taskClasses progArchitecture) of
              Just cls -> cls
              Nothing -> error $ "Task class not found: " ++ show clsId
      -- Get the task port connections
          targetAction =
                case M.lookup portName (sinkPorts taskCls) of
                  Just (_, a) -> a
                  Nothing -> error $ "Port not found: " ++ show portName
          action = case M.lookup targetAction (classMemberFunctions taskCls) of
            Just a -> a
            Nothing -> error $ "Function not found: " ++ show targetAction
      switchInputScope task $ constFoldFunction action
    Nothing -> case M.lookup glb (handlers progArchitecture) of
      Just (TPHandler handler clsId _ _ _ _ _ _) -> do
        -- Get the handler class id
        let handlerCls =
              case M.lookup clsId (handlerClasses progArchitecture) of
                Just cls -> cls
                Nothing -> error $ "Handler class not found: " ++ show clsId
        -- Get the handler port connections
            targetAction =
                case M.lookup portName (sinkPorts handlerCls) of
                  Just (_, a) -> a
                  Nothing -> error $ "Port not found: " ++ show portName
            action = case M.lookup targetAction (classMemberFunctions handlerCls) of
              Just a -> a
              Nothing -> error $ "Function not found: " ++ show targetAction
        switchInputScope handler $ constFoldFunction action
      Nothing -> throwError $ annotateError Internal EUnboxingEmitter
  return ()
constFoldEmitter (TPPeriodicTimerEmitter {}) = return ()
constFoldEmitter (TPSystemInitEmitter {}) = return ()

loadGlobalConstEvironment :: ConstFoldMonad ()
loadGlobalConstEvironment = do
  progArchitecture <- ST.gets progArch
  let glbConstants = globalConstants progArchitecture
  -- Load the global constants into the environment
  mapM_ (\(TPGlobalConstant identifier _ expr _) -> do
    constExpr <- evalConstExpression expr
    ST.modify (\s -> s { globalConstEnv = M.insert identifier constExpr (globalConstEnv s) })) (M.elems glbConstants)

runConstFolding ::
  TerminaProgArch SemanticAnn
  -> Maybe ConstFoldError
runConstFolding progArchitecture =
  let progEmitters = M.elems $ emitters progArchitecture
      env = ConstFoldSt
        {
          localConstEnv = M.empty,
          globalConstEnv = M.empty,
          currentElement = Nothing,
          progArch = progArchitecture,
          maxDepth = 1
        } in
  case flip ST.runState env . runExceptT $
    loadGlobalConstEvironment >>
    mapM_ (\func -> unless (functionHasConstParams func) $
        constFoldFunction func) (M.elems $ functions progArchitecture) >>
    mapM_ (\func -> unless (functionHasConstParams func) $
        constFoldFunction func) 
          (concatMap (M.elems . classMemberFunctions) (M.elems (taskClasses progArchitecture)))>>
    mapM_ (\func -> unless (functionHasConstParams func) $
        constFoldFunction func) 
          (concatMap (M.elems . classMemberFunctions) (M.elems (handlerClasses progArchitecture)))>>
    mapM_ (\func -> unless (functionHasConstParams func) $
        constFoldFunction func) 
          (concatMap (M.elems . classMemberFunctions) (M.elems (resourceClasses progArchitecture)))>>
    mapM_ constFoldEmitter progEmitters of
      (Left err, _) -> Just err
      (Right _, _) -> Nothing