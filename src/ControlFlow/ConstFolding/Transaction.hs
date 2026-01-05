module ControlFlow.ConstFolding.Transaction where
import ControlFlow.BasicBlocks.AST
import ControlFlow.Architecture.Types
import Semantic.Types
import ControlFlow.ConstFolding.Monad
import Utils.Annotations
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M
import Control.Monad.Except
import ControlFlow.ConstFolding.Utils
import Control.Monad
import Command.Types
import Modules.Modules

constFoldCheckFVAType :: FieldAssignment SemanticAnn -> TransFoldMonad ()
constFoldCheckFVAType (FieldValueAssignment _ expr (SemanticAnn (ETy (SimpleType exprTy)) exprLoc)) = do
  constFoldCheckExprType exprLoc exprTy expr
constFoldCheckFVAType _ = return () -- Ignore other cases

constFoldCheckExprType :: Location -> TerminaType SemanticAnn -> Expression SemanticAnn -> TransFoldMonad ()
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
      if lhs >= rhs then
        return ()
      else
        throwError $ annotateError loc (EStringInitializerInvalidSize lhs rhs)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
constFoldCheckExprType _ _ (StructInitializer fvas _) = do
  mapM_ constFoldCheckFVAType fvas
constFoldCheckExprType _ _ _ = return ()

evalConstObject :: Object SemanticAnn -> TransFoldMonad (Const SemanticAnn)
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

-- | Evaluates the type of an expression. This basically only applies to
-- arrays. The function returns the same expression but with the types
-- (i.e., the array sizes) evaluated.
evalExpressionType :: Location -> TerminaType SemanticAnn -> TransFoldMonad (TerminaType SemanticAnn)
evalExpressionType loc (TArray ty arraySize)= do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- evalExpressionType loc ty
  return (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize)))
evalExpressionType loc (TAtomicArray ty arraySize) = do
  arraySizeValue <- evalConstExpression arraySize
  return (TAtomicArray ty (Constant arraySizeValue (buildExpAnn loc TUSize)))
evalExpressionType loc (TFixedLocation (TArray ty arraySize)) = do
  arraySizeValue <- evalConstExpression arraySize
  ty' <- evalExpressionType loc ty
  return (TFixedLocation (TArray ty' (Constant arraySizeValue (buildExpAnn loc TUSize))))
evalExpressionType _ ty = return ty

evalConstExpression :: Expression SemanticAnn -> TransFoldMonad (Const SemanticAnn)
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

data PassedArgument a =
  ConstantArgument (Const a) a
  | VariableArgument (TerminaType a) a
  deriving Show

transFoldFunction :: TPFunction SemanticAnn -> TransFoldMonad ()
transFoldFunction (TPFunction _ _ _ body _) = do
  transFoldBasicBlocks body

functionHasConstParams :: TPFunction SemanticAnn -> Bool
functionHasConstParams (TPFunction _ params _ _ _) = do
  any isConstParam params

  where

    isConstParam :: Parameter SemanticAnn -> Bool
    isConstParam (Parameter _ (TConstSubtype _)) = True
    isConstParam _ = False

transFoldPassArguments :: Location -> [Parameter SemanticAnn] -> [PassedArgument SemanticAnn] -> TransFoldMonad ()
transFoldPassArguments _ [] [] = return ()
-- | This is a special case. This can only happen when we are sending a message
-- using a message queue of unit type. In this case, the argument to be passed
-- must be a null constant.
transFoldPassArguments _ [] [ConstantArgument Null _] = return ()
transFoldPassArguments loc (param:params) (arg:args) = do
  case (paramType param, arg) of
    (TConstSubtype _, ConstantArgument value _) -> do
      ST.modify (\s -> s { localConstEnv = M.insert (paramIdentifier param) value (localConstEnv s) })
      transFoldPassArguments loc params args
    (ty, VariableArgument ty' ann) ->
      checkSameTy (getLocation ann) ty ty'
    _ -> transFoldPassArguments loc params args
  
  where

    checkSameTy :: Location -> TerminaType SemanticAnn -> TerminaType SemanticAnn -> TransFoldMonad ()
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
transFoldPassArguments _ _ _ = throwError $ annotateError Internal EInvalidParameterList

transFoldObject :: Object SemanticAnn -> TransFoldMonad ()
transFoldObject (Variable {}) = return ()
transFoldObject (ArrayIndexExpression obj indexExpr _) = do
  transFoldObject obj
  transFoldExpression indexExpr
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
transFoldObject (MemberAccess obj _ _) = transFoldObject obj
transFoldObject (Dereference obj _) = transFoldObject obj
transFoldObject (DereferenceMemberAccess obj _ _) = transFoldObject obj
transFoldObject (Unbox obj _) = transFoldObject obj

-- | Folds constant expressions in the AST, evaluating constant expressions
-- and validating their results. This function traverses the expression tree
-- and performs constant folding where possible.
transFoldExpression :: Expression SemanticAnn -> TransFoldMonad ()
transFoldExpression (AccessObject obj) = transFoldObject obj
transFoldExpression (Constant _ _) = return ()
transFoldExpression expr@(BinOp op lhs rhs ann) = do
  transFoldExpression lhs
  transFoldExpression rhs
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
transFoldExpression (ReferenceExpression _ obj _) = transFoldObject obj
transFoldExpression (Casting expr _ _) = transFoldExpression expr
transFoldExpression (FunctionCall ident args _) = do
  progArchitecture <- ST.gets progArch
  let func = functions progArchitecture M.! ident
  transFoldFlowTransfer args func
transFoldExpression (MemberFunctionCall obj ident args _) = do
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      memberFuncs <- getFunctionMembers progArchitecture e
      case M.lookup ident memberFuncs of
        Just func -> do
          transFoldFlowTransfer args func
          transFoldObject obj
        Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid member function call")
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid member function call no current member")
transFoldExpression (DerefMemberFunctionCall obj ident args _) = do
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      memberFuncs <- getFunctionMembers progArchitecture e
      case M.lookup ident memberFuncs of
        Just func -> do
          transFoldFlowTransfer args func
          transFoldObject obj
        Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid deref member function call")
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid deref member function call no current member")
transFoldExpression (IsEnumVariantExpression obj _ _ _) = transFoldObject obj
transFoldExpression (IsMonadicVariantExpression obj _ _) = transFoldObject obj
transFoldExpression expr@(ArraySliceExpression _ obj lower upper ann) = do
  transFoldObject obj
  transFoldExpression lower
  transFoldExpression upper
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

transFoldExpression (MonadicVariantInitializer None _) = return ()
transFoldExpression (MonadicVariantInitializer (Some expr) _) = do
  transFoldExpression expr
transFoldExpression (MonadicVariantInitializer Success _) = return ()
transFoldExpression (MonadicVariantInitializer (Failure expr) _) = do
  transFoldExpression expr
transFoldExpression (MonadicVariantInitializer (Ok expr) _) = do
  transFoldExpression expr
transFoldExpression (MonadicVariantInitializer (Error expr) _) = do
  transFoldExpression expr
transFoldExpression (EnumVariantInitializer _ _ exprs _) =
  mapM_ transFoldExpression exprs
transFoldExpression (StructInitializer fvas _) = do
  mapM_ transFoldFieldAssignment fvas
transFoldExpression e = throwError $ annotateError Internal (EInvalidExpression $ "invalid expression: " ++ show e)

transFoldFlowTransfer :: [Expression SemanticAnn] -> TPFunction SemanticAnn -> TransFoldMonad ()
transFoldFlowTransfer exprs func@(TPFunction _ params _ _ ann) = do
  args <- mapM (\expr -> do
    exprType <- getExprType expr
    case exprType of
      (TConstSubtype _) -> flip ConstantArgument (getAnnotation expr) <$> evalConstExpression expr 
      _ -> do
        exprType' <- evalExpressionType (getLocation . getAnnotation $ expr) exprType
        return $ VariableArgument exprType' (getAnnotation expr)) exprs
  localInputScope $ 
    transFoldPassArguments (getLocation ann) params args >> 
    -- If the function has constant parameters, we need to evaluate the function
    when (functionHasConstParams func) (transFoldFunction func)
    
transFoldFieldAssignment :: FieldAssignment SemanticAnn -> TransFoldMonad ()
transFoldFieldAssignment (FieldValueAssignment _ expr (SemanticAnn (ETy (SimpleType ty)) loc')) = do
  constFoldCheckExprType loc' ty expr
transFoldFieldAssignment (FieldValueAssignment _ _ ann) = do
  throwError $ annotateError (getLocation ann) EInvalidFieldValueAssignmentAnnotation
transFoldFieldAssignment (FieldPortConnection AccessPortConnection _ _ (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy _ portSize glbSize))) loc')) = do
  portSizeValue <- evalConstExpression portSize
  glbSizeValue <- evalConstExpression glbSize
  case (portSizeValue, glbSizeValue) of
    (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
      if lhs == rhs then
        return ()
      else
        throwError $ annotateError loc' (EAtomicArrayConnectionSizeMismatch lhs rhs)
    _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
transFoldFieldAssignment _ = return ()


transFoldStatement :: Statement SemanticAnn -> TransFoldMonad ()
transFoldStatement (Declaration _ _ ty initExpr ann) =
  constFoldCheckExprType (getLocation ann) ty initExpr
transFoldStatement (AssignmentStmt obj expr ann) = do
  transFoldObject obj
  transFoldExpression expr
  objType <- getObjType obj
  constFoldCheckExprType (getLocation ann) objType expr
transFoldStatement (SingleExpStmt expr _) = do
  transFoldExpression expr

transFoldBasicBlock :: BasicBlock SemanticAnn -> TransFoldMonad ()
transFoldBasicBlock (RegularBlock stmts) =
  mapM_ transFoldStatement stmts
transFoldBasicBlock (IfElseBlock ifCond elifs mElse _) = do
  transFoldIfBlock ifCond
  mapM_ transFoldElseIfBlock elifs
  maybe (return ()) transFoldElseBlock mElse

  where

    transFoldIfBlock :: CondIf SemanticAnn -> TransFoldMonad ()
    transFoldIfBlock (CondIf cond blk _) = do
      transFoldBasicBlocks blk
      condExprType <- getExprType cond
      case condExprType of
        (TConstSubtype _) -> do
          value <- evalConstExpression cond
          throwError $ annotateError (getLocation . getAnnotation $ cond) (EConstCondition value)
        _ -> return ()

    transFoldElseIfBlock :: CondElseIf SemanticAnn -> TransFoldMonad ()
    transFoldElseIfBlock (CondElseIf elifCond blk _) = do
      transFoldBasicBlocks blk
      condExprType <- getExprType elifCond
      case condExprType of
        (TConstSubtype _) -> do
          value <- evalConstExpression elifCond
          throwError $ annotateError (getLocation . getAnnotation $ elifCond) (EConstCondition value)
        _ -> return ()
    
    transFoldElseBlock :: CondElse SemanticAnn -> TransFoldMonad ()
    transFoldElseBlock (CondElse blk _) = do
      transFoldBasicBlocks blk

transFoldBasicBlock (ForLoopBlock _ _ from_expr to_expr mWhile body_stmt ann) = do
  transFoldBasicBlocks body_stmt
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
transFoldBasicBlock (MatchBlock expr cases mDefaultCase _) =
  transFoldExpression expr >>
  mapM_ (\(MatchCase _ _ blk _) -> do
    transFoldBasicBlocks blk) cases >>
  case mDefaultCase of
    Just (DefaultCase blk _) -> do
      transFoldBasicBlocks blk
    Nothing -> return ()
transFoldBasicBlock (SendMessage obj expr _) = do
  transFoldObject obj
  transFoldExpression expr
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      outPort <- getPortName obj
      (targetGlb, targetFunction) <- followSendMessage progArchitecture e outPort
      switchInputScope targetGlb $ transFoldFlowTransfer [expr] targetFunction
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid send message")
transFoldBasicBlock (ProcedureInvoke obj procName exprs _) = do
  transFoldObject obj
  progArchitecture <- ST.gets progArch
  current <- ST.gets currentElement
  case current of
    Just e -> do
      outPort <- getPortName obj
      (targetRes, targetFunction) <- followProcedureCall progArchitecture e outPort procName
      switchInputScope targetRes $ transFoldFlowTransfer exprs targetFunction
    Nothing -> throwError $ annotateError Internal (EInvalidExpression "invalid procedure call")
transFoldBasicBlock (AtomicLoad obj expr _) =
  transFoldObject obj >> transFoldExpression expr
transFoldBasicBlock (AtomicStore obj expr _) =
  transFoldObject obj >> transFoldExpression expr
transFoldBasicBlock (AtomicArrayLoad obj indexExpr expr _) = do
  transFoldObject obj
  transFoldExpression indexExpr
  transFoldExpression expr
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
transFoldBasicBlock (AtomicArrayStore obj indexExpr expr _) = do
  transFoldObject obj
  transFoldExpression indexExpr
  transFoldExpression expr
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
transFoldBasicBlock (AllocBox obj expr _) =
  transFoldObject obj >>
  transFoldExpression expr
transFoldBasicBlock (FreeBox obj expr _) =
  transFoldObject obj >>
  transFoldExpression expr
transFoldBasicBlock (ReturnBlock Nothing _) =
  return ()
transFoldBasicBlock (ReturnBlock (Just expr) _) = do
  transFoldExpression expr
transFoldBasicBlock (ContinueBlock expr _) =
  transFoldExpression expr
transFoldBasicBlock (RebootBlock _) = return ()
transFoldBasicBlock (SystemCall obj _ exprs (SemanticAnn (ETy (AppType params TUnit)) loc)) = do
  transFoldObject obj
  args <- mapM (\expr -> do
    exprType <- getExprType expr
    case exprType of
      (TConstSubtype _) -> flip ConstantArgument (getAnnotation expr) <$> evalConstExpression expr 
      _ -> do
        exprType' <- evalExpressionType (getLocation . getAnnotation $ expr) exprType
        return $ VariableArgument exprType' (getAnnotation expr)) exprs
  localInputScope $ 
    transFoldPassArguments loc params args
transFoldBasicBlock (SystemCall {}) = throwError $ annotateError Internal EInvalidSystemCallAnnotation

transFoldBasicBlocks :: Block SemanticAnn -> TransFoldMonad ()
transFoldBasicBlocks (Block body _) = localInputScope $ mapM_ transFoldBasicBlock body

transFoldGlobalEnvironment :: BasicBlocksModule -> TransFoldMonad ()
transFoldGlobalEnvironment (TerminaModuleData _ _ _ _ _ _ (BasicBlockData ast)) = do
  mapM_ transFoldGlobalDeclaration ast

  where

    transFoldGlobalDeclaration :: AnnASTElement SemanticAnn -> TransFoldMonad ()
    transFoldGlobalDeclaration (GlobalDeclaration (Resource _ ty (Just expr) _ ann)) = do
        constFoldCheckExprType (getLocation ann) ty expr
    transFoldGlobalDeclaration (GlobalDeclaration (Task _ ty (Just expr) _ ann)) = do
        constFoldCheckExprType (getLocation ann) ty expr
    transFoldGlobalDeclaration (GlobalDeclaration (Handler _ ty (Just expr) _ ann)) = do
        constFoldCheckExprType (getLocation ann) ty expr
    transFoldGlobalDeclaration _ = return ()
  
evalFieldType :: FieldDefinition SemanticAnn -> TransFoldMonad (FieldDefinition SemanticAnn)
evalFieldType (FieldDefinition name ty (SemanticAnn (FTy SimpleField) loc)) = do
  ty' <- evalExpressionType loc ty
  return $ FieldDefinition name ty' (SemanticAnn (FTy SimpleField) loc)
evalFieldType f = return f

evalTypeDefinition ::  AnnASTElement SemanticAnn -> TransFoldMonad (AnnASTElement SemanticAnn)
evalTypeDefinition (TypeDefinition (Struct name fields mods) ann) = do
  fields' <- mapM evalFieldType fields
  return $ TypeDefinition (Struct name fields' mods) ann
evalTypeDefinition (TypeDefinition (Class clsKind name members ifaces mods) ann) = do
  members' <- mapM evalMember members
  return $ TypeDefinition (Class clsKind name members' ifaces mods) ann

  where

    evalMember :: ClassMember SemanticAnn -> TransFoldMonad (ClassMember SemanticAnn)
    evalMember (ClassField fieldDef) = do
      fieldDef' <- evalFieldType fieldDef
      return $ ClassField fieldDef'
    evalMember m = return m
evalTypeDefinition (GlobalDeclaration (Resource name ty@(TAtomicArray {}) initExpr mods ann)) = do
  ty' <- evalExpressionType (getLocation ann) ty
  return $ GlobalDeclaration (Resource name ty' initExpr mods ann)

evalTypeDefinition e = return e

evalModuleTypeDefinitions :: BasicBlocksModule -> TransFoldMonad BasicBlocksModule
evalModuleTypeDefinitions (TerminaModuleData modQualifiedName modFullPath 
    modModificationTime modImportedModules modVisibleModules modSourcecode (BasicBlockData ast)) =
    TerminaModuleData modQualifiedName modFullPath 
        modModificationTime modImportedModules modVisibleModules modSourcecode . BasicBlockData <$> mapM evalTypeDefinition ast
    