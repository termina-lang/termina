module ControlFlow.ConstPropagation where

import qualified Data.Map as M
import Semantic.Types
import Control.Monad.Except
import ControlFlow.ConstPropagation.Errors
import qualified Control.Monad.State as ST
import ControlFlow.Architecture.Types
import Utils.Annotations
import ControlFlow.BasicBlocks.AST
import Core.Utils
import ControlFlow.ConstPropagation.Utils
import Data.Bits
import Control.Monad

data ConstPropSt = ConstPropSt
  {
    localConstEnv :: M.Map Identifier (Const SemanticAnn),
    globalConstEnv :: M.Map Identifier (Const SemanticAnn),
    maxDepth :: Integer,
    progArch :: TerminaProgArch SemanticAnn
  }

type ConstPropMonad = ExceptT ConstPropError (ST.State ConstPropSt)

localInputScope :: ConstPropMonad a -> ConstPropMonad a
localInputScope comp = do
  prevst <- ST.get
  res <- comp
  currst <- ST.get
  ST.put (currst { localConstEnv = localConstEnv prevst })
  return res

globalInputScope :: ConstPropMonad a -> ConstPropMonad a
globalInputScope comp = do
  prevst <- ST.get
  ST.put (prevst { localConstEnv = M.empty })
  res <- comp
  ST.put prevst
  return res

catchUnknownIdentifier :: ConstPropMonad a -> ConstPropMonad a -> ConstPropMonad a
catchUnknownIdentifier handler comp = do
  catchError comp (\err -> do
    case getError err of
      EUnknownIdentifier _ -> handler
      _ -> throwError err)

evalConstObject :: Object SemanticAnn -> ConstPropMonad (Const SemanticAnn)
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
  -> TerminaType SemanticAnn -> ConstPropMonad (Const SemanticAnn)
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
  throwError $ annotateError Internal EInvalidExpression

evalConstExpression :: Expression SemanticAnn -> ConstPropMonad (Const SemanticAnn)
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

constPropFunction :: TPFunction SemanticAnn -> ConstPropMonad ()
constPropFunction (TPFunction _ _ _ body _) = do
  constPropBasicBlocks body

functionHasConstParams :: TPFunction SemanticAnn -> Bool
functionHasConstParams (TPFunction _ params _ _ _) = do
  any isConstParam params

  where

    isConstParam :: Parameter SemanticAnn -> Bool
    isConstParam (Parameter _ (TConstSubtype _)) = True
    isConstParam _ = False

constPropFlowTransfer :: [Expression SemanticAnn] -> TPFunction SemanticAnn -> ConstPropMonad ()
constPropFlowTransfer _ func =
  -- | TODO: Insert const parameters as local 
  when (functionHasConstParams func) $ constPropFunction func

constPropStatement :: Statement SemanticAnn -> ConstPropMonad ()
constPropStatement (Declaration _ _ ty initExpr ann) =
  case ty of
    (TArray _ arraySize) -> do
      arraySizeValue <- evalConstExpression arraySize
      initExprType <- getExprType initExpr
      case initExprType of
        (TArray _ initExprSize) -> do
          initExprSizeValue <- evalConstExpression initExprSize
          case (arraySizeValue, initExprSizeValue) of
            (I (TInteger lhs _) _, I (TInteger rhs _) _) -> do
              if lhs == rhs then
                return ()
              else
                case initExpr of
                  (ArrayInitializer {}) -> do
                    throwError $ annotateError (getLocation ann) (EArrayInitializerSizeMismatch lhs rhs)
                  (ArrayExprListInitializer {}) -> do
                    throwError $ annotateError (getLocation ann) (EArrayExprListInitializerSizeMismatch lhs rhs)
                  (StringInitializer {}) -> do
                    throwError $ annotateError (getLocation ann) (EStringInitializerSizeMismatch lhs rhs)
                  _ -> throwError $ annotateError Internal EUnboxingExpression
            _ -> throwError $ annotateError Internal EInvalidConstantEvaluation
        _ -> throwError $ annotateError Internal EInvalidExpression
    _ -> return ()
constPropStatement (AssignmentStmt {}) = do
  return ()
constPropStatement (SingleExpStmt {}) = do
  return ()

constPropBasicBlock :: BasicBlock SemanticAnn -> ConstPropMonad ()
constPropBasicBlock (RegularBlock stmts) =
  mapM_ constPropStatement stmts
constPropBasicBlock (IfElseBlock {}) =
  return ()
constPropBasicBlock (ForLoopBlock {}) =
  return ()
constPropBasicBlock (MatchBlock {}) =
  return ()
constPropBasicBlock (SendMessage {}) =
  return ()
constPropBasicBlock (ProcedureCall {}) =
  return ()
constPropBasicBlock (AtomicLoad {}) =
  return ()
constPropBasicBlock (AtomicStore {}) =
  return ()
constPropBasicBlock (AtomicArrayLoad {}) =
  return ()
constPropBasicBlock (AtomicArrayStore {}) =
  return ()
constPropBasicBlock (AllocBox {}) =
  return ()
constPropBasicBlock (FreeBox {}) =
  return ()
constPropBasicBlock (ReturnBlock {}) =
  return ()
constPropBasicBlock (ContinueBlock {}) =
  return ()
constPropBasicBlock (SystemCall {}) =
  return ()

constPropBasicBlocks :: Block SemanticAnn -> ConstPropMonad ()
constPropBasicBlocks (Block body _) = localInputScope $ mapM_ constPropBasicBlock body

constPropEmitter :: TPEmitter SemanticAnn -> ConstPropMonad ()
constPropEmitter (TPInterruptEmittter ident _) = do
  progArchitecture <- ST.gets progArch
  let (glb, portName, _) = emitterTargets progArchitecture M.! ident
  case M.lookup glb (tasks progArchitecture) of
    Just task@(TPTask {}) -> do
      -- Get the task class id
      let taskCls = taskClasses progArchitecture M.! taskClass task
      -- Get the task port connections
          targetAction = classMemberFunctions taskCls M.! portName
      return ()
    Nothing -> case M.lookup glb (handlers progArchitecture) of
      Just handler@(TPHandler {}) -> do
        -- Get the handler class id
        let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
        -- Get the handler port connections
            targetAction = classMemberFunctions handlerCls M.! portName
        return ()
      Nothing -> throwError $ annotateError Internal EUnboxingEmitter
  return ()
constPropEmitter (TPPeriodicTimerEmitter {}) = return ()
constPropEmitter (TPSystemInitEmitter {}) = return ()

loadGlobalConstEvironment :: ConstPropMonad ()
loadGlobalConstEvironment = do
  progArchitecture <- ST.gets progArch
  let glbConstants = globalConstants progArchitecture
  -- Load the global constants into the environment
  mapM_ (\(TPGlobalConstant identifier _ expr _) -> do
    constExpr <- evalConstExpression expr
    ST.modify (\s -> s { globalConstEnv = M.insert identifier constExpr (globalConstEnv s) })) (M.elems glbConstants)

runConstPropagation ::
  TerminaProgArch SemanticAnn
  -> Maybe ConstPropError
runConstPropagation progArchitecture =
  let progEmitters = M.elems $ emitters progArchitecture
      env = ConstPropSt
        {
          localConstEnv = M.empty,
          globalConstEnv = M.empty,
          progArch = progArchitecture,
          maxDepth = 1
        } in
  case flip ST.runState env . runExceptT $
    loadGlobalConstEvironment >>
    mapM_ (\func -> unless (functionHasConstParams func) $ 
        constPropFunction func) (M.elems $ functions progArchitecture) >>
    mapM_ constPropEmitter progEmitters of
      (Left err, _) -> Just err
      (Right _, _) -> Nothing