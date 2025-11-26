module ControlFlow.ConstFolding.Simpl where
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import ControlFlow.ConstFolding.Monad
import ControlFlow.ConstFolding.Utils
import Utils.Annotations
import Core.Utils
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import Command.Types
import Modules.Modules

constSimplFieldAssignment :: FieldAssignment SemanticAnn -> ConstSimplMonad (FieldAssignment SemanticAnn)
constSimplFieldAssignment (FieldValueAssignment ident expr ann) = do
  expr' <- constSimplExpression expr
  return $ FieldValueAssignment ident expr' ann
constSimplFieldAssignment fva = return fva

constSimplType :: TerminaType SemanticAnn -> ConstSimplMonad (TerminaType SemanticAnn)
constSimplType (TArray elemTy size) = 
  TArray <$> constSimplType elemTy <*> constSimplExpression size
constSimplType (TMsgQueue ty size) = 
  TMsgQueue ty <$> constSimplExpression size
constSimplType (TPool elemTy size) = 
  TPool elemTy <$> constSimplExpression size
constSimplType (TAtomicArray ty size) =
  TAtomicArray ty <$> constSimplExpression size
constSimplType (TAtomicArrayAccess ty size) =
  TAtomicArrayAccess ty <$> constSimplExpression size
constSimplType ty = return ty

constSimplExpression :: Expression SemanticAnn -> ConstSimplMonad (Expression SemanticAnn)
constSimplExpression e@(BinOp op (Constant lConst _) (Constant rConst _) ann) = do
  ty <- getExprType e
  fConst <- evalBinOp (getLocation ann) op lConst rConst ty
  return $ Constant fConst ann
constSimplExpression e@(BinOp op lhs rhs ann) = do
  lhs' <- constSimplExpression lhs
  rhs' <- constSimplExpression rhs
  case (lhs', rhs') of
    (Constant lConst _, Constant rConst _) -> do
      ty <- getExprType e
      fConst <- evalBinOp (getLocation ann) op lConst rConst ty
      return $ Constant fConst ann
    _ -> return $ BinOp op lhs' rhs' ann
constSimplExpression (Casting expr ty ann) = do
  expr' <- constSimplExpression expr
  case expr' of
    Constant (I (TInteger i repr) _) _ ->
      if memberIntCons i ty then
        return $ Constant (I (TInteger i repr) (Just ty)) ann
      else
        throwError $ annotateError (getLocation ann) (EConstIntegerOverflow i ty)
    _ -> return $ Casting expr' ty ann
constSimplExpression (FunctionCall ident args ann) = do
  args' <- mapM constSimplExpression args
  return $ FunctionCall ident args' ann
constSimplExpression (MemberFunctionCall obj ident args ann) = do
  args' <- mapM constSimplExpression args
  return $ MemberFunctionCall obj ident args' ann
constSimplExpression (DerefMemberFunctionCall obj ident args ann) = do
  args' <- mapM constSimplExpression args
  return $ DerefMemberFunctionCall obj ident args' ann
constSimplExpression (ArraySliceExpression ak obj lower upper ann) = do
  lower' <- constSimplExpression lower
  upper' <- constSimplExpression upper
  return $ ArraySliceExpression ak obj lower' upper' ann
constSimplExpression (MonadicVariantInitializer (Some expr) ann) = do
  expr' <- constSimplExpression expr
  return $ MonadicVariantInitializer (Some expr') ann
constSimplExpression (MonadicVariantInitializer (Failure expr) ann) = do
  expr' <- constSimplExpression expr
  return $ MonadicVariantInitializer (Failure expr') ann
constSimplExpression (MonadicVariantInitializer (Ok expr) ann) = do
  expr' <- constSimplExpression expr
  return $ MonadicVariantInitializer (Ok expr') ann
constSimplExpression (MonadicVariantInitializer (Error expr) ann) = do
  expr' <- constSimplExpression expr
  return $ MonadicVariantInitializer (Error expr') ann
constSimplExpression (EnumVariantInitializer enumId variantId exprs ann) = do
  exprs' <- mapM constSimplExpression exprs
  return $ EnumVariantInitializer enumId variantId exprs' ann
constSimplExpression (StructInitializer fvas ann) = do
  fvas' <- mapM constSimplFieldAssignment fvas
  return $ StructInitializer fvas' ann
constSimplExpression (ArrayInitializer expr ty ann) = do
  expr' <- constSimplExpression expr
  return $ ArrayInitializer expr' ty ann
constSimplExpression (ArrayExprListInitializer exprs ann) = do
  exprs' <- mapM constSimplExpression exprs
  return $ ArrayExprListInitializer exprs' ann
constSimplExpression e = return e

constSimplStatement :: Statement SemanticAnn -> ConstSimplMonad (Statement SemanticAnn)
constSimplStatement (Declaration ident ak ty initExpr ann) = do
  ty' <- constSimplType ty
  initExpr' <- constSimplExpression initExpr
  return $ Declaration ident ak ty' initExpr' ann
constSimplStatement (AssignmentStmt obj expr ann) = do
  expr' <- constSimplExpression expr
  return $ AssignmentStmt obj expr' ann
constSimplStatement (SingleExpStmt expr ann) = do
  expr' <- constSimplExpression expr
  return $ SingleExpStmt expr' ann

constSimplBasicBlock :: BasicBlock SemanticAnn -> ConstSimplMonad (BasicBlock SemanticAnn)
constSimplBasicBlock (RegularBlock stmts) =
  RegularBlock <$> mapM constSimplStatement stmts
constSimplBasicBlock (IfElseBlock ifCond elifs mElse ann) = do
  ifCond' <- consSimplIfBlock ifCond
  elifs' <- mapM constSimplElseIfBlock elifs
  mElse' <- maybe (return Nothing) (fmap Just . constSimplElseBlock) mElse
  return $ IfElseBlock ifCond' elifs' mElse' ann

  where

    consSimplIfBlock :: CondIf SemanticAnn -> ConstSimplMonad (CondIf SemanticAnn)
    consSimplIfBlock (CondIf cond blk ann') = do
      cond' <- constSimplExpression cond
      blk' <- constSimplBasicBlocks blk
      return $ CondIf cond' blk' ann'
    
    constSimplElseIfBlock :: CondElseIf SemanticAnn -> ConstSimplMonad (CondElseIf SemanticAnn)
    constSimplElseIfBlock (CondElseIf elifCond blk ann') = do
      blk' <- constSimplBasicBlocks blk
      elifCond' <- constSimplExpression elifCond
      return $ CondElseIf elifCond' blk' ann'
    
    constSimplElseBlock :: CondElse SemanticAnn -> ConstSimplMonad (CondElse SemanticAnn)
    constSimplElseBlock (CondElse blk ann') = do
      blk' <- constSimplBasicBlocks blk
      return $ CondElse blk' ann'

constSimplBasicBlock (ForLoopBlock iter ty from_expr to_expr mWhile body_stmt ann) = do
  from_expr' <- constSimplExpression from_expr
  to_expr' <- constSimplExpression to_expr
  body_stmt' <- constSimplBasicBlocks body_stmt
  mWhile' <- maybe (return Nothing) (fmap Just . constSimplExpression) mWhile
  return $ ForLoopBlock iter ty from_expr' to_expr' mWhile' body_stmt' ann
constSimplBasicBlock (MatchBlock expr cases mDefaultCase ann) = do
  expr' <- constSimplExpression expr
  cases' <- mapM constSimplCase cases
  mDefaultCase' <- maybe (return Nothing) (\(DefaultCase blk ann') -> constSimplBasicBlocks blk >>= \blk' -> return . Just $ DefaultCase blk' ann') mDefaultCase
  return $ MatchBlock expr' cases' mDefaultCase' ann

  where

    constSimplCase :: MatchCase SemanticAnn -> ConstSimplMonad (MatchCase SemanticAnn)
    constSimplCase (MatchCase variantId vars blk ann') = do
      blk' <- constSimplBasicBlocks blk
      return $ MatchCase variantId vars blk' ann'

constSimplBasicBlock (SendMessage obj expr ann) = do
  expr' <- constSimplExpression expr
  return $ SendMessage obj expr' ann
constSimplBasicBlock (ProcedureInvoke obj procName exprs ann) = do
  exprs' <- mapM constSimplExpression exprs
  return $ ProcedureInvoke obj procName exprs' ann
constSimplBasicBlock (AtomicLoad obj expr ann) = do
  expr' <- constSimplExpression expr
  return $ AtomicLoad obj expr' ann
constSimplBasicBlock (AtomicStore obj expr ann) = do
  expr' <- constSimplExpression expr
  return $ AtomicStore obj expr' ann
constSimplBasicBlock (AtomicArrayLoad obj indexExpr expr ann) = do
  indexExpr' <- constSimplExpression indexExpr
  expr' <- constSimplExpression expr
  return $ AtomicArrayLoad obj indexExpr' expr' ann
constSimplBasicBlock (AtomicArrayStore obj indexExpr expr ann) = do
  indexExpr' <- constSimplExpression indexExpr
  expr' <- constSimplExpression expr
  return $ AtomicArrayStore obj indexExpr' expr' ann
constSimplBasicBlock (AllocBox obj expr ann) = do
  expr' <- constSimplExpression expr
  return $ AllocBox obj expr' ann
constSimplBasicBlock (FreeBox obj expr ann) = do
  expr' <- constSimplExpression expr
  return $ FreeBox obj expr' ann
constSimplBasicBlock r@(ReturnBlock Nothing _) = return r
constSimplBasicBlock (ReturnBlock (Just expr) ann) = do
  expr' <- constSimplExpression expr
  return $ ReturnBlock (Just expr') ann
constSimplBasicBlock (ContinueBlock expr ann) = do
  expr' <- constSimplExpression expr
  return $ ContinueBlock expr' ann
constSimplBasicBlock r@(RebootBlock _) = return r
constSimplBasicBlock (SystemCall obj ident exprs ann) = do
  exprs' <- mapM constSimplExpression exprs
  return $ SystemCall obj ident exprs' ann

constSimplBasicBlocks :: Block SemanticAnn -> ConstSimplMonad (Block SemanticAnn)
constSimplBasicBlocks (Block body ann) = do
  body' <- mapM constSimplBasicBlock body
  return $ Block body' ann


constSimplFieldDefinition :: FieldDefinition SemanticAnn -> ConstSimplMonad (FieldDefinition SemanticAnn)
constSimplFieldDefinition (FieldDefinition name ty ann) = do
  ty' <- constSimplType ty
  return $ FieldDefinition name ty' ann

constSimplParam :: Parameter SemanticAnn -> ConstSimplMonad (Parameter SemanticAnn)
constSimplParam (Parameter name ty) = 
  Parameter name <$> constSimplType ty

constSimplGlobal :: Global SemanticAnn -> ConstSimplMonad (Global SemanticAnn)
constSimplGlobal (Resource ident ty mInitExpr mods ann) = do
  ty' <- constSimplType ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constSimplExpression) mInitExpr
  return $ Resource ident ty' mInitExpr' mods ann
constSimplGlobal (Task ident ty mInitExpr mods ann) = do
  ty' <- constSimplType ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constSimplExpression) mInitExpr
  return $ Task ident ty' mInitExpr' mods ann
constSimplGlobal (Handler ident ty mInitExpr mods ann) = do
  ty' <- constSimplType ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constSimplExpression) mInitExpr
  return $ Handler ident ty' mInitExpr' mods ann
constSimplGlobal (Const identifier ty expr mods ann) = do
  ty' <- constSimplType ty
  expr' <- constSimplExpression expr
  return $ Const identifier ty' expr' mods ann
constSimplGlobal (Channel ident ty mInitExpr mods ann) = do
  ty' <- constSimplType ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constSimplExpression) mInitExpr
  return $ Channel ident ty' mInitExpr' mods ann
constSimplGlobal (Emitter ident ty mInitExpr mods ann) = do
  ty' <- constSimplType ty
  mInitExpr' <- maybe (return Nothing) (fmap Just . constSimplExpression) mInitExpr
  return $ Emitter ident ty' mInitExpr' mods ann
constSimplGlobal g = return g -- This should not happen

constSimplTypeDef :: TypeDef SemanticAnn -> ConstSimplMonad (TypeDef SemanticAnn)
constSimplTypeDef (Struct ident fieldDefs ann) = do
  fieldDefs' <- mapM constSimplFieldDefinition fieldDefs
  return $ Struct ident fieldDefs' ann
constSimplTypeDef (Enum ident variants ann) = do
  variants' <- mapM constSimplEnumVariant variants
  return $ Enum ident variants' ann

  where 

    constSimplEnumVariant :: EnumVariant SemanticAnn -> ConstSimplMonad (EnumVariant SemanticAnn)
    constSimplEnumVariant (EnumVariant variantId tys) = do
      tys' <- mapM constSimplType tys
      return $ EnumVariant variantId tys'

constSimplTypeDef (Class ck classId members provides cann) = do
  members' <- mapM constSimplClassMember members
  return $ Class ck classId members' provides cann

  where

    constSimplClassMember :: ClassMember SemanticAnn -> ConstSimplMonad (ClassMember SemanticAnn)
    constSimplClassMember (ClassField fdef) = 
      ClassField <$> constSimplFieldDefinition fdef
    constSimplClassMember (ClassMethod ak ident params mrty body ann) = do
      params' <- mapM constSimplParam params
      mrty' <- maybe (return Nothing) (fmap Just . constSimplType) mrty
      body' <- constSimplBasicBlocks body
      return $ ClassMethod ak ident params' mrty' body' ann
    constSimplClassMember (ClassProcedure ak ident params body ann) = do
      params' <- mapM constSimplParam params
      body' <- constSimplBasicBlocks body
      return $ ClassProcedure ak ident params' body' ann
    constSimplClassMember (ClassViewer ident params mrty body ann) = do
      params' <- mapM constSimplParam params
      mrty' <- maybe (return Nothing) (fmap Just . constSimplType) mrty
      body' <- constSimplBasicBlocks body
      return $ ClassViewer ident params' mrty' body' ann
    constSimplClassMember (ClassAction ak ident params rty body ann) = do
      params' <- mapM constSimplParam params
      rty' <- constSimplType rty
      body' <- constSimplBasicBlocks body
      return $ ClassAction ak ident params' rty' body' ann
    
constSimplTypeDef (Interface ik ident extends procs mods) = do
  procs' <- mapM constSimplInterfaceMember procs
  return $ Interface ik ident extends procs' mods

  where

    constSimplInterfaceMember :: InterfaceMember SemanticAnn -> ConstSimplMonad (InterfaceMember SemanticAnn)
    constSimplInterfaceMember (InterfaceProcedure ak procId params mods' ann) = do
      params' <- mapM constSimplParam params
      return $ InterfaceProcedure ak procId params' mods' ann

constSimplElement :: AnnASTElement SemanticAnn -> ConstSimplMonad (AnnASTElement SemanticAnn)
constSimplElement (GlobalDeclaration g) =
  GlobalDeclaration <$> constSimplGlobal g
constSimplElement (TypeDefinition td ann) = do
  td' <- constSimplTypeDef td
  return $ TypeDefinition td' ann
constSimplElement (Function ident params mrty body mods ann) = do
  params' <- mapM constSimplParam params
  mrty' <- maybe (return Nothing) (fmap Just . constSimplType) mrty
  body' <- constSimplBasicBlocks body
  return $ Function ident params' mrty' body' mods ann

constSimplModule :: BasicBlocksModule -> ConstSimplMonad BasicBlocksModule
constSimplModule (TerminaModuleData modQualifiedName modFullPath 
    modModificationTime modImportedModules modVisibleModules modSourcecode (BasicBlockData ast)) =
    TerminaModuleData modQualifiedName modFullPath 
        modModificationTime modImportedModules modVisibleModules modSourcecode . BasicBlockData <$> mapM constSimplElement ast
