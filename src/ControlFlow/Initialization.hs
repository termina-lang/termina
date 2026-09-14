-- | Definite assignment check.
--
-- A mutable object may be declared without an initializer. This pass walks the
-- basic blocks /forwards/ and keeps the set of objects that have been declared
-- that way and have not been assigned as a whole yet on the path being walked.
-- Reading one of them, or writing one of its fields or elements, is an error
-- (VE-019 and VE-020).
--
module ControlFlow.Initialization (runInitCheck) where

import Control.Monad (when)
import Control.Monad.Except
import qualified Control.Monad.State as ST
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe)

import ControlFlow.BasicBlocks.AST
import ControlFlow.VarUsage.Errors
import Semantic.Types (SemanticAnn)
import Utils.Annotations

-- | Objects declared without an initializer that are not assigned yet. An
-- object is initialized when it is not in this set, so joining two paths is the
-- union of their sets.
type Pending = S.Set Identifier

type InitMonad = ExceptT VarUsageError (ST.State Pending)

-- | The whole object has been declared without an initializer.
markDeclared :: Identifier -> InitMonad ()
markDeclared ident = ST.modify (S.insert ident)

-- | The whole object has been assigned.
markAssigned :: Identifier -> InitMonad ()
markAssigned ident = ST.modify (S.delete ident)

-- | The object the access starts from.
rootIdent :: Object SemanticAnn -> Maybe Identifier
rootIdent (Variable ident _) = Just ident
rootIdent (ArrayIndexExpression obj _ _) = rootIdent obj
rootIdent (MemberAccess obj _ _) = rootIdent obj
rootIdent (Dereference obj _) = rootIdent obj
rootIdent (DereferenceMemberAccess obj _ _) = rootIdent obj
rootIdent (Unbox obj _) = rootIdent obj

checkRead :: Identifier -> Location -> InitMonad ()
checkRead ident loc = do
  pending <- ST.get
  when (S.member ident pending)
    (throwError $ annotateError loc (EReadBeforeAssignment ident))

-- | A write to a field or to an element only makes sense once the whole object
-- has a value.
checkPartialWrite :: Object SemanticAnn -> Location -> InitMonad ()
checkPartialWrite obj loc =
  case rootIdent obj of
    Nothing -> return ()
    Just ident -> do
      pending <- ST.get
      when (S.member ident pending)
        (throwError $ annotateError loc (EPartialWriteBeforeAssignment ident))

readObject :: Object SemanticAnn -> InitMonad ()
readObject (Variable ident ann) = checkRead ident (getLocation ann)
readObject (ArrayIndexExpression obj e _) = readObject obj >> readExpression e
readObject (MemberAccess obj _ _) = readObject obj
readObject (Dereference obj _) = readObject obj
readObject (DereferenceMemberAccess obj _ _) = readObject obj
readObject (Unbox obj _) = readObject obj

-- | The index expressions of an object that is being written into. The object
-- itself is not read, only the indices are.
readIndices :: Object SemanticAnn -> InitMonad ()
readIndices (Variable _ _) = return ()
readIndices (ArrayIndexExpression obj e _) = readIndices obj >> readExpression e
readIndices (MemberAccess obj _ _) = readIndices obj
readIndices (Dereference obj _) = readIndices obj
readIndices (DereferenceMemberAccess obj _ _) = readIndices obj
readIndices (Unbox obj _) = readIndices obj

readFieldAssignment :: FieldAssignment SemanticAnn -> InitMonad ()
readFieldAssignment (FieldValueAssignment _ e _) = readExpression e
readFieldAssignment _ = return ()

-- | Taking a reference, either @&@ or @&mut@, counts as a read: the receiver
-- may read what it is given.
readExpression :: Expression SemanticAnn -> InitMonad ()
readExpression (AccessObject obj) = readObject obj
readExpression (Constant _ _) = return ()
readExpression (BinOp _ le re _) = readExpression le >> readExpression re
readExpression (ReferenceExpression _ obj _) = readObject obj
readExpression (Casting e _ _) = readExpression e
readExpression (IsEnumVariantExpression obj _ _ _) = readObject obj
readExpression (IsMonadicVariantExpression obj _ _) = readObject obj
readExpression (ArraySliceExpression _ obj lower upper _) =
  readObject obj >> readExpression lower >> readExpression upper
readExpression (MemberFunctionCall obj _ args _) = readObject obj >> mapM_ readExpression args
readExpression (DerefMemberFunctionCall obj _ args _) = readObject obj >> mapM_ readExpression args
readExpression (ArrayInitializer e _ _) = readExpression e
readExpression (ArrayExprListInitializer es _) = mapM_ readExpression es
readExpression (StructInitializer fs _) = mapM_ readFieldAssignment fs
readExpression (EnumVariantInitializer _ _ es _) = mapM_ readExpression es
readExpression (MonadicVariantInitializer opt _) =
  case opt of
    None -> return ()
    Some e -> readExpression e
    Success -> return ()
    Failure e -> readExpression e
    Ok e -> readExpression e
    Error e -> readExpression e
readExpression (FunctionCall _ args _) = mapM_ readExpression args
readExpression (StringInitializer _ _) = return ()

checkStatement :: Statement SemanticAnn -> InitMonad ()
checkStatement (Declaration ident _ _ Nothing _) = markDeclared ident
checkStatement (Declaration ident _ _ (Just initExpr) _) =
  readExpression initExpr >> markAssigned ident
checkStatement (AssignmentStmt obj e ann) = do
  readExpression e
  case obj of
    -- | The whole object is assigned
    Variable ident _ -> markAssigned ident
    -- | Only a part of it is
    _ -> checkPartialWrite obj (getLocation ann) >> readIndices obj
checkStatement (SingleExpStmt e _) = readExpression e

-- | Checks one branch from the current state and returns the state it leaves,
-- restoring the entry state so that the next branch starts where this one did.
checkBranch :: Block SemanticAnn -> InitMonad Pending
checkBranch blk = do
  entry <- ST.get
  checkBlock blk
  out <- ST.get
  ST.put entry
  return out

checkBasicBlock :: BasicBlock SemanticAnn -> InitMonad ()
checkBasicBlock (RegularBlock stmts) = mapM_ checkStatement stmts
checkBasicBlock (IfElseBlock condIf elseIfs mElse _) = do
  readExpression (condIfCond condIf)
  ifOut <- checkBranch (condIfBody condIf)
  elseIfOuts <- mapM
    (\elseIf -> readExpression (condElseIfCond elseIf) >> checkBranch (condElseIfBody elseIf))
    elseIfs
  entry <- ST.get
  -- | Without an else branch there is a path that assigns nothing
  elseOut <- maybe (return entry) (checkBranch . condElseBody) mElse
  ST.put (S.unions (ifOut : elseOut : elseIfOuts))
checkBasicBlock (MatchBlock e cases mDefaultCase _) = do
  readExpression e
  caseOuts <- mapM (checkBranch . matchBody) cases
  entry <- ST.get
  case mDefaultCase of
    Just (DefaultCase blk _) -> do
      defaultOut <- checkBranch blk
      ST.put (S.unions (defaultOut : caseOuts))
    -- | Without a default case the listed cases are exhaustive
    Nothing -> ST.put (if null caseOuts then entry else S.unions caseOuts)
checkBasicBlock (ForLoopBlock _ _ initE endE mBreak blk _) = do
  readExpression initE
  readExpression endE
  mapM_ readExpression mBreak
  -- | The body may not run, so what it assigns does not count afterwards
  _ <- checkBranch blk
  return ()
checkBasicBlock (SendMessage obj e _) = readObject obj >> readExpression e
checkBasicBlock (ProcedureInvoke obj _ args _) = readObject obj >> mapM_ readExpression args
checkBasicBlock (AtomicLoad obj e _) = readObject obj >> readExpression e
checkBasicBlock (AtomicStore obj e _) = readObject obj >> readExpression e
checkBasicBlock (AtomicArrayLoad obj idx e _) =
  readObject obj >> readExpression idx >> readExpression e
checkBasicBlock (AtomicArrayStore obj idx e _) =
  readObject obj >> readExpression idx >> readExpression e
checkBasicBlock (AllocBox obj e _) = readObject obj >> readExpression e
checkBasicBlock (FreeBox obj e _) = readObject obj >> readExpression e
checkBasicBlock (SystemCall obj _ args _) = readObject obj >> mapM_ readExpression args
checkBasicBlock (ReturnBlock mRet _) = mapM_ readExpression mRet
checkBasicBlock (ContinueBlock e _) = readExpression e
checkBasicBlock (RebootBlock _) = return ()

checkBlock :: Block SemanticAnn -> InitMonad ()
checkBlock = mapM_ checkBasicBlock . blockBody

-- | Every member starts with an empty set: the declarations of one member are
-- not visible from the next one.
checkClassMember :: ClassMember SemanticAnn -> InitMonad ()
checkClassMember (ClassMethod _ak _ident _ps _tyret body _ann) = ST.put S.empty >> checkBlock body
checkClassMember (ClassProcedure _ak _ident _ps body _ann) = ST.put S.empty >> checkBlock body
checkClassMember (ClassViewer _ident _ps _tyret body _ann) = ST.put S.empty >> checkBlock body
checkClassMember (ClassAction _ak _ident _mp _tyret body _ann) = ST.put S.empty >> checkBlock body
checkClassMember (ClassField {}) = return ()

checkTypeDef :: TypeDef SemanticAnn -> InitMonad ()
checkTypeDef (Class _kind _ident members _provides _mods) = mapM_ checkClassMember members
checkTypeDef _ = return ()

checkElement :: AnnASTElement SemanticAnn -> InitMonad ()
checkElement (Function _ident _ps _ty body _mods _ann) = checkBlock body
checkElement (TypeDefinition tyDef _ann) = checkTypeDef tyDef
checkElement (GlobalDeclaration {}) = return ()

-- | Run the definite assignment check over a single top-level element.
runInitElement :: AnnASTElement SemanticAnn -> Maybe VarUsageError
runInitElement =
  either Just (const Nothing) . run . checkElement

  where

    run :: InitMonad a -> Either VarUsageError a
    run c = fst $ ST.runState (runExceptT c) S.empty

-- | Run the definite assignment check over a whole module, returning the first
-- error.
runInitCheck :: AnnotatedProgram SemanticAnn -> Maybe VarUsageError
runInitCheck = listToMaybe . mapMaybe runInitElement
