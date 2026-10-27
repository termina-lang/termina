-- | The elaboration: rewrites the lowered AST into the elaborated one, deciding
-- which run-time checks are emitted.
--
-- The passes before it show which obligations hold. The elaboration looks up
-- the obligations of each operation as it rewrites it, and the operation takes
-- its unchecked form when all of them have a proof. The report records the
-- outcome of each obligation.
module Elaboration (
    ElaborationReport
  , elaborateProgram
  , elaborateBody
  , elaborateExpression
  , elaborateConstant
) where

import qualified Lowering.AST as LAST
import Elaboration.AST
import Elaboration.Obligations
import Semantic.Types
import Core.Utils (arrayOf)
import Utils.Annotations (getAnnotation)

import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

-- | Each obligation of the program, with the evidence that discharged it when
-- one of the passes showed it to hold.
type ElaborationReport = [(ObligationId, Maybe Evidence)]

type ElaborationM = Writer ElaborationReport

-- | Whether an operation goes unchecked, given its obligations.
type Decide = [Obligation] -> ElaborationM Bool

-- | Takes down each obligation with its proof, if there is one. The operation
-- goes unchecked when all of its obligations have one.
byProofs :: Proofs -> Decide
byProofs proofs obligations = do
  let outcomes = [(oid, M.lookup oid proofs) | Obligation oid _ <- obligations]
  tell outcomes
  return (all (isJust . snd) outcomes)

elaborateProgram :: Proofs -> LAST.AnnotatedProgram SemanticAnn -> (AnnotatedProgram SemanticAnn, ElaborationReport)
elaborateProgram proofs = runWriter . mapM (elaborateElement (byProofs proofs))

-- | Elaborates a body on its own, leaving out the report.
elaborateBody :: Proofs -> LAST.Block SemanticAnn -> Block SemanticAnn
elaborateBody proofs = fst . runWriter . elabBlock (byProofs proofs)

-- | Elaborates an expression on its own, leaving out the report.
elaborateExpression :: Proofs -> LAST.Expression SemanticAnn -> Expression SemanticAnn
elaborateExpression proofs = fst . runWriter . elabExpr (byProofs proofs)

elaborateElement :: Decide -> LAST.AnnASTElement SemanticAnn -> ElaborationM (AnnASTElement SemanticAnn)
elaborateElement d element = case element of
  Function ident params mRet body modifiers ann ->
    Function ident params mRet
      <$> body' body <*> mapM modifier modifiers <*> pure ann
  ExternFunction ident params mRet mode modifiers ann ->
    ExternFunction ident params mRet mode <$> mapM modifier modifiers <*> pure ann
  GlobalDeclaration global ->GlobalDeclaration <$> elaborateGlobal global
  TypeDefinition typeDef ann -> TypeDefinition <$> elaborateTypeDef typeDef <*> pure ann

  where

    body' = elabBlock d
    expression = elabExpr d

    modifier (Modifier ident mExpr) = Modifier ident <$> traverse expression mExpr

    elaborateGlobal global = case global of
      Task ident ty mExpr modifiers ann ->
        Task ident ty <$> traverse expression mExpr <*> mapM modifier modifiers <*> pure ann
      Resource ident ty mExpr modifiers ann ->
        Resource ident ty <$> traverse expression mExpr <*> mapM modifier modifiers <*> pure ann
      Channel ident ty mExpr modifiers ann ->
        Channel ident ty <$> traverse expression mExpr <*> mapM modifier modifiers <*> pure ann
      Emitter ident ty mExpr modifiers ann ->
        Emitter ident ty <$> traverse expression mExpr <*> mapM modifier modifiers <*> pure ann
      Handler ident ty mExpr modifiers ann ->
        Handler ident ty <$> traverse expression mExpr <*> mapM modifier modifiers <*> pure ann
      Test ident ty mExpr modifiers ann ->
        Test ident ty <$> traverse expression mExpr <*> mapM modifier modifiers <*> pure ann
      Const ident ty expr modifiers ann ->
        Const ident ty <$> expression expr <*> mapM modifier modifiers <*> pure ann
      -- | Each use of a constant expression carries a copy of it, which is
      -- folded and checked there, so the declaration gives no code.
      ConstExpr ident ty expr modifiers ann ->
        ConstExpr ident ty (elaborateConstant expr) <$> mapM modifier modifiers <*> pure ann

    elaborateTypeDef typeDef = case typeDef of
      Struct ident fields modifiers -> Struct ident fields <$> mapM modifier modifiers
      Enum ident variants modifiers -> Enum ident variants <$> mapM modifier modifiers
      Class kind ident members provides modifiers ->
        Class kind ident <$> mapM member members <*> pure provides <*> mapM modifier modifiers
      Interface kind ident extends members modifiers ->
        Interface kind ident extends <$> mapM interfaceMember members <*> mapM modifier modifiers

    member m = case m of
      ClassField field -> pure (ClassField field)
      ClassMethod ak ident params mRet body ann ->
        ClassMethod ak ident params mRet <$> body' body <*> pure ann
      ClassProcedure ak ident params body ann ->
        ClassProcedure ak ident params <$> body' body <*> pure ann
      ClassViewer ident params mRet body ann ->
        ClassViewer ident params mRet <$> body' body <*> pure ann
      ClassAction ak ident mParam body ann ->
        ClassAction ak ident mParam <$> body' body <*> pure ann
      ClassCase ak ident params body ann ->
        ClassCase ak ident params <$> body' body <*> pure ann

    interfaceMember (InterfaceProcedure ak ident params modifiers ann) =
      InterfaceProcedure ak ident params <$> mapM modifier modifiers <*> pure ann

-- | Elaborates a constant expression, such as the size of an array type. The
-- constant folding has evaluated its operations, so none of them is checked.
elaborateConstant :: LAST.Expression SemanticAnn -> Expression SemanticAnn
elaborateConstant = fst . runWriter . elabExpr (const (return True))

elabBlock :: Decide -> LAST.Block SemanticAnn -> ElaborationM (Block SemanticAnn)
elabBlock d (Block body ann) = Block <$> mapM (elabBasicBlock d) body <*> pure ann

elabBasicBlock :: Decide -> LAST.BasicBlock SemanticAnn -> ElaborationM (BasicBlock SemanticAnn)
elabBasicBlock d bb = case bb of
  IfElseBlock (CondIf cond body ann) elseIfs mElse ann' ->
    IfElseBlock
      <$> (CondIf <$> expr cond <*> block body <*> pure ann)
      <*> sequence [CondElseIf <$> expr c <*> block b <*> pure a | CondElseIf c b a <- elseIfs]
      <*> traverse (\(CondElse b a) -> CondElse <$> block b <*> pure a) mElse
      <*> pure ann'
  ForLoopBlock iterator ty initE endE mBreak body ann ->
    ForLoopBlock iterator ty <$> expr initE <*> expr endE <*> traverse expr mBreak <*> block body <*> pure ann
  MatchBlock e cases mDefault ann ->
    MatchBlock <$> expr e
      <*> sequence [MatchCase ident vars <$> block b <*> pure a | MatchCase ident vars b a <- cases]
      <*> traverse (\(DefaultCase b a) -> DefaultCase <$> block b <*> pure a) mDefault
      <*> pure ann
  SendMessage obj e ann -> SendMessage <$> object obj <*> expr e <*> pure ann
  ProcedureInvoke obj ident args ann -> ProcedureInvoke <$> object obj <*> pure ident <*> mapM expr args <*> pure ann
  AtomicLoad obj e ann -> AtomicLoad <$> object obj <*> expr e <*> pure ann
  AtomicStore obj e ann -> AtomicStore <$> object obj <*> expr e <*> pure ann
  AtomicArrayLoad obj index e ann -> AtomicArrayLoad <$> object obj <*> atomicIndex obj index <*> expr e <*> pure ann
  AtomicArrayStore obj index e ann -> AtomicArrayStore <$> object obj <*> atomicIndex obj index <*> expr e <*> pure ann
  AllocBox obj e ann -> AllocBox <$> object obj <*> expr e <*> pure ann
  FreeBox obj e ann -> FreeBox <$> object obj <*> expr e <*> pure ann
  DequeTake obj e ann -> DequeTake <$> object obj <*> expr e <*> pure ann
  DequePut end obj e ann -> DequePut end <$> object obj <*> expr e <*> pure ann
  DequeExchange kind obj e out ann -> DequeExchange kind <$> object obj <*> expr e <*> expr out <*> pure ann
  RegularBlock stmts -> RegularBlock <$> mapM statement stmts
  ReturnBlock mExpr ann -> ReturnBlock <$> traverse expr mExpr <*> pure ann
  ContinueBlock e ann -> ContinueBlock <$> expr e <*> pure ann
  RebootBlock ann -> pure (RebootBlock ann)
  NoReturnCall e ann -> NoReturnCall <$> expr e <*> pure ann
  SystemCall obj ident args ann -> SystemCall <$> object obj <*> pure ident <*> mapM expr args <*> pure ann

  where

    expr = elabExpr d
    object = elabObj d
    block = elabBlock d

    -- | The index of an atomic access to an element of an array, which goes
    -- through the bounds check unless its obligation has a proof.
    atomicIndex obj index = do
      unchecked <- d (atomicIndexChecks bb)
      index' <- expr index
      return $ case getTypeSemAnn (getAnnotation obj) >>= arrayOf of
        Just (_, size) | not unchecked ->
          CheckedIndex (elaborateConstant size) index' (getAnnotation index)
        _ -> index'

    statement (Declaration ident ak ty mExpr ann) = Declaration ident ak ty <$> traverse expr mExpr <*> pure ann
    statement (AssignmentStmt obj e ann) = AssignmentStmt <$> object obj <*> expr e <*> pure ann
    statement (SingleExpStmt e ann) = SingleExpStmt <$> expr e <*> pure ann

elabObj :: Decide -> LAST.Object SemanticAnn -> ElaborationM (Object SemanticAnn)
elabObj d obj = case obj of
  LAST.Variable ident ann -> pure (Variable ident ann)
  LAST.ArrayIndexExpression inner index ann -> do
    unchecked <- d (objectChecks obj)
    (if unchecked then UncheckedArrayIndex else CheckedArrayIndex)
      <$> elabObj d inner <*> elabExpr d index <*> pure ann
  LAST.MemberAccess inner ident ann -> MemberAccess <$> elabObj d inner <*> pure ident <*> pure ann
  LAST.Dereference inner ann -> Dereference <$> elabObj d inner <*> pure ann
  LAST.DereferenceMemberAccess inner ident ann -> DereferenceMemberAccess <$> elabObj d inner <*> pure ident <*> pure ann
  LAST.Unbox inner ann -> Unbox <$> elabObj d inner <*> pure ann

elabExpr :: Decide -> LAST.Expression SemanticAnn -> ElaborationM (Expression SemanticAnn)
elabExpr d e = case e of
  LAST.AccessObject obj -> AccessObject <$> object obj
  LAST.Constant c ann -> pure (Constant c ann)
  LAST.BinOp op left right ann -> do
    unchecked <- d (expressionChecks e)
    (if unchecked then UncheckedBinOp else CheckedBinOp) op
      <$> expr left <*> expr right <*> pure ann
  LAST.ReferenceExpression ak obj ann -> ReferenceExpression ak <$> object obj <*> pure ann
  LAST.Casting inner ty ann -> Casting <$> expr inner <*> pure ty <*> pure ann
  LAST.FunctionCall ident args ann -> FunctionCall ident <$> mapM expr args <*> pure ann
  LAST.MemberFunctionCall obj ident args ann -> MemberFunctionCall <$> object obj <*> pure ident <*> mapM expr args <*> pure ann
  LAST.DerefMemberFunctionCall obj ident args ann -> DerefMemberFunctionCall <$> object obj <*> pure ident <*> mapM expr args <*> pure ann
  LAST.ArrayInitializer inner size ann -> ArrayInitializer <$> expr inner <*> expr size <*> pure ann
  LAST.ArrayExprListInitializer exprs ann -> ArrayExprListInitializer <$> mapM expr exprs <*> pure ann
  LAST.StructInitializer fields ann -> StructInitializer <$> mapM field fields <*> pure ann
  LAST.EnumVariantInitializer enum variant args ann -> EnumVariantInitializer enum variant <$> mapM expr args <*> pure ann
  LAST.MonadicVariantInitializer variant ann -> MonadicVariantInitializer <$> monadic variant <*> pure ann
  LAST.StringInitializer str ann -> pure (StringInitializer str ann)
  LAST.IsEnumVariantExpression obj enum variant ann -> (\o -> IsEnumVariantExpression o enum variant ann) <$> object obj
  LAST.IsMonadicVariantExpression obj label ann -> (\o -> IsMonadicVariantExpression o label ann) <$> object obj
  LAST.ArraySliceExpression ak obj lower upper ann -> do
    unchecked <- d (expressionChecks e)
    (if unchecked then UncheckedArraySlice else CheckedArraySlice) ak
      <$> object obj <*> expr lower <*> expr upper <*> pure ann

  where

    expr = elabExpr d
    object = elabObj d

    field (FieldValueAssignment ident inner ann) = FieldValueAssignment ident <$> expr inner <*> pure ann
    field (FieldAddressAssignment ident inner ann) = FieldAddressAssignment ident <$> expr inner <*> pure ann
    field (FieldPortConnection kind ident glb ann) = pure (FieldPortConnection kind ident glb ann)

    monadic variant = case variant of
      Some inner -> Some <$> expr inner
      Ok inner -> Ok <$> expr inner
      Error inner -> Error <$> expr inner
      Failure inner -> Failure <$> expr inner
      None -> pure None
      Success -> pure Success
