-- | The elaboration: rewrites the lowered AST into the elaborated one, deciding
-- which run-time checks are emitted.
--
-- The obligations of each body go to the provers, and an operation takes its
-- unchecked form when they discharge all of its obligations. The expressions
-- outside any body, which are the initializers of the globals and the
-- modifiers, go through the same provers one at a time. The report records
-- the outcome of each obligation.
module Elaboration (
    ElaborationReport
  , provers
  , elaborateProgram
  , elaborateBody
  , elaborateExpression
  , elaborateConstant
) where

import qualified Lowering.AST as L
import Elaboration.AST
import Elaboration.Obligations
import Elaboration.Prover
import Elaboration.Prover.Constant (constantProver)
import Semantic.Types
import Core.Utils (arrayOf)
import Utils.Annotations (getAnnotation)

import Control.Monad.Writer
import qualified Data.Map.Strict as M

-- | Each obligation of the program, with the evidence that discharged it when
-- one of the provers did.
type ElaborationReport = [(ObligationId, Maybe Evidence)]

-- | The provers that need nothing but the program. The value prover reads what
-- the value analysis found, so the build adds it for each module.
provers :: [Prover]
provers = [constantProver]

type ElaborationM = Writer ElaborationReport

-- | Whether the obligation is discharged.
type Discharged = ObligationId -> Bool

elaborateProgram :: [Prover] -> L.AnnotatedProgram SemanticAnn -> (AnnotatedProgram SemanticAnn, ElaborationReport)
elaborateProgram ps = runWriter . mapM (elaborateElement ps)

-- | Elaborates a body on its own, leaving out the report.
elaborateBody :: [Prover] -> L.Block SemanticAnn -> Block SemanticAnn
elaborateBody ps = fst . runWriter . elaborateBodyM ps

-- | Elaborates an expression on its own, leaving out the report.
elaborateExpression :: [Prover] -> L.Expression SemanticAnn -> Expression SemanticAnn
elaborateExpression ps = fst . runWriter . elaborateExpressionM ps

elaborateScope :: [Prover] -> Scope -> ElaborationM Discharged
elaborateScope ps scope = do
  let (obligations, proved) = discharge ps scope
  tell [(oid, M.lookup oid proved) | Obligation oid _ <- obligations]
  return (`M.member` proved)

elaborateBodyM :: [Prover] -> L.Block SemanticAnn -> ElaborationM (Block SemanticAnn)
elaborateBodyM ps blk = (`elabBlock` blk) <$> elaborateScope ps (BodyScope blk)

elaborateExpressionM :: [Prover] -> L.Expression SemanticAnn -> ElaborationM (Expression SemanticAnn)
elaborateExpressionM ps expr = (`elabExpr` expr) <$> elaborateScope ps (ExpressionScope expr)

elaborateElement :: [Prover] -> L.AnnASTElement SemanticAnn -> ElaborationM (AnnASTElement SemanticAnn)
elaborateElement ps element = case element of
  Function ident params mRet body modifiers ann ->
    Function ident params mRet
      <$> body' body <*> mapM modifier modifiers <*> pure ann
  GlobalDeclaration global -> GlobalDeclaration <$> elaborateGlobal global
  TypeDefinition typeDef ann -> TypeDefinition <$> elaborateTypeDef typeDef <*> pure ann

  where

    body' = elaborateBodyM ps
    expression = elaborateExpressionM ps

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
      Const ident ty expr modifiers ann ->
        Const ident ty <$> expression expr <*> mapM modifier modifiers <*> pure ann
      ConstExpr ident ty expr modifiers ann ->
        ConstExpr ident ty <$> expression expr <*> mapM modifier modifiers <*> pure ann

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
      ClassAction ak ident mParam ret body ann ->
        ClassAction ak ident mParam ret <$> body' body <*> pure ann

    interfaceMember (InterfaceProcedure ak ident params modifiers ann) =
      InterfaceProcedure ak ident params <$> mapM modifier modifiers <*> pure ann

-- | Elaborates a constant expression, such as the size of an array type. The
-- constant folding has evaluated its operations, so none of them is checked.
elaborateConstant :: L.Expression SemanticAnn -> Expression SemanticAnn
elaborateConstant = elabExpr (const True)

elabBlock :: Discharged -> L.Block SemanticAnn -> Block SemanticAnn
elabBlock d (Block body ann) = Block (map (elabBasicBlock d) body) ann

elabBasicBlock :: Discharged -> L.BasicBlock SemanticAnn -> BasicBlock SemanticAnn
elabBasicBlock d bb = case bb of
  IfElseBlock (CondIf cond body ann) elseIfs mElse ann' ->
    IfElseBlock (CondIf (expr cond) (block body) ann)
      [CondElseIf (expr c) (block b) a | CondElseIf c b a <- elseIfs]
      ((\(CondElse b a) -> CondElse (block b) a) <$> mElse) ann'
  ForLoopBlock iterator ty initE endE mBreak body ann ->
    ForLoopBlock iterator ty (expr initE) (expr endE) (expr <$> mBreak) (block body) ann
  MatchBlock e cases mDefault ann ->
    MatchBlock (expr e)
      [MatchCase ident vars (block b) a | MatchCase ident vars b a <- cases]
      ((\(DefaultCase b a) -> DefaultCase (block b) a) <$> mDefault) ann
  SendMessage obj e ann -> SendMessage (object obj) (expr e) ann
  ProcedureInvoke obj ident args ann -> ProcedureInvoke (object obj) ident (map expr args) ann
  AtomicLoad obj e ann -> AtomicLoad (object obj) (expr e) ann
  AtomicStore obj e ann -> AtomicStore (object obj) (expr e) ann
  AtomicArrayLoad obj index e ann -> AtomicArrayLoad (object obj) (atomicIndex obj index) (expr e) ann
  AtomicArrayStore obj index e ann -> AtomicArrayStore (object obj) (atomicIndex obj index) (expr e) ann
  AllocBox obj e ann -> AllocBox (object obj) (expr e) ann
  FreeBox obj e ann -> FreeBox (object obj) (expr e) ann
  RegularBlock stmts -> RegularBlock (map statement stmts)
  ReturnBlock mExpr ann -> ReturnBlock (expr <$> mExpr) ann
  ContinueBlock e ann -> ContinueBlock (expr e) ann
  RebootBlock ann -> RebootBlock ann
  SystemCall obj ident args ann -> SystemCall (object obj) ident (map expr args) ann

  where

    expr = elabExpr d
    object = elabObj d
    block = elabBlock d

    -- | The index of an atomic access to an element of an array, which goes
    -- through the bounds check unless the provers discharge it.
    atomicIndex obj index =
      case getTypeSemAnn (getAnnotation obj) >>= arrayOf of
        Just (_, size) ->
          if unchecked d (atomicIndexChecks bb)
            then expr index
            else CheckedIndex (elaborateConstant size) (expr index) (getAnnotation index)
        Nothing -> expr index

    statement (Declaration ident ak ty mExpr ann) = Declaration ident ak ty (expr <$> mExpr) ann
    statement (AssignmentStmt obj e ann) = AssignmentStmt (object obj) (expr e) ann
    statement (SingleExpStmt e ann) = SingleExpStmt (expr e) ann

-- | Whether every obligation of the node is discharged.
unchecked :: Discharged -> [Obligation] -> Bool
unchecked d = all (d . obligationId)

elabObj :: Discharged -> L.Object SemanticAnn -> Object SemanticAnn
elabObj d obj = case obj of
  L.Variable ident ann -> Variable ident ann
  L.ArrayIndexExpression inner index ann ->
    (if unchecked d (objectChecks obj) then UncheckedArrayIndex else CheckedArrayIndex)
      (elabObj d inner) (elabExpr d index) ann
  L.MemberAccess inner ident ann -> MemberAccess (elabObj d inner) ident ann
  L.Dereference inner ann -> Dereference (elabObj d inner) ann
  L.DereferenceMemberAccess inner ident ann -> DereferenceMemberAccess (elabObj d inner) ident ann
  L.Unbox inner ann -> Unbox (elabObj d inner) ann

elabExpr :: Discharged -> L.Expression SemanticAnn -> Expression SemanticAnn
elabExpr d e = case e of
  L.AccessObject obj -> AccessObject (object obj)
  L.Constant c ann -> Constant c ann
  L.BinOp op left right ann ->
    (if unchecked d (expressionChecks e) then UncheckedBinOp else CheckedBinOp)
      op (expr left) (expr right) ann
  L.ReferenceExpression ak obj ann -> ReferenceExpression ak (object obj) ann
  L.Casting inner ty ann -> Casting (expr inner) ty ann
  L.FunctionCall ident args ann -> FunctionCall ident (map expr args) ann
  L.MemberFunctionCall obj ident args ann -> MemberFunctionCall (object obj) ident (map expr args) ann
  L.DerefMemberFunctionCall obj ident args ann -> DerefMemberFunctionCall (object obj) ident (map expr args) ann
  L.ArrayInitializer inner size ann -> ArrayInitializer (expr inner) (expr size) ann
  L.ArrayExprListInitializer exprs ann -> ArrayExprListInitializer (map expr exprs) ann
  L.StructInitializer fields ann -> StructInitializer (map field fields) ann
  L.EnumVariantInitializer enum variant args ann -> EnumVariantInitializer enum variant (map expr args) ann
  L.MonadicVariantInitializer variant ann -> MonadicVariantInitializer (monadic variant) ann
  L.StringInitializer str ann -> StringInitializer str ann
  L.IsEnumVariantExpression obj enum variant ann -> IsEnumVariantExpression (object obj) enum variant ann
  L.IsMonadicVariantExpression obj label ann -> IsMonadicVariantExpression (object obj) label ann
  L.ArraySliceExpression ak obj lower upper ann ->
    (if unchecked d (expressionChecks e) then UncheckedArraySlice else CheckedArraySlice)
      ak (object obj) (expr lower) (expr upper) ann

  where

    expr = elabExpr d
    object = elabObj d

    field (FieldValueAssignment ident inner ann) = FieldValueAssignment ident (expr inner) ann
    field (FieldAddressAssignment ident inner ann) = FieldAddressAssignment ident (expr inner) ann
    field (FieldPortConnection kind ident glb ann) = FieldPortConnection kind ident glb ann

    monadic variant = case variant of
      Some inner -> Some (expr inner)
      Ok inner -> Ok (expr inner)
      Error inner -> Error (expr inner)
      Failure inner -> Failure (expr inner)
      None -> None
      Success -> Success
