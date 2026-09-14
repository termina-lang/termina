-- | Definite assignment and variable usage check.
--
-- This pass walks the basic blocks, checking that: 
-- - An object declared without an initializer may not be read, nor may one of
-- its fields or elements be written, before the whole object is assigned
-- (VE-007, VE-008).  
-- - Whether an identifier is read at all somewhere, which is what the unused
-- variable, unused field and uncalled member function checks need (VE-001,
-- VE-002, VE-003, VE-004, VE-005).  
-- - A value that nobody reads before it is overwritten is a dead store
-- (VE-006).
--
-- Accordingly, the state keeps one accumulator per body for the objects that
-- are declared and not assigned yet, one per top-level element for every
-- identifier that is read, and one per body for the assignments whose value is
-- still unread. The second one spans the whole class because a field is used
-- when /any/ of its members reads it.
module ControlFlow.VarUsage (runVarUsageCheck) where

import Control.Monad (when, unless)
import Control.Monad.Except
import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe)

import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
import ControlFlow.VarUsage.Errors
import Semantic.Types (SemanticAnn, getObjectSAnns)
import Utils.Annotations

data VarUsageSt = VarUsageSt
  {
    -- | Objects declared without an initializer that are not assigned yet. An
    -- object is initialized when it is not in this set, so joining two paths is
    -- the union of their sets.
    pending :: S.Set Identifier,
    -- | Identifiers read so far. Besides variables, it holds the names of the
    -- fields reached through @self->@ and, under the key of
    -- 'memberFunctionKey', the member functions called through @self@.
    readIdents :: S.Set Identifier,
    -- | Objects declared in the body being checked, with the location to blame
    -- if nobody reads them.
    declared :: [(Identifier, Location)],
    -- | Assignments of a whole object that reach this point along the current
    -- path without having been read, kept under the name assigned and located
    -- by the assignment itself. Joining two paths is the union of their maps,
    -- since a value is worth reporting as soon as one path leaves it unread.
    unread :: M.Map Identifier (S.Set Location),
    -- | Assignments read at some point along some path. An assignment read
    -- anywhere is not dead, so this set is what rescues the candidates below.
    readDefs :: S.Set Location,
    -- | Assignments overwritten before being read, in the order they were
    -- found. They are only candidates: the walk of a loop body reports the
    -- same one on every turn, and a later turn may still read it.
    deadStores :: [(Identifier, Location)],
    -- | Of the assignments above, the ones that are the initializer of a
    -- declaration. They are reported apart because the remedy is not the same:
    -- an assignment is removed, an initializer is moved or dropped.
    initializers :: S.Set Location
  }

-- | What a branch leaves behind: the objects it does not assign and the
-- assignments it does not read.
type BranchOut = (S.Set Identifier, M.Map Identifier (S.Set Location))

type VarUsageMonad = ExceptT VarUsageError (ST.State VarUsageSt)

emptySt :: VarUsageSt
emptySt = VarUsageSt S.empty S.empty [] M.empty S.empty [] S.empty

-- | Key under which a call to a member function through self is recorded. It
-- is not a valid identifier, so it cannot clash with the name of a variable.
memberFunctionKey :: Identifier -> Identifier
memberFunctionKey ident = "self->" ++ ident ++ "()"

-- | Key under which the read of a field is recorded. Fields and variables
-- share the set of identifiers read, so the key spells out the access the
-- field is reached through: that keeps a field from rescuing a variable of the
-- same name, and the field of one object from answering for the field of
-- another.
fieldKey :: Object SemanticAnn -> Identifier -> Identifier
fieldKey obj ident = objectKey obj ++ "->" ++ ident

-- | The key of a field of the class being checked, which its own members reach
-- through self.
selfFieldKey :: Identifier -> Identifier
selfFieldKey ident = "self->" ++ ident

-- | Spelling of an access, which is what makes the keys above unique. It is
-- canonical: dereferencing and unboxing do not show, and both field accessors
-- render alike, so that @(*obj).field@ and @obj->field@ share one key. Indices
-- are not part of it either, so every element of an array shares one.
objectKey :: Object SemanticAnn -> Identifier
objectKey (Variable ident _) = ident
objectKey (ArrayIndexExpression obj _ _) = objectKey obj ++ "[]"
objectKey (MemberAccess obj ident _) = objectKey obj ++ "->" ++ ident
objectKey (Dereference obj _) = objectKey obj
objectKey (DereferenceMemberAccess obj ident _) = objectKey obj ++ "->" ++ ident
objectKey (Unbox obj _) = objectKey obj

markDeclared :: Identifier -> Location -> VarUsageMonad ()
markDeclared ident loc = ST.modify (\st -> st {
    pending = S.insert ident (pending st),
    declared = (ident, loc) : declared st
  })

-- | A declaration with an initializer: the object has a value from the start,
-- but it still has to be read by somebody.
markInitialized :: Identifier -> Location -> VarUsageMonad ()
markInitialized ident loc = ST.modify (\st -> st {
    pending = S.delete ident (pending st),
    declared = (ident, loc) : declared st
  })

markAssigned :: Identifier -> VarUsageMonad ()
markAssigned ident = ST.modify (\st -> st { pending = S.delete ident (pending st) })

-- | Reading an identifier reads whatever assignments of it reach this point,
-- which takes them out of the candidates for good.
markRead :: Identifier -> VarUsageMonad ()
markRead ident = ST.modify (\st -> st {
    readIdents = S.insert ident (readIdents st),
    readDefs = S.union (M.findWithDefault S.empty ident (unread st)) (readDefs st),
    unread = M.delete ident (unread st)
  })

-- | An assignment is killed when the whole object is assigned again and when
-- the body ends. One killed unread is a candidate to be reported.
killAssignments :: Identifier -> VarUsageMonad ()
killAssignments ident = ST.modify (\st -> st {
    unread = M.delete ident (unread st),
    deadStores = deadStores st ++
      [(ident, loc) | loc <- S.toList (M.findWithDefault S.empty ident (unread st))]
  })

-- | Assignment of a whole object: it kills whatever reached this point.
markAssignment :: Identifier -> Location -> VarUsageMonad ()
markAssignment ident loc = do
  killAssignments ident
  ST.modify (\st -> st { unread = M.insert ident (S.singleton loc) (unread st) })

checkRead :: Identifier -> Location -> VarUsageMonad ()
checkRead ident loc = do
  notAssigned <- ST.gets pending
  when (S.member ident notAssigned)
    (throwError $ annotateError loc (EReadBeforeAssignment ident))
  markRead ident

-- | A write to a field or to an element only makes sense once the whole object
-- has a value.
checkPartialWrite :: Object SemanticAnn -> Location -> VarUsageMonad ()
checkPartialWrite obj loc = do
  let ident = rootIdent obj
  notAssigned <- ST.gets pending
  when (S.member ident notAssigned)
    (throwError $ annotateError loc (EPartialWriteBeforeAssignment ident))

-- | Reading an object reads its root variable, every field along the way, which
-- is what the unused-field check needs, and the expressions that index it.
readObject :: Object SemanticAnn -> VarUsageMonad ()
readObject = walkObject ObjectVisitor
  {
    atRoot = \ident ann -> checkRead ident (getLocation ann)
  , atField = \_ obj ident -> markRead (fieldKey obj ident)
  , atIndex = readExpression
  }

-- | Writing into an object does not use it: an object that is written and
-- never read is dead code, and the unused check is the one that says so.
--
-- There are two exceptions, and both are about an effect that is observable
-- from outside the body. The first one is a write that goes through a
-- dereference, which means the object belongs to somebody else: writing an
-- out parameter, @*status = Failure(e)@, uses it, and so does writing a field
-- through @self@. Only the root is used that way, so a field that is written
-- and never read is still dead. The second one is a memory-mapped location,
-- where the write is the effect: a hardware register is written and never read
-- back, and there the whole access counts.
markWrittenObject :: Object SemanticAnn -> VarUsageMonad ()
markWrittenObject obj = do
  when (isFixedLocation obj) (markChain obj)
  when (goesThroughReference obj) (markRead (rootIdent obj))
  when (rootIdent obj == "self") (markRead "self")

  where

    -- | Whether the object written belongs to somebody else. It does when the
    -- access dereferences, and also when it goes through an object of
    -- reference type: a reference to an array is indexed without dereferencing
    -- it first, as in @paction_num[i] = ...@ over a @&mut [usize; 4]@.
    goesThroughReference :: Object SemanticAnn -> Bool
    goesThroughReference (Dereference _ _) = True
    goesThroughReference (DereferenceMemberAccess {}) = True
    goesThroughReference o@(ArrayIndexExpression inner _ _) =
      isReference o || isReference inner || goesThroughReference inner
    goesThroughReference o@(MemberAccess inner _ _) =
      isReference o || isReference inner || goesThroughReference inner
    goesThroughReference o@(Unbox inner _) =
      isReference o || goesThroughReference inner
    goesThroughReference o@(Variable _ _) = isReference o

    isReference :: Object SemanticAnn -> Bool
    isReference o =
      case getObjectSAnns (getAnnotation o) of
        Just (_, TReference {}) -> True
        _ -> False

    isFixedLocation :: Object SemanticAnn -> Bool
    isFixedLocation o =
      case getObjectSAnns (getAnnotation o) of
        Just (_, TFixedLocation _) -> True
        _ -> case o of
          ArrayIndexExpression inner _ _ -> isFixedLocation inner
          MemberAccess inner _ _ -> isFixedLocation inner
          Dereference inner _ -> isFixedLocation inner
          DereferenceMemberAccess inner _ _ -> isFixedLocation inner
          Unbox inner _ -> isFixedLocation inner
          Variable _ _ -> False

    -- | Marks every name the access goes through, so that writing
    -- @self->registers.data@ uses both the field and the receiver.
    markChain :: Object SemanticAnn -> VarUsageMonad ()
    markChain = walkObject ObjectVisitor
      {
        atRoot = \ident _ -> markRead ident
      , atField = \_ inner ident -> markRead (fieldKey inner ident)
      , atIndex = const (return ())
      }

-- | The index expressions of an object that is being written into. The object
-- itself is not read, only the indices are.
readIndices :: Object SemanticAnn -> VarUsageMonad ()
readIndices = walkObject ObjectVisitor
  {
    atRoot = \_ _ -> return ()
  , atField = \_ _ _ -> return ()
  , atIndex = readExpression
  }

-- | Records a call to a member function of the class made through self.
readSelfMemberFunction :: Object SemanticAnn -> Identifier -> VarUsageMonad ()
readSelfMemberFunction (Variable "self" _) ident = markRead (memberFunctionKey ident)
readSelfMemberFunction (Dereference (Variable "self" _) _) ident = markRead (memberFunctionKey ident)
readSelfMemberFunction _ _ = return ()

-- | Taking a reference, either @&@ or @&mut@, counts as a read: the receiver
-- may read what it is given. A constant expression that belongs to a type does
-- not, since the program does not compute it.
readExpression :: Expression SemanticAnn -> VarUsageMonad ()
readExpression expr = do
  case expr of
    MemberFunctionCall obj ident _ _ -> readSelfMemberFunction obj ident
    DerefMemberFunctionCall obj ident _ _ -> readSelfMemberFunction obj ident
    _ -> return ()
  mapM_ readChild (expressionChildren expr)

readChild :: Child SemanticAnn -> VarUsageMonad ()
readChild (ChildObject obj) = readObject obj
readChild (ChildReference _ obj) = readObject obj
readChild (ChildExpr e) = readExpression e
readChild (ChildArg e) = readExpression e
readChild (ChildConstExpr _) = return ()

checkStatement :: Statement SemanticAnn -> VarUsageMonad ()
checkStatement (Declaration ident _ _ Nothing ann) = markDeclared ident (getLocation ann)
-- | The value an initializer gives the object is an assignment like any
-- other, so one that nobody reads before it is overwritten is a dead store.
-- The object is then meant to be declared without an initializer.
checkStatement (Declaration ident _ _ (Just initExpr) ann) = do
  readExpression initExpr
  markInitialized ident (getLocation ann)
  markAssignment ident (getLocation ann)
  ST.modify (\st -> st {
      initializers = S.insert (getLocation ann) (initializers st)
    })
checkStatement (AssignmentStmt obj e ann) = do
  readExpression e
  case obj of
    -- | The whole object is assigned
    Variable ident _ -> markAssigned ident >> markAssignment ident (getLocation ann)
    -- | Only a part of it is
    _ -> checkPartialWrite obj (getLocation ann) >> markWrittenObject obj >> readIndices obj
checkStatement (SingleExpStmt e _) = readExpression e

-- | Checks one branch from the current state and returns the objects it leaves
-- unassigned, restoring the entry state so that the next branch starts where
-- this one did. What the branch reads is kept, since reading does not depend
-- on the path.
checkBranch :: Block SemanticAnn -> VarUsageMonad BranchOut
checkBranch blk = do
  entry <- currentOut
  checkBlock blk
  out <- currentOut
  ST.modify (\st -> st { pending = fst entry, unread = snd entry })
  return out

-- | What the path being walked has left behind so far.
currentOut :: VarUsageMonad BranchOut
currentOut = ST.gets (\st -> (pending st, unread st))

-- | Joins the paths that meet after a conditional: an object is initialized
-- when every path assigns it, and a value is left unread when any path leaves
-- it unread.
joinBranches :: [BranchOut] -> VarUsageMonad ()
joinBranches outs = ST.modify (\st -> st {
    pending = S.unions (map fst outs),
    unread = M.unionsWith S.union (map snd outs)
  })

checkBasicBlock :: BasicBlock SemanticAnn -> VarUsageMonad ()
checkBasicBlock (RegularBlock stmts) = mapM_ checkStatement stmts
checkBasicBlock (IfElseBlock condIf elseIfs mElse _) = do
  readExpression (condIfCond condIf)
  ifOut <- checkBranch (condIfBody condIf)
  elseIfOuts <- mapM
    (\elseIf -> readExpression (condElseIfCond elseIf) >> checkBranch (condElseIfBody elseIf))
    elseIfs
  entry <- currentOut
  -- | Without an else branch there is a path that assigns nothing
  elseOut <- maybe (return entry) (checkBranch . condElseBody) mElse
  joinBranches (ifOut : elseOut : elseIfOuts)
checkBasicBlock (MatchBlock e cases mDefaultCase _) = do
  readExpression e
  caseOuts <- mapM checkMatchCase cases
  entry <- currentOut
  case mDefaultCase of
    Just (DefaultCase blk _) -> do
      defaultOut <- checkBranch blk
      joinBranches (defaultOut : caseOuts)
    -- | Without a default case the listed cases are exhaustive
    Nothing -> joinBranches (if null caseOuts then [entry] else caseOuts)
checkBasicBlock (ForLoopBlock _ _ initE endE mBreak blk _) = do
  readExpression initE
  readExpression endE
  mapM_ readExpression mBreak
  entry <- ST.gets unread
  bodyUnread <- loopUnread entry
  -- | The body may not run, so what it assigns does not count afterwards,
  -- while what it leaves unread may still be read after the loop
  ST.modify (\st -> st { unread = M.unionWith S.union entry bodyUnread })

  where

    -- | The assignments the body leaves unread. A value assigned in one
    -- iteration may be read in the next one, so the body is walked again with
    -- what the previous walk left unread, until the set stops growing; the
    -- reads of those extra walks are what rescue the candidates.
    loopUnread :: M.Map Identifier (S.Set Location)
      -> VarUsageMonad (M.Map Identifier (S.Set Location))
    loopUnread known = do
      ST.modify (\st -> st { unread = known })
      (_, out) <- checkBranch blk
      let known' = M.unionWith S.union known out
      if known' == known then return out else loopUnread known'
-- | Every other block only evaluates the expressions it holds, in order.
-- 'simpleBlockChildren' is exhaustive, so a new kind of block breaks its
-- definition rather than this one; a new block that branches has to be given a
-- case above.
checkBasicBlock block = mapM_ (mapM_ readChild) (simpleBlockChildren block)

-- | The variables a case binds are declared by the case itself.
checkMatchCase :: MatchCase SemanticAnn -> VarUsageMonad BranchOut
checkMatchCase (MatchCase _ bvars body ann) = do
  mapM_ (`markInitialized` getLocation ann) bvars
  checkBranch body

checkBlock :: Block SemanticAnn -> VarUsageMonad ()
checkBlock = mapM_ checkBasicBlock . blockBody

-- | An identifier that is declared and never read is dead, unless its name
-- starts with an underscore, in which case it is the other way round.
checkDeclaredAreRead :: VarUsageMonad ()
checkDeclaredAreRead = do
  st <- ST.get
  mapM_ (check (readIdents st)) (declared st)

  where

    check :: S.Set Identifier -> (Identifier, Location) -> VarUsageMonad ()
    check reads' (ident, loc) =
      case ident of
        ('_' : _) -> when (S.member ident reads')
          (throwError $ annotateError loc (EUsedIgnoredParameter ident))
        _ -> unless (S.member ident reads')
          (throwError $ annotateError loc (ENotUsed ident))

-- | When the body ends, every assignment still unread is killed, and the
-- candidates that nobody read along any path are the dead stores. The check is
-- left for the end because a candidate found early may be read later, either
-- in another turn of a loop or along another branch.
checkDeadStores :: VarUsageMonad ()
checkDeadStores = do
  idents <- ST.gets (M.keys . unread)
  mapM_ killAssignments idents
  st <- ST.get
  mapM_ (report (initializers st))
    (filter (not . (`S.member` readDefs st) . snd) (deadStores st))

  where

    -- | The value of an initializer and the value of an assignment are dead in
    -- the same way, but they are not removed in the same way, so each one is
    -- reported with the remedy that fits it.
    report :: S.Set Location -> (Identifier, Location) -> VarUsageMonad ()
    report inits (ident, loc)
      | S.member loc inits = throwError $ annotateError loc (EInitializerNotUsed ident)
      | otherwise = throwError $ annotateError loc (EAssignedValueNotUsed ident)

-- | Runs the body of a member or of a function: the objects pending
-- assignment, the declarations and the assignments are its own, the
-- identifiers read are not.
checkBody :: [Parameter SemanticAnn] -> Location -> Block SemanticAnn -> VarUsageMonad ()
checkBody ps loc body = do
  ST.modify (\st -> st {
      pending = S.empty, declared = [],
      unread = M.empty, readDefs = S.empty, deadStores = [],
      initializers = S.empty
    })
  mapM_ (\p -> markInitialized (paramIdentifier p) loc) (filter (not . isBoxParam) ps)
  checkBlock body
  checkDeclaredAreRead
  checkDeadStores

  where

    -- | A box parameter is consumed, not read, so the linearity check owns it.
    isBoxParam :: Parameter SemanticAnn -> Bool
    isBoxParam p = case paramType p of
      TBoxSubtype _ -> True
      _ -> False

checkClassMember :: ClassMember SemanticAnn -> VarUsageMonad ()
checkClassMember (ClassMethod _ak ident ps _tyret body ann) =
  checkSelfBody (ESelfNotUsed ident) (getLocation ann) (checkBody ps (getLocation ann) body)
checkClassMember (ClassViewer ident ps _tyret body ann) =
  checkSelfBody (ESelfNotUsed ident) (getLocation ann) (checkBody ps (getLocation ann) body)
checkClassMember (ClassAction _ak ident mp _tyret body ann) =
  checkSelfBody (EActionSelfNotUsed ident) (getLocation ann)
    (checkBody (maybe [] (: []) mp) (getLocation ann) body)
checkClassMember (ClassProcedure _ak ident ps body ann) =
  checkSelfBody (ESelfNotUsed ident) (getLocation ann) (checkBody ps (getLocation ann) body)
checkClassMember (ClassField {}) = return ()

-- | Methods, viewers and actions must use self. Since the identifiers read are
-- shared by every member of the class, self is taken out of the set before the
-- body and looked up again afterwards.
checkSelfBody :: Error -> Location -> VarUsageMonad () -> VarUsageMonad ()
checkSelfBody err loc body = do
  ST.modify (\st -> st { readIdents = S.delete "self" (readIdents st) })
  body
  wasRead <- ST.gets (S.member "self" . readIdents)
  unless wasRead (throwError $ annotateError loc err)

-- | A field is used when any member of the class reads it. Sink and in ports
-- are driven by the runtime, so they are not read by anybody.
checkFieldIsRead :: ClassMember SemanticAnn -> VarUsageMonad ()
checkFieldIsRead (ClassField (FieldDefinition _ (TSinkPort {}) _)) = return ()
checkFieldIsRead (ClassField (FieldDefinition _ (TInPort {}) _)) = return ()
checkFieldIsRead (ClassField fdef) = do
  wasRead <- ST.gets (S.member (selfFieldKey (fieldIdentifier fdef)) . readIdents)
  unless wasRead
    (throwError $ annotateError (getLocation (fieldAnnotation fdef))
      (ENotUsed (fieldIdentifier fdef)))
checkFieldIsRead _ = return ()

-- | Methods and viewers can only be called through self by the members of
-- their own class, so one that no member calls is never used.
checkMemberFunctionIsCalled :: ClassMember SemanticAnn -> VarUsageMonad ()
checkMemberFunctionIsCalled (ClassMethod _ak ident _ps _tyret _body ann) =
  checkCalled ident (getLocation ann)
checkMemberFunctionIsCalled (ClassViewer ident _ps _tyret _body ann) =
  checkCalled ident (getLocation ann)
checkMemberFunctionIsCalled _ = return ()

checkCalled :: Identifier -> Location -> VarUsageMonad ()
checkCalled ident loc = do
  wasCalled <- ST.gets (S.member (memberFunctionKey ident) . readIdents)
  unless wasCalled (throwError $ annotateError loc (EMemberFunctionNotUsed ident))

checkTypeDef :: TypeDef SemanticAnn -> VarUsageMonad ()
checkTypeDef (Class _kind _ident members _provides _mods) = do
  mapM_ checkClassMember members
  mapM_ checkMemberFunctionIsCalled members
  mapM_ checkFieldIsRead members
checkTypeDef _ = return ()

checkElement :: AnnASTElement SemanticAnn -> VarUsageMonad ()
checkElement (Function _ident ps _ty body _mods ann) = checkBody ps (getLocation ann) body
checkElement (TypeDefinition tyDef _ann) = checkTypeDef tyDef
checkElement (GlobalDeclaration {}) = return ()

-- | Run the check over a single top-level element.
runInitElement :: AnnASTElement SemanticAnn -> Maybe VarUsageError
runInitElement =
  either Just (const Nothing) . run . checkElement

  where

    run :: VarUsageMonad a -> Either VarUsageError a
    run c = fst $ ST.runState (runExceptT c) emptySt

-- | Run the check over a whole module, returning the first error.
runVarUsageCheck :: AnnotatedProgram SemanticAnn -> Maybe VarUsageError
runVarUsageCheck = listToMaybe . mapMaybe runInitElement
