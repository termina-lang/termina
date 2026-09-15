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
-- Accordingly the state is split the way 'ControlFlow.Dataflow' asks for it:
-- what a branch has to give back when it ends (the objects not assigned yet and
-- the assignments nobody has read) and what it may not (which identifiers are
-- read anywhere, and which assignments turned out to be read). The identifiers
-- read span the whole class, because a field is used when /any/ of its members
-- reads it.
module ControlFlow.VarUsage (runVarUsageCheck) where

import Control.Monad (when, unless)
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe)

import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
    (Child(..), ObjectVisitor(..), expressionChildren, simpleBlockChildren,
     walkObject, rootIdent)
import ControlFlow.Dataflow
import ControlFlow.VarUsage.Errors
import Semantic.Types (SemanticAnn, getObjectSAnns)
import Utils.Annotations

-- | What belongs to the path being walked.
data VarUsagePath = VarUsagePath
  {
    -- | Objects declared without an initializer that are not assigned yet. An
    -- object is initialized when it is not in this set, so joining two paths is
    -- the union of their sets.
    pending :: S.Set Identifier,
    -- | Assignments of a whole object that reach this point along the current
    -- path without having been read, kept under the name assigned and located
    -- by the assignment itself. Joining two paths is the union of their maps,
    -- since a value is worth reporting as soon as one path leaves it unread.
    unread :: M.Map Identifier (S.Set Location)
  } deriving Eq

instance Lattice VarUsagePath where
  joinPath left right = VarUsagePath
    (S.union (pending left) (pending right))
    (M.unionWith S.union (unread left) (unread right))

-- | What belongs to the program, and no branch takes back.
data VarUsageGlobal = VarUsageGlobal
  {
    -- | Identifiers read so far. Besides variables, it holds the names of the
    -- fields reached through @self->@ and, under the key of
    -- 'memberFunctionKey', the member functions called through @self@.
    readIdents :: S.Set Identifier,
    -- | Objects declared in the body being checked, with the location to blame
    -- if nobody reads them.
    declared :: [(Identifier, Location)],
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

type VarUsageMonad = DataflowM VarUsagePath VarUsageGlobal VarUsageError

emptySt :: DFState VarUsagePath VarUsageGlobal
emptySt = DFState (VarUsagePath S.empty M.empty) (VarUsageGlobal S.empty [] S.empty [] S.empty)

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
markDeclared ident loc = do
  modifyPath (\p -> p { pending = S.insert ident (pending p) })
  modifyGlobal (\g -> g { declared = (ident, loc) : declared g })

-- | A declaration with an initializer: the object has a value from the start,
-- but it still has to be read by somebody.
markInitialized :: Identifier -> Location -> VarUsageMonad ()
markInitialized ident loc = do
  markAssigned ident
  modifyGlobal (\g -> g { declared = (ident, loc) : declared g })

markAssigned :: Identifier -> VarUsageMonad ()
markAssigned ident = modifyPath (\p -> p { pending = S.delete ident (pending p) })

-- | Reading an identifier reads whatever assignments of it reach this point,
-- which takes them out of the candidates for good.
markRead :: Identifier -> VarUsageMonad ()
markRead ident = do
  reaching <- unreadOf ident
  modifyGlobal (\g -> g {
      readIdents = S.insert ident (readIdents g),
      readDefs = S.union reaching (readDefs g)
    })
  modifyPath (\p -> p { unread = M.delete ident (unread p) })

-- | The assignments of an identifier that reach this point unread.
unreadOf :: Identifier -> VarUsageMonad (S.Set Location)
unreadOf ident = M.findWithDefault S.empty ident . unread <$> getPath

-- | An assignment is killed when the whole object is assigned again and when
-- the body ends. One killed unread is a candidate to be reported.
killAssignments :: Identifier -> VarUsageMonad ()
killAssignments ident = do
  killed <- unreadOf ident
  modifyGlobal (\g -> g {
      deadStores = deadStores g ++ [(ident, loc) | loc <- S.toList killed]
    })
  modifyPath (\p -> p { unread = M.delete ident (unread p) })

-- | Assignment of a whole object: it kills whatever reached this point.
markAssignment :: Identifier -> Location -> VarUsageMonad ()
markAssignment ident loc = do
  killAssignments ident
  modifyPath (\p -> p { unread = M.insert ident (S.singleton loc) (unread p) })

checkRead :: Identifier -> Location -> VarUsageMonad ()
checkRead ident loc = do
  notAssigned <- pending <$> getPath
  when (S.member ident notAssigned)
    (throwError $ annotateError loc (EReadBeforeAssignment ident))
  markRead ident

-- | A write to a field or to an element only makes sense once the whole object
-- has a value.
checkPartialWrite :: Object SemanticAnn -> Location -> VarUsageMonad ()
checkPartialWrite obj loc = do
  let ident = rootIdent obj
  notAssigned <- pending <$> getPath
  when (S.member ident notAssigned)
    (throwError $ annotateError loc (EPartialWriteBeforeAssignment ident))

-- | Reading an object reads its root variable, every field along the way, which
-- is what the unused-field check needs, and the expressions that index it.
readObject :: Object SemanticAnn -> VarUsageMonad ()
readObject = walkObject ObjectVisitor
  {
    atRoot = \ident ann -> checkRead ident (getLocation ann)
  , atField = \obj ident -> markRead (fieldKey obj ident)
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
      , atField = \inner ident -> markRead (fieldKey inner ident)
      , atIndex = const (return ())
      }

-- | The index expressions of an object that is being written into. The object
-- itself is not read, only the indices are.
readIndices :: Object SemanticAnn -> VarUsageMonad ()
readIndices = walkObject ObjectVisitor
  {
    atRoot = \_ _ -> return ()
  , atField = \_ _ -> return ()
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
  modifyGlobal (\g -> g {
      initializers = S.insert (getLocation ann) (initializers g)
    })
checkStatement (AssignmentStmt obj e ann) = do
  readExpression e
  case obj of
    -- | The whole object is assigned
    Variable ident _ -> markAssigned ident >> markAssignment ident (getLocation ann)
    -- | Only a part of it is
    _ -> checkPartialWrite obj (getLocation ann) >> markWrittenObject obj >> readIndices obj
checkStatement (SingleExpStmt e _) = readExpression e

-- | What each node means to this pass. The two refinements are empty because
-- reading a condition teaches it nothing about the paths it guards.
transfer :: Transfer VarUsagePath VarUsageGlobal VarUsageError
transfer = Transfer
  {
    onStatement = checkStatement
    -- | Every other block only evaluates the expressions it holds, in order.
  , onSimpleBlock = \block -> mapM_ (mapM_ readChild) (simpleBlockChildren block)
  , onExpression = readExpression
  , onCondition = readExpression
    -- | The variables a case binds are declared by the case itself.
  , onCaseEntry = \(MatchCase _ bvars _ ann) ->
      mapM_ (`markInitialized` getLocation ann) bvars
  , refineTrue = const (return ())
  , refineFalse = const (return ())
    -- | A candidate found in one turn is rescued by a later one, so the walk
    -- of the body needs nothing around it.
  , onLoopBody = fixpoint
  }

checkBlock :: Block SemanticAnn -> VarUsageMonad ()
checkBlock = walkForward transfer

-- | An identifier that is declared and never read is dead, unless its name
-- starts with an underscore, in which case it is the other way round.
checkDeclaredAreRead :: VarUsageMonad ()
checkDeclaredAreRead = do
  global <- getGlobal
  mapM_ (check (readIdents global)) (declared global)

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
  idents <- M.keys . unread <$> getPath
  mapM_ killAssignments idents
  global <- getGlobal
  mapM_ (report (initializers global))
    (filter (not . (`S.member` readDefs global) . snd) (deadStores global))

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
  putPath (VarUsagePath S.empty M.empty)
  modifyGlobal (\g -> g {
      declared = [], readDefs = S.empty, deadStores = [],
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
  modifyGlobal (\g -> g { readIdents = S.delete "self" (readIdents g) })
  body
  wasRead <- S.member "self" . readIdents <$> getGlobal
  unless wasRead (throwError $ annotateError loc err)

-- | A field is used when any member of the class reads it. Sink and in ports
-- are driven by the runtime, so they are not read by anybody.
checkFieldIsRead :: ClassMember SemanticAnn -> VarUsageMonad ()
checkFieldIsRead (ClassField (FieldDefinition _ (TSinkPort {}) _)) = return ()
checkFieldIsRead (ClassField (FieldDefinition _ (TInPort {}) _)) = return ()
checkFieldIsRead (ClassField fdef) = do
  wasRead <- S.member (selfFieldKey (fieldIdentifier fdef)) . readIdents <$> getGlobal
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
  wasCalled <- S.member (memberFunctionKey ident) . readIdents <$> getGlobal
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
runVarUsageElement :: AnnASTElement SemanticAnn -> Maybe VarUsageError
runVarUsageElement =
  either Just (const Nothing) . fst . runDataflow emptySt . checkElement

-- | Run the check over a whole module, returning the first error.
runVarUsageCheck :: AnnotatedProgram SemanticAnn -> Maybe VarUsageError
runVarUsageCheck = listToMaybe . mapMaybe runVarUsageElement
