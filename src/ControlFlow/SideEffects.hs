module ControlFlow.SideEffects where

import ControlFlow.SideEffects.Monad
  (SideEffectsMonad, SideEffectsEnv, insertMutableReference, resetMutableReferences,
   runSideEffects, isMutableSelfMethod, getMutableSelfMethods, getEffectful,
   startClass, startCallable, noteEffect, endFunction, endMember, effectfulFunctions)
import ControlFlow.SideEffects.Errors (SideEffectsError, Error(..), Effect(..))
import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
  (Child(..), childExpressions, expressionChildren, indexExpressions)
import ControlFlow.Dataflow (Transfer(..), walkForward)
import Semantic.Types
import Semantic.Utils (objectPath, mayAlias, AccessPath)
import Utils.Annotations (Location, getLocation, getAnnotation, annotateError)
import Configuration.Platform (Platform)
import Control.Applicative ((<|>))
import Control.Monad (when, forM_)
import Control.Monad.Except (throwError)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe)

-- | The objects a call mutates: the arguments it passes to its @&mut@
-- parameters (their access paths). A call is effectful exactly when this is
-- non-empty. Only explicit @&mut@ parameters count; a @&mut self@ receiver is
-- not a persistent side effect on a caller object.
callMutations :: Expression SemanticAnn -> [AccessPath]
callMutations e = case e of
  FunctionCall _ args ann              -> mutated ann args
  MemberFunctionCall _ _ args ann      -> mutated ann args
  DerefMemberFunctionCall _ _ args ann -> mutated ann args
  _                                    -> []

  where

    mutated :: SemanticAnn -> [Expression SemanticAnn] -> [AccessPath]
    mutated (SemanticAnn (ETy (AppType ps _)) _) args =
      [ objectPath obj
      | (p, arg) <- zip ps args
      , isMutableRef (paramType p)
      , Just obj <- [referencedObject arg]
      ]
    mutated _ _ = []

    isMutableRef :: TerminaType SemanticAnn -> Bool
    isMutableRef (TReference Mutable _) = True
    isMutableRef _                      = False

    referencedObject :: Expression SemanticAnn -> Maybe (Object SemanticAnn)
    referencedObject (ReferenceExpression Mutable obj _)      = Just obj
    referencedObject (ArraySliceExpression Mutable obj _ _ _) = Just obj
    referencedObject _                                        = Nothing

-- | Whether an expression subtree mutates state anywhere inside it, which is
-- what decides whether the order in which two subexpressions are evaluated can
-- change the result. A read is not a mutation, so an access to a loc field and a
-- bounds check, which 'hasPersistentEffect' does count, are left out here: two
-- of them among the arguments of a call give the same result in any order.
mutatesState :: Expression SemanticAnn -> Bool
mutatesState e = not (null (callMutations e)) || any mutatesState (childExpressions e)

-- | Whether reading an object is itself an effect and not a plain load. Two
-- cases, and in Termina both are written as a read:
--
-- A field declared @loc@ lives at a fixed address, which is storage the
-- program does not own, e.g. a device register. The generated C reaches it
-- through a pointer to @volatile@, so the access is kept where it is written
-- and two reads of it may give different values, which makes where it happens
-- part of the behaviour of the program.
--
-- An array access whose index the compiler does not know lowers to a call to
-- the bounds check, which returns the index when it falls inside the array and
-- raises the exception when it does not. An index that is a constant is
-- checked while the program is compiled and lowers to the index alone, which
-- is why the generator only emits the call for the rest (the same condition it
-- uses to decide, in @genObject@).
objectEffect :: Object SemanticAnn -> Maybe Effect
objectEffect obj = case obj of
  Variable _ ann                   -> location ann
  ArrayIndexExpression o index ann -> location ann <|> checked index ann <|> objectEffect o
  MemberAccess o _ ann             -> location ann <|> objectEffect o
  DereferenceMemberAccess o _ ann  -> location ann <|> objectEffect o
  Dereference o ann                -> location ann <|> objectEffect o
  Unbox o ann                      -> location ann <|> objectEffect o

  where

    location :: SemanticAnn -> Maybe Effect
    location ann = case getTypeSemAnn ann of
      Just (TFixedLocation _) -> Just (ReadsLocation (getLocation ann))
      _                       -> Nothing

    checked :: Expression SemanticAnn -> SemanticAnn -> Maybe Effect
    checked index ann = case getTypeSemAnn (getAnnotation index) of
      Just (TConstSubtype _) -> Nothing
      _                      -> Just (ChecksIndex (getLocation ann))

-- | What the pass knows about the names an expression may call: the methods of
-- the class that take @&mut self@, and the functions and members whose body
-- was already found to carry an effect.
data Callees = Callees
  {
    mutableSelf :: S.Set Identifier
  , effectfulFuns :: M.Map Identifier Effect
  , effectfulMems :: M.Map Identifier Effect
  }

-- | What the pass knows at this point of the walk.
callees :: SideEffectsMonad Callees
callees = do
  ms <- getMutableSelfMethods
  (functions, members) <- getEffectful
  return (Callees ms functions members)

-- | The objects an expression reaches at its own level, either by value or
-- through a reference it takes of them.
immediateObjects :: Expression SemanticAnn -> [Object SemanticAnn]
immediateObjects = mapMaybe pick . expressionChildren

  where

    pick (ChildObject obj)      = Just obj
    pick (ChildReference _ obj) = Just obj
    pick _                      = Nothing

-- | Whether an expression subtree carries a side effect that outlives it: a
-- call that mutates through a @&mut@ argument, an access to a field declared @loc@,
-- or a bounds check that can end in the exception path. It is the notion of
-- the two rules that forbid such an effect in a position where it may or may
-- not happen, the element of an initializer list and the right operand of a
-- logical operator.
persistentEffect :: Callees -> Expression SemanticAnn -> Maybe Effect
persistentEffect known e =
      mutation
  <|> receiverMutation
  <|> reachedEffect
  <|> listToMaybe (mapMaybe objectEffect (immediateObjects e))
  <|> listToMaybe (mapMaybe (persistentEffect known) (childExpressions e))

  where

    mutation = case callMutations e of
      (_ : _) -> Just (MutatesThroughCall (getLocation (getAnnotation e)))
      []      -> Nothing

    -- | A method that takes @&mut self@ writes the state of its class, which
    -- is not an argument of the call and so does not show up in
    -- 'callMutations'.
    receiverMutation = case e of
      MemberFunctionCall _ method _ ann
        | method `S.member` mutableSelf known -> Just (MutatesReceiver (getLocation ann))
      DerefMemberFunctionCall _ method _ ann
        | method `S.member` mutableSelf known -> Just (MutatesReceiver (getLocation ann))
      _ -> Nothing

    -- | A call reaches whatever its callee carries: a method that takes
    -- @&self@ writes nothing, but it may read a loc field or index an
    -- array, and that effect belongs to the expression that calls it.
    reachedEffect = case e of
      FunctionCall name _ ann           -> reaches name ann (effectfulFuns known)
      MemberFunctionCall _ name _ ann   -> reaches name ann (effectfulMems known)
      DerefMemberFunctionCall _ name _ ann -> reaches name ann (effectfulMems known)
      _                                 -> Nothing

    reaches name ann carried =
      CallsEffectful (getLocation ann) name <$> M.lookup name carried

-- | The objects mutated anywhere in a subtree, each paired with the location of
-- the call that mutates it. A mutation is either a @&mut@ argument, or, for a
-- call to a @&mut self@ method, the receiver itself (which is borrowed mutably).
subtreeMutations :: S.Set Identifier -> Expression SemanticAnn -> [(AccessPath, Location)]
subtreeMutations mutableMethods e =
  argMutations ++ receiverMutation ++ concatMap (subtreeMutations mutableMethods) (childExpressions e)

  where

    loc = getLocation (getAnnotation e)

    argMutations = [ (p, loc) | p <- callMutations e ]

    receiverMutation = case e of
      MemberFunctionCall obj method _ _      | method `S.member` mutableMethods -> [(objectPath obj, loc)]
      DerefMemberFunctionCall obj method _ _ | method `S.member` mutableMethods -> [(objectPath obj, loc)]
      _ -> []

-- | The objects read anywhere in a subtree (by value, or through an immutable
-- reference passed to a call), each paired with the location of the access.
subtreeReads :: Expression SemanticAnn -> [(AccessPath, Location)]
subtreeReads e = here ++ concatMap subtreeReads (childExpressions e)
  where
    here = case e of
      AccessObject obj -> [(objectPath obj, getLocation (getAnnotation obj))]
      ReferenceExpression Immutable obj ann -> [(objectPath obj, getLocation ann)]
      _ -> []

-- | If a member call invokes a method that borrows @self@ mutably, register
-- the receiver as a mutable reference: it borrows the whole receiver for the
-- duration of the call, which aliases any @&mut@ argument reaching into it
-- (e.g. @self->update(&mut self->counter)@).
registerMutableSelfReceiver :: Identifier -> Object SemanticAnn -> SideEffectsMonad ()
registerMutableSelfReceiver method obj = do
  mutable <- isMutableSelfMethod method
  when mutable $
    insertMutableReference (objectPath obj) (getLocation (getAnnotation obj))

checkExpression :: Expression SemanticAnn -> SideEffectsMonad ()
checkExpression expr = case expr of
  ReferenceExpression Mutable obj ann -> do
    insertMutableReference (objectPath obj) (getLocation ann)
    mapM_ checkExpression (indexExpressions obj)
  ReferenceExpression _ obj _ -> mapM_ checkExpression (indexExpressions obj)
  ArraySliceExpression Mutable obj lower upper ann -> do
    insertMutableReference (objectPath obj) (getLocation ann)
    mapM_ checkExpression (lower : upper : indexExpressions obj)
  ArraySliceExpression _ obj lower upper _ ->
    mapM_ checkExpression (lower : upper : indexExpressions obj)
  BinOp _ left right _ -> do
    checkExpression left
    checkExpression right
  Casting e _ _ -> checkExpression e
  FunctionCall _ args _ -> mapM_ checkExpression args
  MemberFunctionCall obj method args _ -> do
    registerMutableSelfReceiver method obj
    mapM_ checkExpression (args ++ indexExpressions obj)
  DerefMemberFunctionCall obj method args _ -> do
    registerMutableSelfReceiver method obj
    mapM_ checkExpression (args ++ indexExpressions obj)
  ArrayInitializer e size _ -> do
    checkExpression e
    checkExpression size
  ArrayExprListInitializer es _ -> mapM_ checkExpression es
  StructInitializer fields _ -> mapM_ checkFieldAssignment fields
  EnumVariantInitializer _ _ args _ -> mapM_ checkExpression args
  MonadicVariantInitializer mv _ -> checkMonadicVariant mv
  AccessObject obj -> mapM_ checkExpression (indexExpressions obj)
  Constant _ _ -> return ()
  StringInitializer _ _ -> return ()
  IsEnumVariantExpression {} -> return ()
  IsMonadicVariantExpression {} -> return ()

  where

    checkFieldAssignment :: FieldAssignment' Expression SemanticAnn -> SideEffectsMonad ()
    checkFieldAssignment (FieldValueAssignment _ e _) = checkExpression e
    checkFieldAssignment (FieldAddressAssignment _ e _) = checkExpression e
    checkFieldAssignment FieldPortConnection {} = return ()

    checkMonadicVariant :: MonadicVariant' Expression SemanticAnn -> SideEffectsMonad ()
    checkMonadicVariant (Some e) = checkExpression e
    checkMonadicVariant (Ok e) = checkExpression e
    checkMonadicVariant (Error e) = checkExpression e
    checkMonadicVariant (Failure e) = checkExpression e
    checkMonadicVariant None = return ()
    checkMonadicVariant Success = return ()

-- | Among a group of sibling subexpressions (evaluated in unspecified order),
-- at most one may contain a persistent side effect.
atMostOneEffect :: [Expression SemanticAnn] -> SideEffectsMonad ()
atMostOneEffect siblings =
  case filter mutatesState siblings of
    (firstEff : secondEff : _) ->
      throwError $ annotateError
        (getLocation (getAnnotation secondEff))
        (EMultipleSideEffects (getLocation (getAnnotation firstEff)))
    _ -> return ()

-- | Within a group of sibling subexpressions, an object mutated by one may not
-- be accessed by another: the two are unordered, so the value read would depend
-- on the evaluation order.
checkInterference :: [Expression SemanticAnn] -> SideEffectsMonad ()
checkInterference group = do
  mutableMethods <- getMutableSelfMethods
  forM_ (zip [0 :: Int ..] group) (checkSibling mutableMethods)

  where

    checkSibling :: S.Set Identifier -> (Int, Expression SemanticAnn) -> SideEffectsMonad ()
    checkSibling mutableMethods (i, si) =
      forM_ (subtreeMutations mutableMethods si) $ \(mutPath, mutLoc) ->
        forM_ (readsOfOtherSiblings i) $ \(readPath, readLoc) ->
          when (mayAlias mutPath readPath) $
            throwError $ annotateError readLoc (EInterferingSideEffect mutLoc)

    -- | The reads of every sibling except the i-th.
    readsOfOtherSiblings :: Int -> [(AccessPath, Location)]
    readsOfOtherSiblings i =
      concat [ subtreeReads sj | (j, sj) <- zip [0 :: Int ..] group, j /= i ]

-- | Enforce the evaluation-order rule on an expression: at every group of
-- sibling subexpressions (the arguments of a call, the operands of a binary
-- operator) at most one may carry a side effect.
checkEffectOrdering :: Expression SemanticAnn -> SideEffectsMonad ()
checkEffectOrdering e = case e of
  FunctionCall _ args _              -> checkGroup args
  MemberFunctionCall _ _ args _      -> checkGroup args
  DerefMemberFunctionCall _ _ args _ -> checkGroup args
  EnumVariantInitializer _ _ args _  -> checkGroup args
  BinOp LogicalAnd left right _      -> checkLogical ESideEffectInRHSLogicalAnd left right
  BinOp LogicalOr  left right _      -> checkLogical ESideEffectInRHSLogicalOr left right
  BinOp _ left right _               -> checkGroup [left, right]
  Casting inner _ _                  -> checkEffectOrdering inner
  ArraySliceExpression _ _ lower upper _ -> mapM_ checkEffectOrdering [lower, upper]
  ArrayInitializer inner size _      -> mapM_ checkInitElem [inner, size]
  ArrayExprListInitializer es _      -> mapM_ checkInitElem es
  StructInitializer fields _         -> mapM_ checkInitField fields
  MonadicVariantInitializer mv _     -> checkVariantOrdering mv
  _                                  -> return ()

  where

    checkGroup :: [Expression SemanticAnn] -> SideEffectsMonad ()
    checkGroup group = do
      atMostOneEffect group
      checkInterference group
      mapM_ checkEffectOrdering group

    -- | Reject an effect that outlives the expression in a position where it
    -- may or may not happen, naming what the effect is.
    rejectIfEffect :: (Effect -> Error) -> Expression SemanticAnn -> SideEffectsMonad ()
    rejectIfEffect err el = do
      known <- callees
      forM_ (persistentEffect known el) $ \effect ->
        throwError $ annotateError (getLocation (getAnnotation el)) (err effect)

    -- | The right operand of && / || may be skipped by short-circuit
    -- evaluation, so it must carry no side effect; the left is always evaluated.
    checkLogical :: (Effect -> Error) -> Expression SemanticAnn -> Expression SemanticAnn -> SideEffectsMonad ()
    checkLogical err left right = do
      rejectIfEffect err right
      checkEffectOrdering left
      checkEffectOrdering right

    -- | An initializer-list element must carry no side effect at all.
    checkInitElem :: Expression SemanticAnn -> SideEffectsMonad ()
    checkInitElem el = rejectIfEffect ESideEffectInInitializerList el >> checkEffectOrdering el

    checkInitField :: FieldAssignment' Expression SemanticAnn -> SideEffectsMonad ()
    checkInitField (FieldValueAssignment _ el _)   = checkInitElem el
    checkInitField (FieldAddressAssignment _ el _) = checkInitElem el
    checkInitField FieldPortConnection {}          = return ()

    checkVariantOrdering :: MonadicVariant' Expression SemanticAnn -> SideEffectsMonad ()
    checkVariantOrdering (Some inner)    = checkEffectOrdering inner
    checkVariantOrdering (Ok inner)      = checkEffectOrdering inner
    checkVariantOrdering (Error inner)   = checkEffectOrdering inner
    checkVariantOrdering (Failure inner) = checkEffectOrdering inner
    checkVariantOrdering None            = return ()
    checkVariantOrdering Success         = return ()

-- | Check a single full expression: the per-expression aliasing walk (with its
-- reset) plus the evaluation-order check.
checkFullExpression :: Expression SemanticAnn -> SideEffectsMonad ()
checkFullExpression e = do
  resetMutableReferences
  checkExpression e
  checkEffectOrdering e
  noteEffectOf e

-- | The guard of a loop sits where the right operand of an && sits: the
-- generated @for@ joins the range of the iterator and the guard with an &&, so
-- an effect written in the guard happens or not depending on the iterator,
-- which is what Rule 13.5 forbids. The user wrote no && here, so the error is
-- its own and names the loop.
checkLoopGuard :: Expression SemanticAnn -> SideEffectsMonad ()
checkLoopGuard e = do
  known <- callees
  forM_ (persistentEffect known e) $ \effect ->
    throwError $ annotateError (getLocation (getAnnotation e)) (ESideEffectInLoopGuard effect)
  checkFullExpression e

-- | Record what the expression carries, which is what the body being checked
-- hands to whoever calls it.
noteEffectOf :: Expression SemanticAnn -> SideEffectsMonad ()
noteEffectOf e = do
  known <- callees
  forM_ (persistentEffect known e) noteEffect

-- | Several expressions that together make up one full expression (e.g. the
-- argument list of a call): they share one aliasing map, and form one sibling
-- group for the evaluation-order check.
checkFullExpressions :: [Expression SemanticAnn] -> SideEffectsMonad ()
checkFullExpressions es = do
  resetMutableReferences
  mapM_ checkExpression es
  atMostOneEffect es
  checkInterference es
  mapM_ checkEffectOrdering es
  mapM_ noteEffectOf es

checkStatement :: Statement SemanticAnn -> SideEffectsMonad ()
checkStatement stmt = case stmt of
  Declaration _ _ _ initExpr _ -> mapM_ checkFullExpression initExpr
  -- | Writing through a field declared loc, or through an index that is checked
  -- while the program runs, is an effect of the body as much as reading one.
  AssignmentStmt lhs rhs _     -> forM_ (objectEffect lhs) noteEffect >> checkFullExpression rhs
  SingleExpStmt e _            -> checkFullExpression e

-- | What each node means to this pass. It learns nothing from a condition and
-- carries nothing from one statement to the next, so the refinements are empty
-- and the state of a path is the unit.
transfer :: Transfer () SideEffectsEnv SideEffectsError
transfer = Transfer
  {
    onStatement = checkStatement
  , onSimpleBlock = checkSimpleBlock
  , onExpression = checkFullExpression
  , onCondition = checkFullExpression
  , onLoopGuard = checkLoopGuard
  , onCaseEntry = \_ _ -> return ()
  , refineTrue = const (return ())
  , refineFalse = const (return ())
    -- | The iterator holds a number, which has no effects to order.
  , onLoopEntry = \_ _ _ _ -> return ()
  }

checkBlock :: Block SemanticAnn -> SideEffectsMonad ()
checkBlock = walkForward transfer

-- | The blocks that only evaluate expressions. The arguments of a call are
-- checked as a group, since the aliasing rule is about what happens between
-- siblings of one expression, which is why this is not read off
-- 'simpleBlockChildren'.
checkSimpleBlock :: BasicBlock SemanticAnn -> SideEffectsMonad ()
checkSimpleBlock bb = case bb of
  SendMessage _ payload _        -> checkFullExpression payload
  ProcedureInvoke _ _ args _     -> checkFullExpressions args
  SystemCall _ _ args _          -> checkFullExpressions args
  AtomicLoad _ dst _             -> checkFullExpression dst
  AtomicStore _ val _            -> checkFullExpression val
  AtomicArrayLoad _ idx dst _    -> checkFullExpressions [idx, dst]
  AtomicArrayStore _ idx val _   -> checkFullExpressions [idx, val]
  AllocBox _ arg _               -> checkFullExpression arg
  FreeBox _ arg _                -> checkFullExpression arg
  ReturnBlock mRet _             -> mapM_ checkFullExpression mRet
  ContinueBlock e _              -> checkFullExpression e
  RebootBlock _                  -> return ()
  -- | The blocks that branch are walked by the shared skeleton.
  RegularBlock {}                -> return ()
  IfElseBlock {}                 -> return ()
  ForLoopBlock {}                -> return ()
  MatchBlock {}                  -> return ()

-- | Checks the body of a member and records what it carries under its name,
-- which is what a later member of the same class gets when it calls it.
checkClassMember :: ClassMember SemanticAnn -> SideEffectsMonad ()
checkClassMember member = case member of
  ClassMethod _ak ident _ps _tyret body _ann -> checkBody ident body
  ClassProcedure _ak ident _ps body _ann     -> checkBody ident body
  ClassViewer ident _ps _tyret body _ann     -> checkBody ident body
  ClassAction _ak ident _mp _tyret body _ann -> checkBody ident body
  ClassField {}                              -> return ()

  where

    checkBody ident body = startCallable >> checkBlock body >> endMember ident

checkTypeDef :: TypeDef SemanticAnn -> SideEffectsMonad ()
checkTypeDef (Class _kind _ident members _provides _mods) = do
    startClass (mutableSelfMethodNames members)
    mapM_ checkClassMember members
checkTypeDef _ = return ()

-- | The names of the methods that take a @&mut self@ receiver, i.e. a mutable
-- reference to the whole state. A @&priv self@ method is not a mutable
-- reference (it cannot even produce one to its fields), and a viewer is always
-- @&self@, so neither qualifies.
mutableSelfMethodNames :: [ClassMember SemanticAnn] -> S.Set Identifier
mutableSelfMethodNames members = S.fromList
    [ ident | ClassMethod Mutable ident _ _ _ _ <- members ]

checkElement :: AnnASTElement SemanticAnn -> SideEffectsMonad ()
checkElement (Function ident _ps _ty body _mods _ann) =
    startCallable >> checkBlock body >> endFunction ident
checkElement (TypeDefinition tyDef _ann)               = checkTypeDef tyDef
checkElement (GlobalDeclaration {})                    = return ()

-- | Run the side-effect check over a whole module, returning the first error
-- and the functions known to carry an effect once it is done. The elements are
-- checked in the order they are written, which is the order in which a module
-- sees its own names, and the modules in dependency order, so what a call
-- reaches is already known by the time the call is checked.
runSideEffectCheck :: Platform -> M.Map Identifier Effect -> AnnotatedProgram SemanticAnn
  -> (Maybe SideEffectsError, M.Map Identifier Effect)
runSideEffectCheck plt functions program =
    case runSideEffects plt functions (mapM_ checkElement program) of
      (Left err, env) -> (Just err, effectfulFunctions env)
      (Right (), env) -> (Nothing, effectfulFunctions env)
