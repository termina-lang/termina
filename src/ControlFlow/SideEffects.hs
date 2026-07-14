module ControlFlow.SideEffects where

import ControlFlow.SideEffects.Monad
  (SideEffectsMonad, insertMutableReference, resetMutableReferences, runSideEffects,
   setMutableSelfMethods, isMutableSelfMethod, getMutableSelfMethods)
import ControlFlow.SideEffects.Errors (SideEffectsError, Error(..))
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Semantic.Utils (objectPath, mayAlias, AccessPath)
import Utils.Annotations (Location, getLocation, getAnnotation, annotateError)
import Configuration.Platform (Platform)
import Control.Monad (when, forM_)
import Control.Monad.Except (throwError)
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

-- | The index expressions embedded in an object's access path (the @i@ in
-- @arr[i]@), gathered along the whole path.
indexExpressions :: Object SemanticAnn -> [Expression SemanticAnn]
indexExpressions obj = case obj of
  ArrayIndexExpression o idx _  -> idx : indexExpressions o
  MemberAccess o _ _            -> indexExpressions o
  DereferenceMemberAccess o _ _ -> indexExpressions o
  Dereference o _               -> indexExpressions o
  Unbox o _                     -> indexExpressions o
  Variable {}                   -> []

-- | The immediate sub-expressions of an expression, including any index
-- expressions embedded in the objects it accesses or references.
childExpressions :: Expression SemanticAnn -> [Expression SemanticAnn]
childExpressions e = case e of
  BinOp _ left right _               -> [left, right]
  Casting inner _ _                  -> [inner]
  FunctionCall _ args _              -> args
  MemberFunctionCall _ _ args _      -> args
  DerefMemberFunctionCall _ _ args _ -> args
  AccessObject obj                   -> indexExpressions obj
  ReferenceExpression _ obj _        -> indexExpressions obj
  ArraySliceExpression _ obj lower upper _ -> lower : upper : indexExpressions obj
  ArrayInitializer inner size _      -> [inner, size]
  ArrayExprListInitializer es _      -> es
  StructInitializer fields _         -> concatMap fieldExprs fields
  EnumVariantInitializer _ _ args _  -> args
  MonadicVariantInitializer mv _     -> variantExprs mv
  _                                  -> []

  where

    fieldExprs :: FieldAssignment' Expression SemanticAnn -> [Expression SemanticAnn]
    fieldExprs (FieldValueAssignment _ ex _)   = [ex]
    fieldExprs (FieldAddressAssignment _ ex _) = [ex]
    fieldExprs FieldPortConnection {}          = []

    variantExprs :: MonadicVariant' Expression SemanticAnn -> [Expression SemanticAnn]
    variantExprs (Some ex)    = [ex]
    variantExprs (Ok ex)      = [ex]
    variantExprs (Error ex)   = [ex]
    variantExprs (Failure ex) = [ex]
    variantExprs None         = []
    variantExprs Success      = []

-- | Whether an expression subtree contains a persistent side effect, i.e. a
-- call that mutates through a @&mut@ argument anywhere inside it.
hasEffect :: Expression SemanticAnn -> Bool
hasEffect e = not (null (callMutations e)) || any hasEffect (childExpressions e)

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

checkSideEffExpression :: Expression SemanticAnn -> SideEffectsMonad ()
checkSideEffExpression expr = case expr of
  ReferenceExpression Mutable obj ann -> do
    insertMutableReference (objectPath obj) (getLocation ann)
    mapM_ checkSideEffExpression (indexExpressions obj)
  ReferenceExpression _ obj _ -> mapM_ checkSideEffExpression (indexExpressions obj)
  ArraySliceExpression Mutable obj lower upper ann -> do
    insertMutableReference (objectPath obj) (getLocation ann)
    mapM_ checkSideEffExpression (lower : upper : indexExpressions obj)
  ArraySliceExpression _ obj lower upper _ ->
    mapM_ checkSideEffExpression (lower : upper : indexExpressions obj)
  BinOp _ left right _ -> do
    checkSideEffExpression left
    checkSideEffExpression right
  Casting e _ _ -> checkSideEffExpression e
  FunctionCall _ args _ -> mapM_ checkSideEffExpression args
  MemberFunctionCall obj method args _ -> do
    registerMutableSelfReceiver method obj
    mapM_ checkSideEffExpression (args ++ indexExpressions obj)
  DerefMemberFunctionCall obj method args _ -> do
    registerMutableSelfReceiver method obj
    mapM_ checkSideEffExpression (args ++ indexExpressions obj)
  ArrayInitializer e size _ -> do
    checkSideEffExpression e
    checkSideEffExpression size
  ArrayExprListInitializer es _ -> mapM_ checkSideEffExpression es
  StructInitializer fields _ -> mapM_ checkFieldAssignment fields
  EnumVariantInitializer _ _ args _ -> mapM_ checkSideEffExpression args
  MonadicVariantInitializer mv _ -> checkMonadicVariant mv
  AccessObject obj -> mapM_ checkSideEffExpression (indexExpressions obj)
  Constant _ _ -> return ()
  StringInitializer _ _ -> return ()
  IsEnumVariantExpression {} -> return ()
  IsMonadicVariantExpression {} -> return ()

  where

    checkFieldAssignment :: FieldAssignment' Expression SemanticAnn -> SideEffectsMonad ()
    checkFieldAssignment (FieldValueAssignment _ e _) = checkSideEffExpression e
    checkFieldAssignment (FieldAddressAssignment _ e _) = checkSideEffExpression e
    checkFieldAssignment FieldPortConnection {} = return ()

    checkMonadicVariant :: MonadicVariant' Expression SemanticAnn -> SideEffectsMonad ()
    checkMonadicVariant (Some e) = checkSideEffExpression e
    checkMonadicVariant (Ok e) = checkSideEffExpression e
    checkMonadicVariant (Error e) = checkSideEffExpression e
    checkMonadicVariant (Failure e) = checkSideEffExpression e
    checkMonadicVariant None = return ()
    checkMonadicVariant Success = return ()

-- | Among a group of sibling subexpressions (evaluated in unspecified order),
-- at most one may contain a persistent side effect.
atMostOneEffect :: [Expression SemanticAnn] -> SideEffectsMonad ()
atMostOneEffect siblings =
  case filter hasEffect siblings of
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

    -- | Reject a persistent side effect in a forbidden position.
    rejectIfEffect :: Error -> Expression SemanticAnn -> SideEffectsMonad ()
    rejectIfEffect err el =
      when (hasEffect el) $
        throwError $ annotateError (getLocation (getAnnotation el)) err

    -- | The right operand of && / || may be skipped by short-circuit
    -- evaluation, so it must carry no side effect; the left is always evaluated.
    checkLogical :: Error -> Expression SemanticAnn -> Expression SemanticAnn -> SideEffectsMonad ()
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
  checkSideEffExpression e
  checkEffectOrdering e

-- | Several expressions that together make up one full expression (e.g. the
-- argument list of a call): they share one aliasing map, and form one sibling
-- group for the evaluation-order check.
checkFullExpressions :: [Expression SemanticAnn] -> SideEffectsMonad ()
checkFullExpressions es = do
  resetMutableReferences
  mapM_ checkSideEffExpression es
  atMostOneEffect es
  checkInterference es
  mapM_ checkEffectOrdering es

checkSideEffStatement :: Statement SemanticAnn -> SideEffectsMonad ()
checkSideEffStatement stmt = case stmt of
  Declaration _ _ _ initExpr _ -> checkFullExpression initExpr
  AssignmentStmt _ rhs _       -> checkFullExpression rhs
  SingleExpStmt e _            -> checkFullExpression e

checkSideEffBlock :: Block SemanticAnn -> SideEffectsMonad ()
checkSideEffBlock = mapM_ checkSideEffBasicBlock . blockBody

checkSideEffBasicBlock :: BasicBlock SemanticAnn -> SideEffectsMonad ()
checkSideEffBasicBlock bb = case bb of
  RegularBlock stmts -> mapM_ checkSideEffStatement stmts
  IfElseBlock condIf elseIfs mElse _ -> do
    checkFullExpression (condIfCond condIf)
    checkSideEffBlock (condIfBody condIf)
    mapM_ (\ei -> checkFullExpression (condElseIfCond ei) >> checkSideEffBlock (condElseIfBody ei)) elseIfs
    mapM_ (\(CondElse blk _) -> checkSideEffBlock blk) mElse
  ForLoopBlock _ _ initV endV mBreak body _ -> do
    checkFullExpression initV
    checkFullExpression endV
    mapM_ checkFullExpression mBreak
    checkSideEffBlock body
  MatchBlock subject cases mDefault _ -> do
    checkFullExpression subject
    mapM_ (checkSideEffBlock . matchBody) cases
    mapM_ (\(DefaultCase blk _) -> checkSideEffBlock blk) mDefault
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

checkSideEffClassMember :: ClassMember SemanticAnn -> SideEffectsMonad ()
checkSideEffClassMember (ClassMethod _ak _ident _ps _tyret body _ann)  = checkSideEffBlock body
checkSideEffClassMember (ClassProcedure _ak _ident _ps body _ann)      = checkSideEffBlock body
checkSideEffClassMember (ClassViewer _ident _ps _tyret body _ann)      = checkSideEffBlock body
checkSideEffClassMember (ClassAction _ak _ident _mp _tyret body _ann)  = checkSideEffBlock body
checkSideEffClassMember (ClassField {})                                = return ()

checkSideEffTypeDef :: TypeDef SemanticAnn -> SideEffectsMonad ()
checkSideEffTypeDef (Class _kind _ident members _provides _mods) = do
    setMutableSelfMethods (mutableSelfMethodNames members)
    mapM_ checkSideEffClassMember members
checkSideEffTypeDef _ = return ()

-- | The names of the methods that take a @&mut self@ receiver, i.e. a mutable
-- reference to the whole state. A @&priv self@ method is not a mutable
-- reference (it cannot even produce one to its fields), and a viewer is always
-- @&self@, so neither qualifies.
mutableSelfMethodNames :: [ClassMember SemanticAnn] -> S.Set Identifier
mutableSelfMethodNames members = S.fromList
    [ ident | ClassMethod Mutable ident _ _ _ _ <- members ]

checkSideEffElement :: AnnASTElement SemanticAnn -> SideEffectsMonad ()
checkSideEffElement (Function _ident _ps _ty body _mods _ann) = checkSideEffBlock body
checkSideEffElement (TypeDefinition tyDef _ann)               = checkSideEffTypeDef tyDef
checkSideEffElement (GlobalDeclaration {})                    = return ()

-- | Run the side-effect check over a single top-level element.
runSideEffectElement :: Platform -> AnnASTElement SemanticAnn -> Maybe SideEffectsError
runSideEffectElement plt =
    either Just (const Nothing) . runSideEffects plt . checkSideEffElement

-- | Run the side-effect check over a whole module, returning the first error.
runSideEffectCheck :: Platform -> AnnotatedProgram SemanticAnn -> Maybe SideEffectsError
runSideEffectCheck plt = listToMaybe . mapMaybe (runSideEffectElement plt)
