-- | Simple POC of data flo analysis to compute flow of box variables.

module ControlFlow.BoxUsage (
  runBoxUsageCheck
) where

{--
At Termina level, each block is a basic block.

In this module, we implement a backward analysis.
That is, given a block, i.e. a sequence of statements, we go to the last
statement and build our sets and maps backwards.
--}

import ControlFlow.BoxUsage.Monad
import ControlFlow.BoxUsage.Errors

import Utils.Annotations

import Control.Monad
import Control.Monad.Except

import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Control.Monad.State as ST

-- AST to work with.
import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
-- We need to know the type of objects.
import Semantic.Types
import ControlFlow.BoxUsage.Types
import Data.Bifunctor
import qualified Data.Set as S

-- There are two types of arguments :
-- + Moving out variables of type box T and TOption<box T>
-- + Copying expressions, everything.
useArguments :: Expression SemanticAnn -> BoxUsageM BoxUsageError ()
-- If we are giving a variable of type box T, we moving it out.
useArguments e@(AccessObject (Variable ident ann))
  = case getTypeSemAnn ann of
    Just (TBoxSubtype _) ->
      let loc = getLocation ann in
      safeMoveBox ident loc
    _ -> useExpression e
useArguments (ReferenceExpression _ (Variable ident _ann) _a) =
  safeUseVariable ident
-- Box variables inside expressions are read as values.
useArguments e = useExpression e

useObject :: Object SemanticAnn -> BoxUsageM BoxUsageError ()
useObject = walkObject ObjectVisitor
  {
    atRoot = \ident ann ->
      let loc = getLocation ann in
      maybe
        (throwError $ annotateError loc EExpectedOptionBoxType)
        (\case {
            TOption (TBoxSubtype _) -> moveOptionBox ident loc >> safeUseVariable ident;
            _ -> safeUseVariable ident
        }) (getTypeSemAnn ann)
    -- A field reached through a reference is recorded, which is what tells
    -- the two option-box errors apart; one reached directly is not.
  , atField = \accessor _obj ident ->
      case accessor of
        ThroughReference -> safeUseVariable ident
        Direct -> return ()
  , atIndex = useExpression
  }

getObjType :: Object SemanticAnn -> BoxUsageM Error (AccessKind, TerminaType SemanticAnn)
getObjType = maybe (throwError EInvalidObjectTypeAnnotation) return . getObjectSAnns . getAnnotation

useExpression :: Expression SemanticAnn -> BoxUsageM BoxUsageError ()
useExpression = mapM_ useChild . expressionChildren

-- | A reference to a bare variable only uses it, without going through the
-- option-box protocol, and a constant expression that belongs to a type is not
-- part of the computation.
useChild :: Child SemanticAnn -> BoxUsageM BoxUsageError ()
useChild (ChildObject obj) = useObject obj
useChild (ChildReference _ (Variable ident _)) = safeUseVariable ident
useChild (ChildReference _ obj) = useObject obj
useChild (ChildExpr e) = useExpression e
useChild (ChildArg e) = useArguments e
useChild (ChildConstExpr _) = return ()

checkBlock :: Block SemanticAnn -> BoxUsageM BoxUsageError ()
checkBlock bret = checkBasicBlocks (blockBody bret)

checkStatement :: Statement SemanticAnn -> BoxUsageM BoxUsageError ()
checkStatement (Declaration ident _accK tyS initE ann)
  -- variable def is defined
  = let loc = getLocation ann in
  case tyS of
    -- Box are only declared on match statements
    TOption (TBoxSubtype _) -> defVariableOptionBox ident loc
    -- Box are not possible, they come from somewhere else.
    TBoxSubtype _ -> throwError $ annotateError loc EDefiningBox
    -- | Everything else is a plain variable, which the forward pass owns
    _        -> return ()
  -- Use everithing in the |initE| if included
  >> mapM_ useExpression initE
-- All branches should have the same used Only ones.
checkStatement (AssignmentStmt obj e ann) = do
  -- | We need to check if the object is an option-box
  obj_ty <- withLocation (getLocation ann) (getObjType obj)
  case obj_ty of
    (_, TOption (TBoxSubtype _)) -> 
      -- | We are assigning to an option-box. This can only be done through a
      -- MonadicVariantInitializer.
      case e of 
        MonadicVariantInitializer None _ -> 
          case obj of
            Variable ident _ -> initializeOptionBox ident (getLocation ann)
            _ -> throwError $ annotateError (getLocation ann) EBadOptionBoxAssignExpression
        MonadicVariantInitializer (Some boxObjExpr) _ -> do
          -- | We need to move the box object 
          case boxObjExpr of
            AccessObject (Variable ident _) -> 
              let loc = getLocation ann in
              safeMoveBox ident loc
            _ -> throwError $ annotateError (getLocation ann) EBadOptionBoxAssignExpression
          -- | And update the option-box as allocated
          case obj of
            Variable ident _ -> allocOptionBox ident (getLocation ann)
            _ -> throwError $ annotateError (getLocation ann) EBadOptionBoxAssignExpression
        _ -> throwError $ annotateError (getLocation ann) EBadOptionBoxAssignExpression
    _ -> case obj of
      Variable ident _ -> safeUseVariable ident >> useExpression e
      _ -> useObject obj >> useExpression e
checkStatement (SingleExpStmt e _ann)
  = useExpression e

checkBasicBlocks :: [BasicBlock SemanticAnn] -> BoxUsageM BoxUsageError ()
checkBasicBlocks = mapM_ checkBasicBlock . reverse

checkStatements :: [Statement SemanticAnn] -> BoxUsageM BoxUsageError ()
checkStatements = mapM_ checkStatement . reverse

checkBasicBlock :: BasicBlock SemanticAnn -> BoxUsageM BoxUsageError ()
checkBasicBlock (IfElseBlock condIf elseIfs bFalse _ann)
  = do
  let blocks = condIfBody condIf : map condElseIfBody elseIfs ++ (condElseBody <$> maybeToList bFalse)
      bodiesWithLocs = map (\b -> (blockBody b, getLocation (blockAnnotation b))) blocks
  prevSt <- ST.get
  -- All sets generated for all different branches.
  sets <- mapM (\(body, loc) -> do
    blockSt <- runEncapsWithEmptyVars (checkBasicBlocks body >> ST.get)
    return (blockSt, loc)) bodiesWithLocs
   -- Rule here is, when entering, the state of all the boxes must be the same and the
   -- set of used boxes must be equal.
  finalState <- checkUseVariableStates (prevSt {usedVarSet = S.empty}) sets
  unifyState (optionBoxesMap finalState, movedBoxes finalState, S.union (usedVarSet prevSt) (usedVarSet finalState))
   -- Use the else-ifs conditional expressions
  mapM_ (useExpression . condElseIfCond) elseIfs
  -- Finally, use the if conditional expression
  useExpression (condIfCond condIf)
checkBasicBlock (ForLoopBlock  _itIdent _itTy eB eE mBrk block ann) = do
    prevSt <- ST.get
    -- What happens inside the body of a for, may not happen at all.
    loopSt <- runEncapsWithEmptyVars (checkBasicBlocks (blockBody block) >> ST.get)
    finalState <- checkUseVariableStates (prevSt {usedVarSet = S.empty}) [(loopSt, getLocation ann)]
    unifyState (optionBoxesMap finalState, movedBoxes finalState, S.union (usedVarSet prevSt) (usedVarSet finalState))
    mapM_ useExpression mBrk
    -- Use the expressions of the for loop bounds, just in case they contain
    -- references to const input parameters.
    useExpression eB
    useExpression eE
checkBasicBlock (MatchBlock e mcase mDefaultCase ann) = do
  prevSt <- ST.get
  caseSets <- maybe (throwError $ annotateError (getLocation ann) EInvalidExprTypeAnnotation)
    (\case
        TOption (TBoxSubtype _) ->
            case mcase of
              [ml,mr] -> do
                let (mSome, mNone) = if matchIdentifier ml == "Some" then (ml,mr) else (mr,ml)
                someBlk <- runEncapsWithEmptyVars (checkBasicBlocks (blockBody . matchBody $ mSome)
                  >> defBox (head (matchBVars mSome)) (getLocation (matchAnnotation mSome)) >> ST.get)
                noneBlk <- runEncapsWithEmptyVars (checkBasicBlocks (blockBody . matchBody $ mNone) >> ST.get)
                return [(someBlk, getLocation . matchAnnotation $ mSome), (noneBlk, getLocation . matchAnnotation $ mNone)]
              [mSome@(MatchCase "Some" _ _ _)] -> do
                someBlk <- runEncapsWithEmptyVars (checkBasicBlocks (blockBody . matchBody $ mSome)
                  >> defBox (head (matchBVars mSome)) (getLocation (matchAnnotation mSome)) >> ST.get)
                return [(someBlk, getLocation . matchAnnotation $ mSome)]
              [MatchCase "None" _ _ _] -> 
                throwError $ annotateError (getLocation ann) EOptionBoxMatchMissingSomeCase
              _ -> throwError $ annotateError Internal EMalformedOptionBoxMatch;
        -- Otherwise, it is a simple use variable.
        _ -> runMultipleEncapsWithEmptyVars (
          map (\c -> do
            blockSt <- checkMatchCase c >> ST.get
            return (blockSt, getLocation . matchAnnotation $ c)) mcase);
    ) (getResultingType $ getSemanticAnn $ getAnnotation e)
  sets <- case mDefaultCase of
    Just (DefaultCase blk ann') -> do
      defaultBlk <- runEncapsWithEmptyVars (checkBasicBlocks (blockBody blk) >> ST.get)
      return $ (defaultBlk, getLocation ann') : caseSets
    Nothing -> return caseSets
  finalState <- checkUseVariableStates (prevSt {usedVarSet = S.empty}) sets
  unifyState (optionBoxesMap finalState, movedBoxes finalState, S.union (usedVarSet prevSt) (usedVarSet finalState))
  useExpression e
checkBasicBlock (SendMessage obj arg ann) = useObject obj >>
  case arg of
    AccessObject input_obj@(Variable var _) -> do
      input_obj_type <- withLocation (getLocation ann) (getObjType input_obj)
      case input_obj_type of
        (_, TBoxSubtype _) -> let loc = getLocation ann in
          safeMoveBox var loc
        _ -> useObject input_obj
    _ -> useExpression arg
checkBasicBlock (AllocBox obj arg ann) = useObject obj >>
  case arg of
    -- I don't think we can have expression computing variables here.
    ReferenceExpression Mutable (Variable avar _anni) _ann ->
      allocOptionBox avar (getLocation ann)
    AccessObject (Variable avar _anni) ->
      allocOptionBox avar (getLocation ann)
    _ -> throwError $ annotateError (getLocation ann) EBadAllocArg
checkBasicBlock (FreeBox obj arg ann)
  = useObject obj >>
  case arg of
    AccessObject (Variable var _anni) ->
      let loc = getLocation ann in
      safeMoveBox var loc
    _ -> withLocation (getLocation ann) (throwError EBadFreeArg)
checkBasicBlock (RegularBlock stmts) = checkStatements stmts
-- | Every other block only evaluates the expressions it holds, and none of them
-- hands over a box other than through a call argument.
checkBasicBlock block = mapM_ (mapM_ useChild) (simpleBlockChildren block)

-- General case, not when it is TOption Box
checkMatchCase :: MatchCase SemanticAnn -> BoxUsageM BoxUsageError ()
checkMatchCase (MatchCase _mIdent _bvars blk _ann)
  = checkBasicBlocks (blockBody blk)

useArraySize :: TerminaType SemanticAnn -> BoxUsageM BoxUsageError ()
useArraySize (TReference _ (TArray ty size)) = do
  useArraySize ty
  useExpression size
useArraySize (TArray ty size) = do
  useArraySize ty
  useExpression size
useArraySize _ty = return ()

checkUseVariableStates :: BoxUsageSt -> [(BoxUsageSt, Location)] -> BoxUsageM BoxUsageError BoxUsageSt
checkUseVariableStates prevSt sets = do
  finalSt <- checkOptionBoxStates prevSt sets 
  checkSameMovedBoxes (map (first (flip M.difference (movedBoxes prevSt) . movedBoxes)) sets)
  return finalSt

checkSameMovedBoxes :: [(VarMap, Location)] -> BoxUsageM BoxUsageError ()
checkSameMovedBoxes [] = return ()
checkSameMovedBoxes [(boxes, _)] = 
  case M.toList boxes of
    [] -> return ()
    ((ident, loc):_) -> throwError $ annotateError loc (EBoxMoveConditionalBranch ident)
checkSameMovedBoxes (x:xs) = mapM_ (sameMovedBoxes x) xs

  where

    sameMovedBoxes :: (VarMap, Location) -> (VarMap, Location) -> BoxUsageM BoxUsageError ()
    sameMovedBoxes (lmap, lloc) (rmap, rloc) = do
      mapM_ (\k ->
        case (M.lookup k lmap, M.lookup k rmap) of
          (Nothing, Nothing) -> throwError $ annotateError Internal EUnboxingVariableMap
          (Nothing, Just vloc)  -> throwError $ annotateError lloc (EMissingBoxMove k vloc)
          (Just vloc, Nothing)  -> throwError $ annotateError rloc (EMissingBoxMove k vloc)
          _ -> return ()) (M.keys $ M.union lmap rmap)

checkOptionBoxStates :: BoxUsageSt -> [(BoxUsageSt, Location)] -> BoxUsageM BoxUsageError BoxUsageSt
checkOptionBoxStates prevSt [] = return prevSt
checkOptionBoxStates prevSt [(state, loc)] = do
  let lmap = optionBoxesMap prevSt
      rmap = optionBoxesMap state
  mapM_ (\k ->
    case (M.lookup k lmap, M.lookup k rmap) of
      (Nothing, Nothing) -> throwError $ annotateError Internal EUnboxingOptionMap
      (Nothing, Just rval) -> 
        unless (isAllocated rval) $ throwError $ annotateError (getLocation rval) (EDifferentNewOptionBoxUse k rval)
      (Just lval, Nothing) -> 
        unless (isAllocated lval) $ throwError $ annotateError (getLocation lval) (EDifferentNewOptionBoxUse k lval)
      (Just lval, Just rval) -> 
        unless (sameState lval rval) (
          case (S.member k (usedVarSet prevSt), S.member k (usedVarSet state)) of
            (False, True) -> throwError $ annotateError (getLocation rval) (EDifferentNewOptionBoxUse k rval)
            (True, False) -> throwError $ annotateError loc (EMissingOptionBox k lval)
            _ -> throwError $ annotateError (getLocation lval) (EDifferentOptionBoxUse k lval (rval, loc))))
        (M.keys $ M.union lmap rmap)
  unifyStates prevSt state
checkOptionBoxStates lSt ((rSt, rloc):xs) = do
  let lmap = optionBoxesMap lSt
      rmap = optionBoxesMap rSt
  mapM_ (\k ->
    case (M.lookup k lmap, M.lookup k rmap) of
      (Nothing, Nothing) -> throwError $ annotateError Internal EUnboxingOptionMap
      (Nothing, Just _) -> 
        -- | If the option-box is not in the previous state, it means that
        -- it was not used after the branches and it was firstly "mentioned"
        -- in the current one.  However, since there are going to be more
        -- branches, then we will check later on if it is used correctly or
        -- not.
        return ()
      (Just lval, Nothing) -> 
        unless (isAllocated lval) $ throwError $ annotateError (getLocation lval) (EDifferentNewOptionBoxUse k lval)
      (Just lval, Just rval) -> 
        unless (sameState lval rval) (
          case (S.member k (usedVarSet lSt), S.member k (usedVarSet rSt)) of
            (False, True) -> return ()
            (True, False) -> throwError $ annotateError rloc (EMissingOptionBox k lval)
            _ -> throwError $ annotateError (getLocation lval) (EDifferentOptionBoxUse k lval (rval, rloc))))
        (M.keys $ M.union lmap rmap)
  nextSt <- unifyStates lSt rSt
  checkOptionBoxStates nextSt xs

checkClassMember :: ClassMember SemanticAnn -> BoxUsageM BoxUsageError ()
checkClassMember (ClassField {}) = return ()
checkClassMember (ClassMethod _ak _ident ps _tyret bret _ann)
  = checkBlock bret
  >> mapM_ (useArraySize . paramType) ps
checkClassMember (ClassProcedure _ak _ident ps blk ann)
  = checkBlock blk
  >> mapM_ (useArraySize . paramType) ps
  >> mapM_ (`defArgumentsProc` getLocation ann) ps
checkClassMember (ClassViewer _ident ps _tyret bret _ann)
  = checkBlock bret
  >> mapM_ (useArraySize . paramType) ps
checkClassMember (ClassAction _ak _ident Nothing _tyret bret _ann)
  = checkBlock bret
checkClassMember (ClassAction _ak _ident (Just p) _tyret bret ann)
  = checkBlock bret
  >> mapM_ (`defArgumentsProc` getLocation ann) [p]

checkTypeDef :: TypeDef SemanticAnn -> BoxUsageM BoxUsageError ()
checkTypeDef (Class _k _id members _provides _mods)
  = mapM_ checkClassMember members
checkTypeDef (Struct {}) = return ()
checkTypeDef (Interface {}) = return ()
checkTypeDef (Enum {}) = return ()

-- Globals
checkElement :: AnnASTElement SemanticAnn -> BoxUsageM BoxUsageError ()
checkElement (Function _ident ps _ty blk _mods anns)
 = checkBlock blk
 >> mapM_ (useArraySize . paramType) ps
 >> mapM_ (`defArgumentsProc` getLocation anns) ps
 -- >> mapM_ ((annotateError (location anns)) . defVariable . paramIdentifier) ps
checkElement (GlobalDeclaration {})
  = return ()
checkElement (TypeDefinition tyDef _ann)
  = checkTypeDef tyDef

runBoxUsageElement :: AnnASTElement SemanticAnn -> Maybe BoxUsageError
runBoxUsageElement =
  either Just (const Nothing)
  . fst
  . runBoxUsage
  . checkElement

runBoxUsageCheck :: AnnotatedProgram  SemanticAnn -> Maybe BoxUsageError
runBoxUsageCheck
  = safeHead
  . filter isJust
  . map runBoxUsageElement
  where
    safeHead []     = Nothing
    safeHead (x:_) = x
