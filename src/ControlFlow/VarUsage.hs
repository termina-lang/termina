-- | Simple POC of data flo analysis to compute flow of box variables.

module ControlFlow.VarUsage (
  runUDAnnotatedProgram
) where

{--
At Termina level, each block is a basic block.

In this module, we implement a backward analysis.
That is, given a block, i.e. a sequence of statements, we go to the last
statement and build our sets and maps backwards.
--}

import ControlFlow.VarUsage.Computation
import ControlFlow.VarUsage.Errors.Errors

import Utils.Annotations

import Control.Monad
import Control.Monad.Except

import Data.Maybe
import qualified Data.Map as M
import qualified Control.Monad.State as ST

-- AST to work with.
import ControlFlow.BasicBlocks.AST
-- We need to know the type of objects.
import qualified Semantic.Monad as SM
import Semantic.Types
import ControlFlow.VarUsage.Types
import Data.Bifunctor
import qualified Data.Set as S


-- There are two types of arguments :
-- + Moving out variables of type box T and TOption<box T>
-- + Copying expressions, everything.
useArguments :: Expression SemanticAnn -> UDM VarUsageError ()
-- If we are giving a variable of type box T, we moving it out.
useArguments e@(AccessObject (Variable ident ann))
  = case SM.getTypeSemAnn ann of
    Just (TBoxSubtype _) ->
      let loc = location ann in
      safeMoveBox ident loc
    _ -> useExpression e
-- Box variables inside expressions are read as values.
useArguments e = useExpression e

useObject :: Object SemanticAnn -> UDM VarUsageError ()
useObject (Variable ident ann)
  =
  let loc = location ann in
  maybe
        (throwError $ annotateError loc EUnboxingObjectType)
        (\case {
            TOption (TBoxSubtype _) -> moveOptionBox ident loc >> safeUseVariable ident;
            _ -> safeUseVariable ident
        }) (SM.getTypeSemAnn ann)
useObject (ArrayIndexExpression obj e _ann)
  = useObject obj >> useExpression e
useObject (MemberAccess obj _i _ann)
  = useObject obj
useObject (Dereference obj _ann)
  = useObject obj
useObject (DereferenceMemberAccess obj i _ann)
  = safeUseVariable i
  >> useObject obj
useObject (Unbox obj _ann)
  = useObject obj

useFieldAssignment :: FieldAssignment SemanticAnn -> UDM VarUsageError ()
useFieldAssignment (FieldValueAssignment _ident e _) = useExpression e
useFieldAssignment _ = return ()

getObjType :: Object SemanticAnn -> UDM Error (AccessKind, TerminaType)
getObjType = maybe (throwError EUnboxingObjectType) return . SM.getObjectSAnns . getAnnotation

useExpression :: Expression SemanticAnn -> UDM VarUsageError ()
useExpression (AccessObject obj)
  = useObject obj
useExpression (Constant _c _a)
  = return ()
useExpression (BinOp _o el er _ann)
  = useExpression el >> useExpression er
useExpression (ReferenceExpression _aK obj _a)
  = useObject obj
useExpression (Casting e _ty _a)
  = useExpression e
useExpression (IsEnumVariantExpression obj _ _ _)
  = useObject obj
useExpression (IsOptionVariantExpression obj _ _)
  = useObject obj
useExpression (ArraySliceExpression _aK obj eB eT _ann)
  = useObject obj >> useExpression eB >> useExpression eT
useExpression (MemberFunctionCall obj _ident args _ann) = do
    useObject obj >> mapM_ useArguments args
useExpression (DerefMemberFunctionCall obj _ident args _ann)
  = useObject obj >> mapM_ useArguments args
useExpression (ArrayInitializer e _size _ann)
  = useExpression e
useExpression (ArrayExprListInitializer exprs _ann) = mapM_ useExpression exprs
useExpression (StructInitializer fs _ident _ann)
  = mapM_ useFieldAssignment fs
useExpression (EnumVariantInitializer _ident _ident2 es _ann)
  = mapM_ useExpression es
useExpression (OptionVariantInitializer opt _ann)
  = case opt of
        None   -> return ()
        Some e -> useExpression e
useExpression (FunctionCall _ident args _ann)
  = mapM_ useArguments args

useDefBlockRet :: Block SemanticAnn -> UDM VarUsageError ()
useDefBlockRet bret = useDefBasicBlocks (blockBody bret)

useDefStmt :: Statement SemanticAnn -> UDM VarUsageError ()
useDefStmt (Declaration ident _accK tyS initE ann)
  -- variable def is defined
  = let loc = location ann in
  case tyS of
    -- Box are only declared on match statements
    TOption (TBoxSubtype _) -> defVariableOptionBox ident loc
    -- Box are not possible, they come from somewhere else.
    TBoxSubtype _ -> throwError $ annotateError loc EDefiningBox
    --Everything else
    _        -> defVariable ident loc
  -- Use everithing in the |initE|
  >> useExpression initE
-- All branches should have the same used Only ones.
useDefStmt (AssignmentStmt obj e ann) = do
  -- | We need to check if the object is an option-box
  obj_ty <- withLocation (location ann) (getObjType obj)
  case obj_ty of
    (_, TOption (TBoxSubtype _)) -> 
      -- | We are assigning to an option-box. This can only be done through a
      -- OptionVariantInitializer.
      case e of 
        OptionVariantInitializer None _ -> 
          case obj of
            Variable ident _ -> initializeOptionBox ident (location ann)
            _ -> throwError $ annotateError (location ann) EBadOptionBoxAssignExpression
        OptionVariantInitializer (Some boxObjExpr) _ -> do
          -- | We need to move the box object 
          case boxObjExpr of
            AccessObject (Variable ident _) -> 
              let loc = location ann in
              safeMoveBox ident loc
            _ -> throwError $ annotateError (location ann) EBadOptionBoxAssignExpression
          -- | And update the option-box as allocated
          case obj of
            Variable ident _ -> allocOptionBox ident (location ann)
            _ -> throwError $ annotateError (location ann) EBadOptionBoxAssignExpression
        _ -> throwError $ annotateError (location ann) EBadOptionBoxAssignExpression
    _ -> useObject obj >> useExpression e
useDefStmt (SingleExpStmt e _ann)
  = useExpression e

useDefBasicBlocks :: [BasicBlock SemanticAnn] -> UDM VarUsageError ()
useDefBasicBlocks = mapM_ useDefBasicBlock . reverse

useDefStatements :: [Statement SemanticAnn] -> UDM VarUsageError ()
useDefStatements = mapM_ useDefStmt . reverse

useDefBasicBlock :: BasicBlock SemanticAnn -> UDM VarUsageError ()
useDefBasicBlock (IfElseBlock eCond bTrue elseIfs bFalse _ann)
  = do
  let blocks = bTrue : map elseIfBody elseIfs ++ maybeToList bFalse
      bodiesWithLocs = map (\b -> (blockBody b, location (blockAnnotation b))) blocks
  prevSt <- ST.get
  -- All sets generated for all different branches.
  sets <- mapM (\(body, loc) -> do
    blockSt <- runEncapsWithEmptyVars (useDefBasicBlocks body >> ST.get)
    return (blockSt, loc)) bodiesWithLocs
   -- Rule here is, when entering, the state of all the boxes must be the same and the
   -- set of used boxes must be equal.
  finalState <- checkUseVariableStates (prevSt {usedVarSet = S.empty}) sets
  unifyState (optionBoxesMap finalState, movedBoxes finalState, S.union (usedVarSet prevSt) (usedVarSet finalState))
   -- Use the else-ifs conditional expressions
  mapM_ (useExpression . elseIfCond) elseIfs
  -- Finally, use the if conditional expression
  useExpression eCond
useDefBasicBlock (ForLoopBlock  _itIdent _itTy _eB _eE mBrk block ann) = do
    prevSt <- ST.get
    -- What happens inside the body of a for, may not happen at all.
    loopSt <- runEncapsWithEmptyVars (useDefBasicBlocks (blockBody block) >> ST.get)
    finalState <- checkUseVariableStates (prevSt {usedVarSet = S.empty}) [(loopSt, location ann)]
    unifyState (optionBoxesMap finalState, movedBoxes finalState, S.union (usedVarSet prevSt) (usedVarSet finalState))
    maybe (return ()) useExpression mBrk
useDefBasicBlock (MatchBlock e mcase ann) = do
  prevSt <- ST.get
  sets <- maybe (throwError $ annotateError (location ann) EUnboxingExpressionType)
    (\case
        TOption (TBoxSubtype _) ->
            case mcase of
              [ml,mr] -> do
                let (mSome, mNone) = if matchIdentifier ml == "Some" then (ml,mr) else (mr,ml)
                someBlk <- runEncapsWithEmptyVars (useDefBasicBlocks (blockBody . matchBody $ mSome)
                  >> defBox (head (matchBVars mSome)) (location (matchAnnotation mSome)) >> ST.get)
                noneBlk <- runEncapsWithEmptyVars (useDefBasicBlocks (blockBody . matchBody $ mNone) >> ST.get)
                return [(someBlk, location . matchAnnotation $ mSome), (noneBlk, location . matchAnnotation $ mNone)]
              _ -> throwError $ annotateError Internal EMalformedOptionBoxMatch;
        -- Otherwise, it is a simple use variable.
        _ -> runMultipleEncapsWithEmptyVars (
          map (\c -> do
            blockSt <- useMCase c >> ST.get
            return (blockSt, location . matchAnnotation $ c)) mcase);
    ) (SM.getResultingType $ SM.getSemanticAnn $ getAnnotation e)
  finalState <- checkUseVariableStates (prevSt {usedVarSet = S.empty}) sets
  unifyState (optionBoxesMap finalState, movedBoxes finalState, S.union (usedVarSet prevSt) (usedVarSet finalState))
  useExpression e
useDefBasicBlock (SendMessage obj arg ann) = useObject obj >>
  case arg of
    AccessObject input_obj@(Variable var _) -> do
      input_obj_type <- withLocation (location ann) (getObjType input_obj)
      case input_obj_type of
        (_, TBoxSubtype _) -> let loc = location ann in
          safeMoveBox var loc
        _ -> useObject input_obj
    _ -> useExpression arg
useDefBasicBlock (AllocBox obj arg ann) = useObject obj >>
  case arg of
    -- I don't think we can have expression computing variables here.
    ReferenceExpression Mutable (Variable avar _anni) _ann ->
      allocOptionBox avar (location ann)
    _ ->throwError $ annotateError (location ann) EBadAllocArg
useDefBasicBlock (FreeBox obj arg ann)
  = useObject obj >>
  case arg of
    AccessObject (Variable var _anni) ->
      let loc = location ann in
      safeMoveBox var loc
    _ -> withLocation (location ann) (throwError EBadFreeArg)
useDefBasicBlock (ProcedureCall obj _ident args _ann)
  = useObject obj >> mapM_ useArguments args
useDefBasicBlock (AtomicLoad obj e _ann)
  = useObject obj >> useExpression e
useDefBasicBlock (AtomicStore obj e _ann)
  = useObject obj >> useExpression e
useDefBasicBlock (AtomicArrayLoad obj eI eO _ann)
  = useObject obj >> useExpression eI >> useExpression eO
useDefBasicBlock (AtomicArrayStore obj eI eO _ann)
  = useObject obj >> useExpression eI >> useExpression eO
useDefBasicBlock (RegularBlock stmts) = useDefStatements stmts
useDefBasicBlock (ReturnBlock e _ann) =
  maybe (return ()) useExpression e
useDefBasicBlock (ContinueBlock e _ann) = useExpression e

-- General case, not when it is TOption Box
useMCase :: MatchCase SemanticAnn -> UDM VarUsageError ()
useMCase (MatchCase _mIdent bvars blk ann)
  = useDefBasicBlocks (blockBody blk)
  >> mapM_ (`defVariable` location ann) bvars

checkUseVariableStates :: UDSt -> [(UDSt, Location)] -> UDM VarUsageError UDSt
checkUseVariableStates prevSt sets = do
  finalSt <- checkOptionBoxStates prevSt sets 
  checkSameMovedBoxes (map (first (flip M.difference (movedBoxes prevSt) . movedBoxes)) sets)
  return finalSt

checkSameMovedBoxes :: [(VarMap, Location)] -> UDM VarUsageError ()
checkSameMovedBoxes [] = return ()
checkSameMovedBoxes [(boxes, _)] = 
  case M.toList boxes of
    [] -> return ()
    ((ident, loc):_) -> throwError $ annotateError loc (EBoxMoveConditionalBranch ident)
checkSameMovedBoxes (x:xs) = mapM_ (sameMovedBoxes x) xs

  where

    sameMovedBoxes :: (VarMap, Location) -> (VarMap, Location) -> UDM VarUsageError ()
    sameMovedBoxes (lmap, lloc) (rmap, rloc) = do
      mapM_ (\k ->
        case (M.lookup k lmap, M.lookup k rmap) of
          (Nothing, Nothing) -> throwError $ annotateError Internal EUnboxingVariableMap
          (Nothing, Just vloc)  -> throwError $ annotateError lloc (EMissingBoxMove k vloc)
          (Just vloc, Nothing)  -> throwError $ annotateError rloc (EMissingBoxMove k vloc)
          _ -> return ()) (M.keys $ M.union lmap rmap)

checkOptionBoxStates :: UDSt -> [(UDSt, Location)] -> UDM VarUsageError UDSt
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
        -- | If the option-box is not in the previous state, it means that
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

useDefCMemb :: ClassMember SemanticAnn -> UDM VarUsageError ()
useDefCMemb (ClassField fdef ann)
  = defVariable (fieldIdentifier fdef) (location ann)
useDefCMemb (ClassMethod _ident _tyret bret _ann)
  = useDefBlockRet bret
useDefCMemb (ClassProcedure _ident ps blk ann)
  = useDefBlockRet blk
  >> mapM_ (`defArgumentsProc` location ann) ps
  -- >> mapM_ (annotateError (location ann) . defVariable . paramIdentifier) ps
useDefCMemb (ClassViewer _ident ps _tyret bret ann)
  = useDefBlockRet bret
  >> mapM_ ((`defVariable` location ann) . paramIdentifier) ps
useDefCMemb (ClassAction _ident p _tyret bret ann)
  = useDefBlockRet bret
  >> mapM_ (`defArgumentsProc` location ann) [p]

useDefTypeDef :: TypeDef SemanticAnn -> UDM VarUsageError ()
useDefTypeDef (Class _k _id members _provides _mods)
  -- First we go through uses
  = mapM_ useDefCMemb muses
  -- Then all definitions (ClassFields)
  >> mapM_ useDefCMemb mdefs
  where
    (muses,mdefs) = foldr (\m (u,d)->
                             case m of {
                               ClassField (FieldDefinition _ (TSinkPort {})) _ -> (u, d);
                               ClassField (FieldDefinition _ (TInPort {})) _ -> (u, d);
                               ClassField {} -> (u, m:d);
                               _             -> (m:u, d)
                                       }) ([],[]) members
useDefTypeDef (Struct {}) = return ()
useDefTypeDef (Interface {}) = return ()
useDefTypeDef (Enum {}) = return ()

-- Globals
useDefFrag :: AnnASTElement SemanticAnn -> UDM VarUsageError ()
useDefFrag (Function _ident ps _ty blk _mods anns)
 = useDefBlockRet blk
 >> mapM_ (`defArgumentsProc` location anns) ps
 -- >> mapM_ ((annotateError (location anns)) . defVariable . paramIdentifier) ps
useDefFrag (GlobalDeclaration {})
  = return ()
useDefFrag (TypeDefinition tyDef _ann)
  = useDefTypeDef tyDef

runUDFrag :: AnnASTElement SemanticAnn -> Maybe VarUsageError
runUDFrag =
  either Just (const Nothing)
  . fst
  . runComputation
  . useDefFrag

runUDAnnotatedProgram :: AnnotatedProgram  SemanticAnn -> Maybe VarUsageError
runUDAnnotatedProgram
  = safeHead
  . filter isJust
  . map runUDFrag
  where
    safeHead []     = Nothing
    safeHead (x:_) = x
