-- | Simple POC of data flo analysis to compute flow of box variables.

module ControlFlow.VarUsage (
  runUDAnnotatedProgram
) where

{-
At termina level, each block is a basic block.
Maybe ask Pablo about this, but functions do not change values unless are
explicit about it using the |mut| operator.

In this module, we implement a backward analysis.
That is, given a block, i.e. a sequence of statements, we go to the last
statement and build our sets backwards.
-}

-- Monad and manipulations
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


-- There are two types of arguments :
-- + Moving out variables of type box and Option<box T>
-- + Copying expressions, everything.
-- It is context dependent (AFAIK).
useArguments :: Expression SemanticAnn -> UDM VarUsageError ()
-- If we are giving a variable of type Box, we moving it out.
useArguments e@(AccessObject (Variable ident ann))
  = case SM.getTypeSemAnn ann of
    Just (BoxSubtype _) ->
      let loc = location ann in
      withLocation loc $ safeMoveBox ident loc
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
            -- Nothing, we can use it freely, in the normal sense of using it.
                BoxSubtype _ -> return ();
            -- We can use Options only once!
                Option (BoxSubtype _) -> moveOptionBox ident loc;
                _ -> withLocation loc $ safeUseVariable ident loc
        }) (SM.getTypeSemAnn ann)
useObject (ArrayIndexExpression obj e _ann)
  = useObject obj >> useExpression e
useObject (MemberAccess obj _i _ann)
  = useObject obj
useObject (Dereference obj _ann)
  = useObject obj
useObject (DereferenceMemberAccess obj i ann)
  = let loc = location ann in
    withLocation loc (safeUseVariable i loc)
  >> useObject obj
-- TODO Use Object unbox?
useObject (Unbox obj _ann)
  = useObject obj

useFieldAssignment :: FieldAssignment SemanticAnn -> UDM VarUsageError ()
useFieldAssignment (FieldValueAssignment _ident e _) = useExpression e
-- Should we also check port connections? This is `global` to taks level :shrug:
useFieldAssignment _ = return ()

getObjectType :: Object SemanticAnn -> UDM Error (AccessKind, TerminaType)
getObjectType = maybe (throwError EUnboxingObjectType) return . SM.getObjectSAnns . getAnnotation

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
      -- TODO Can Box be passed around as arguments?
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
      -- TODO Can Box be passed around as arguments?
  = mapM_ useArguments args

useDefBlockRet :: Block SemanticAnn -> UDM VarUsageError ()
useDefBlockRet bret = useDefBasicBlocks (blockBody bret)

useDefStmt :: Statement SemanticAnn -> UDM VarUsageError ()
useDefStmt (Declaration ident _accK tyS initE ann)
  -- variable def is defined
  = let loc = location ann in
  case tyS of
    -- Box are only declared on match statements
    Option (BoxSubtype _) -> defVariableOptionBox ident loc
    -- Box are not possible, they come from somewhere else.
    BoxSubtype _ -> throwError $ annotateError loc EDefiningBox
    --Everything else
    _        -> defVariable ident loc
  -- Use everithing in the |initE|
  >> useExpression initE
-- All branches should have the same used Only ones.
useDefStmt (AssignmentStmt obj e _ann)
  -- DONE [UseDef.Report.Q1]
  = useExpression e
  >> useObject obj
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
  -- All sets generated for all different branches.
  sets <- mapM (\(body, loc) -> do
    blockSt <- runEncapsulated (useDefBasicBlocks body >> ST.get)
    return (blockSt, loc)) bodiesWithLocs
  -- Rule here is, when entering, the state of all the boxes must be the same and the
  -- set of used boxes must be equal.
  (finalOptionBoxMap, finalBoxes, usedRegular) <- checkUseVariableStates sets
  unionS (finalOptionBoxMap, finalBoxes, usedRegular)
  -- Use the else-ifs conditional expressions
  mapM_ (useExpression . elseIfCond) elseIfs
  -- Finally, use the if conditional expression
  useExpression eCond
useDefBasicBlock (ForLoopBlock  _itIdent _itTy _eB _eE mBrk block _ann) = do
    ----------------------------------------
    prevSt <- ST.get
    -- What happens inside the body of a for, may not happen at all.
    loopSt <- runEncapsulated (useDefBasicBlocks (blockBody block) >> ST.get)
    usedRegular <- checUseVariableLoop prevSt loopSt
    ----------------------------------------
    unionUsed usedRegular
    ----------------------------------------
    -- Break condition.
    -- Used at the beggining of for loop.
    >> maybe (return ()) useExpression mBrk
useDefBasicBlock (MatchBlock e mcase ann)
  = do
  -- Depending on expression |e| type
  -- we handle OptionBox special case properly.
  sets <- maybe (throwError $ annotateError (location ann) EUnboxingExpressionType)
    (\case
        Option (BoxSubtype _) ->
            case mcase of
              [ml,mr] -> do
                let (mSome, mNone) = if matchIdentifier ml == "Some" then (ml,mr) else (mr,ml)
                someBlk <- runEncapsulated (useDefBasicBlocks (blockBody . matchBody $ mSome) 
                  >> defBox (head (matchBVars mSome)) (location (matchAnnotation mSome)) >> ST.get)
                noneBlk <- runEncapsulated (useDefBasicBlocks (blockBody . matchBody $ mNone) >> ST.get)
                return [(someBlk, location . matchAnnotation $ mSome), (noneBlk, location . matchAnnotation $ mNone)]
              _ -> throwError $ annotateError Internal EMalformedOptionBoxMatch;
        -- Otherwise, it is a simple use variable.
        _ -> runEncaps (
          map (\c -> do
            blockSt <- useMCase c >> ST.get
            return (blockSt, location . matchAnnotation $ c)) mcase);
    ) (SM.getResultingType $ SM.getSemanticAnn $ getAnnotation e)
  (finalOptionBoxMap, finalBoxes, usedRegular) <- checkUseVariableStates sets
  unionS (finalOptionBoxMap, finalBoxes, usedRegular)
  useExpression e
useDefBasicBlock (SendMessage obj arg ann) = useObject obj >>
  case arg of
    AccessObject input_obj@(Variable var _) -> do
      input_obj_type <- withLocation (location ann) (getObjectType input_obj)
      case input_obj_type of
        (_, BoxSubtype _) -> let loc = location ann in
          withLocation loc (safeMoveBox var loc)
        _ -> useObject input_obj
    _ -> useExpression arg
useDefBasicBlock (AllocBox obj arg ann) = useObject obj >>
  case arg of
    -- I don't think we can have expression computing variables here.
    ReferenceExpression Mutable (Variable avar _anni) _ann ->
      allocBox avar (location ann)
    _ ->throwError $ annotateError (location ann) EBadAllocArg
useDefBasicBlock (FreeBox obj arg ann)
  = useObject obj >>
  case arg of
    AccessObject (Variable var _anni) ->
      let loc = location ann in
      withLocation loc (safeMoveBox var loc)
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

-- General case, not when it is Option Box
useMCase :: MatchCase SemanticAnn -> UDM VarUsageError ()
useMCase (MatchCase _mIdent bvars blk ann)
  = useDefBasicBlocks (blockBody blk)
  >> mapM_ (`defVariable` location ann) bvars

checUseVariableLoop :: UDSt -> UDSt -> UDM VarUsageError VarMap
checUseVariableLoop prevMap loopst = do
  let prevOptionBoxes = optionBoxesMap prevMap
  mapM_ (\k -> 
    let pval = prevOptionBoxes M.! k in
    case M.lookup k (optionBoxesMap loopst) of
      Nothing -> throwError $ annotateError Internal EMissingOptionBox
      Just loopval -> unless (sameState pval loopval) (throwError $ annotateError (getLocation loopval) (EDifferentOptionBoxUseLoop k))) (M.keys prevOptionBoxes)

--  unless (M.null (movedBoxes loopst)) (
--    throwError $ annotateError )
  -- Then continue adding uses
  return $ usedVarMap loopst

checkUseVariableStates :: [(UDSt, Location)] -> UDM VarUsageError (OptionBoxMap, VarMap, VarMap)
checkUseVariableStates sets = do
  let states = fst <$> sets
  -- Should all be the same
  checkOptionBoxStates (map (first optionBoxesMap) sets)
  checkMovedBoxes (map (first movedBoxes) sets)
  -- Then continue adding uses
  return (optionBoxesMap . head $ states, movedBoxes . head $ states, M.unions (usedVarMap <$> states))

checkMovedBoxes :: [(VarMap, Location)] -> UDM VarUsageError ()
checkMovedBoxes [] = return ()
checkMovedBoxes (x:xs) = mapM_ (sameMovedBoxes x) xs

  where

    sameMovedBoxes :: (VarMap, Location) -> (VarMap, Location) -> UDM VarUsageError ()
    sameMovedBoxes (lmap, lloc) (rmap, rloc) = do
      mapM_ (\k -> 
        case M.lookup k rmap of
          Nothing -> throwError $ annotateError rloc (EMissingBoxMove k lloc)
          Just _ -> return ()) (M.keys lmap)

checkOptionBoxStates :: [(OptionBoxMap, Location)] -> UDM VarUsageError ()
checkOptionBoxStates [] = return ()
checkOptionBoxStates (x:xs) = mapM_ (sameOptionBoxState x) xs

  where

    sameOptionBoxState :: (OptionBoxMap, Location) -> (OptionBoxMap, Location) -> UDM VarUsageError ()
    sameOptionBoxState (lmap, lloc) (rmap, _) = do
      mapM_ (\k -> 
        let lval = lmap M.! k in
        case M.lookup k rmap of
          Nothing -> throwError $ annotateError Internal EMissingOptionBox
          Just rval -> unless (sameState rval lval) (throwError $ annotateError (getLocation rval) (EDifferentOptionBoxUse k lloc))) (M.keys lmap)

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
                               ClassField (FieldDefinition _ (SinkPort {})) _ -> (u, d);
                               ClassField (FieldDefinition _ (InPort {})) _ -> (u, d);
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
