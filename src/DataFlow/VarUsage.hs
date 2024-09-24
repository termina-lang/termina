-- | Simple POC of data flo analysis to compute flow of box variables.

module DataFlow.VarUsage (
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
import           DataFlow.VarUsage.Computation
import           DataFlow.VarUsage.Errors

import Utils.Annotations

import           Control.Monad
import           Control.Monad.Except

import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Map.Strict      as M

-- AST to work with.
import           Semantic.AST            as SAST
-- We need to know the type of objects.
import qualified Semantic.Monad as SM
import Semantic.Types


-- There are two types of arguments :
-- + Moving out variables of type box and Option<box T>
-- + Copying expressions, everything.
-- It is context dependent (AFAIK).
useArguments :: Expression SemanticAnn -> UDM VarUsageError ()
-- If we are giving a variable of type Box, we moving it out.
useArguments e@(AccessObject (Variable ident ann))
  = case SM.getTypeSemAnn ann of
     Just (BoxSubtype _) ->
       withLocation (location ann) $ useBoxVar ident
     _ -> useExpression e
-- Box variables inside expressions are read as values.
useArguments e = useExpression e

useObject :: Object SemanticAnn -> UDM VarUsageError ()
useObject (Variable ident ann)
  =
  let loc = location ann in
  maybe
        (throwError $ annotateError loc ImpossibleError)
        (\case {
                -- Nothing, we can use it freely, in the normal sense of using it.
                BoxSubtype _ -> return ()
                ;
            -- We can use Options only once!
                Option (BoxSubtype _) ->
                withLocation loc $ addUseOnlyOnce ident
                ;
            -- Mutable Options??
                _ ->
                withLocation loc $ safeAddUse ident
        }) (SM.getTypeSemAnn ann)
useObject (ArrayIndexExpression obj e _ann)
  = useObject obj >> useExpression e
useObject (MemberAccess obj _i _ann)
  = useObject obj
useObject (Dereference obj _ann)
  = useObject obj
useObject (DereferenceMemberAccess obj i ann)
  = withLocation (location ann) (safeAddUse i)
  >> useObject obj
-- TODO Use Object unbox?
useObject (Unbox obj _ann)
  = useObject obj

useFieldAssignment :: FieldAssignment SemanticAnn -> UDM VarUsageError ()
useFieldAssignment (FieldValueAssignment _ident e _) = useExpression e
-- Should we also check port connections? This is `global` to taks level :shrug:
useFieldAssignment _ = return ()

getObjectType :: Object SemanticAnn -> UDM Error (AccessKind, TerminaType)
getObjectType = maybe (throwError ImpossibleError) return . SM.getObjectSAnns . getAnnotation

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
useExpression (MemberFunctionCall obj ident args ann) = do
    useObject obj
    obj_type <- withLocation (location ann) (getObjectType obj)
    case obj_type  of
      (_, AccessPort (Allocator {})) ->
        withLocation (location ann) (case ident of
          "alloc" -> do
            case args of
              -- I don't think we can have expression computing variables here.
              [ReferenceExpression Mutable (Variable avar _anni) _ann] -> allocOO avar
              _ -> throwError ImpossibleErrorBadAllocArg
          "free" -> do
            case args of
              [AccessObject (Variable var _anni)] -> useBoxVar var
              _ -> throwError ImpossibleErrorBadFreeArg
          _ -> throwError ImpossibleError) -- Pools only have alloc
      (_, OutPort {}) ->
        case ident of
          "send" -> do
            case args of
              [AccessObject input_obj@(Variable var _)] -> do
                  input_obj_type <- withLocation (location ann) (getObjectType input_obj)
                  case input_obj_type of
                    (_, BoxSubtype _) -> withLocation (location ann) (useBoxVar var)
                    _ -> useObject input_obj
              _ -> withLocation (location ann) (throwError ImpossibleErrorBadSendArg)
          _ -> withLocation (location ann) $ throwError ImpossibleError -- OutPorts only have send
      -- TODO Can Box be passed around as arguments?
      _ -> mapM_ useArguments args
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

useDefBlockRet :: BlockRet SemanticAnn -> UDM VarUsageError ()
useDefBlockRet bret =
  maybe (return ()) useExpression (returnExpression (blockRet bret))
  >> useDefBlock (blockBody bret)

-- Not so sure about this.
useDefBlock :: Block SemanticAnn -> UDM VarUsageError ()
useDefBlock = mapM_ useDefStmt . reverse

useDefStmt :: Statement SemanticAnn -> UDM VarUsageError ()
useDefStmt (Declaration ident _accK tyS initE ann)
  -- variable def is defined
  =
  withLocation (location ann)
  (case tyS of
    -- Box are only declared on match statements
    Option (BoxSubtype _) -> defVariableOO ident
    -- Box are not possible, they come from somewhere else.
    BoxSubtype _ -> throwError (DefiningBox ident)
    --Everything else
    _        -> defVariable ident)
  -- Use everithing in the |initE|
  >> useExpression initE
-- All branches should have the same used Only ones.
useDefStmt (AssignmentStmt obj e _ann)
  -- DONE [UseDef.Report.Q1]
  = useExpression e
  >> useObject obj
useDefStmt (IfElseStmt eCond bTrue elseIfs bFalse ann)
  = do
  -- All sets generated for all different branches.
  sets <- runEncaps
                ([ useDefBlock bTrue >> get
                 , maybe (return ()) useDefBlock bFalse >> get
                ]
                ++
                 map ((\l -> mapM_ useDefStmt l >> get) . reverse . elseIfBody) elseIfs
                )
  -- Rule here is, all branches should have the same onlyonce behaviour.
  let (usedOO, usedBoxes)
        = foldr (\s (oo,dd) -> (usedOption s : oo, usedBox s : dd)) ([],[]) sets -- (map usedOption sets)
  unless (sameMaps usedOO)
    (throwError $ annotateError (location ann) (DifferentOnlyOnce usedOO))
  unless (sameSets usedBoxes)
    (throwError $ annotateError (location ann) (DifferentBoxesSets usedBoxes))
  -- We get all uses
  let normalUses = S.unions (map usedSet sets)
  --
  continueWith (head usedOO) normalUses (head usedBoxes)
  -- Issue #40, forgot to use condition expression.
  useExpression eCond
  mapM_ (useExpression . elseIfCond) elseIfs
useDefStmt (ForLoopStmt _itIdent _itTy _eB _eE mBrk block ann)
  =
    -- Iterator body can alloc and free/give memory.
    -- It can only use variables /only/ only if they are declared inside the
    -- iterator body, and freed and everything.
    ----------------------------------------
    -- Break condition.
    -- Used at the end of the body of loop. Simulating next loop use.
    -- I think with this use should be enough.
    maybe (return ()) useExpression mBrk
    >>
    ----------------------------------------
    -- What happens inside the body of a for, may not happen at all.
    runEncapsulated
      ( modify emptyButUsed
        >> useDefBlock block
        >> get >>= \st -> -- No Option should be in the state
        unless (M.null (usedOption st)) (throwError $ annotateError (location ann) ForMoreOOpt)
        >> -- No Box should be in the state
        let usedBoxSet = usedBox st in
        unless (S.null usedBoxSet) (throwError $ annotateError (location ann) (ForMoreOBox (getVars usedBoxSet)))
        >> -- If everything goes okay, return used variables
        return (usedSet st)
      )
    ----------------------------------------
    -- We add all normal use variables (even if we do not guarantee they body is going to be executed.)
    >>= unionUsed -- Note that we add uses, but we do not remove definitions, that is dangerous.
    ----------------------------------------
    -- Break condition.
    -- Used at the beggining of for loop.
    >> maybe (return ()) useExpression mBrk
    ----------------------------------------
    -- Use expression over begin and end.
--    >> useExpression eB
--    >> useExpression eE
useDefStmt (MatchStmt e mcase ann)
  =
  -- Depending on expression |e| type
  -- we handle OptionBox special case properly.
  maybe (throwError $ annotateError (location ann) ImpossibleErrorMatchGetType)
    (\case
        Option (BoxSubtype _) ->
          case mcase of
            [x,y] ->
              let (mo,mn) = destroyOptionBox (x,y)
              in runEncapsEOO [mo,mn]
            _ -> throwError $ annotateError (location ann) InternalOptionMissMatch;
        -- Otherwise, it is a simple use variable.
        _ -> runEncapsEOO (map ( (>> get) . useMCase) mcase);
    )
    (SM.getResultingType $ SM.getSemanticAnn $ getAnnotation e)
  >>= \sets ->
  -- Get all OO sets
  let
    (usedOOpt, usedBoxes, usedN)
     = foldr (\s (opts,boxes,norms) ->
               ( usedOption s : opts
               , usedBox s : boxes
               , usedSet s : norms)) ([],[],[]) sets
  in
  -- Should all be the same
  unless (sameMaps usedOOpt)
        (throwError $ annotateError (location ann) DifferentOnlyOnceMatch)
  >>
  unless (sameSets usedBoxes)
        (throwError $ annotateError (location ann) DifferentBoxesSetsMatch)
  -- Then continue addin uses
  >> unionS
        (head usedOOpt)
        (S.unions usedN)
        (head usedBoxes)
  >> -- Use of expression matching
  useExpression e
useDefStmt (SingleExpStmt e _ann)
  = useExpression e

destroyOptionBox
  :: (MatchCase SemanticAnn, MatchCase SemanticAnn)
  -> (UDM VarUsageError UDSt , UDM VarUsageError UDSt)
destroyOptionBox (ml, mr)
  =
  let (mOpt, mNone) = if matchIdentifier ml == "Some" then (ml,mr) else (mr,ml)
  in
    (useDefBlock (matchBody mOpt)
      >>
     withLocation (location (matchAnnotation mOpt)) (defBoxVar (head (matchBVars mOpt)))
     >> get
    , useDefBlock (matchBody mNone) >> get)

-- General case, not when it is Option Box
useMCase :: MatchCase SemanticAnn -> UDM VarUsageError ()
useMCase (MatchCase _mIdent bvars blk ann)
  = useDefBlock blk
  >> withLocation (location ann) (mapM_ defVariable bvars)

sameMaps :: [OOVarSt] -> Bool
sameMaps [] = False
sameMaps (x:xs) = all (M.null . M.difference x) xs

sameSets :: [VarSet] -> Bool
sameSets [] = False
sameSets (x:xs) = all (S.null . S.difference x) xs

useDefCMemb :: ClassMember SemanticAnn -> UDM VarUsageError ()
useDefCMemb (ClassField fdef ann)
  = withLocation (location ann) (defVariable (fieldIdentifier fdef))
useDefCMemb (ClassMethod _ident _tyret bret _ann)
  = useDefBlockRet bret
useDefCMemb (ClassProcedure _ident ps blk ann)
  = useDefBlockRet blk
  >> mapM_ (withLocation (location ann) . defArgumentsProc) ps
  -- >> mapM_ (annotateError (location ann) . defVariable . paramIdentifier) ps
useDefCMemb (ClassViewer _ident ps _tyret bret ann)
  = useDefBlockRet bret
  >> mapM_ (withLocation (location ann) . defVariable . paramIdentifier) ps
useDefCMemb (ClassAction _ident p _tyret bret ann)
  = useDefBlockRet bret
  >> mapM_ (withLocation (location ann) . defArgumentsProc) [p]

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
 >> mapM_ (withLocation (location anns) . defArgumentsProc ) ps
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
