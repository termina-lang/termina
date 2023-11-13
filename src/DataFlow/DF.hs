-- | Simple POC of data flo analysis to compute flow of dynamic variables.

module DataFlow.DF where

{-
At termina level, each block is a basic block.
Maybe ask Pablo about this, but functions do not change values unless are
explicit about it using the |mut| operator.

In this module, we implement a backward analysis.
That is, given a block, i.e. a sequence of statements, we go to the last
statement and build our sets backwards.
-}

-- Monad and manipulations
import           DataFlow.Computation
import           DataFlow.Errors

import Annotations

import           Control.Monad
import           Control.Monad.Except

import           Data.List            (all, find, foldl')
import           Data.Maybe
-- import qualified Data.Set             as S
import qualified Data.Map.Strict      as M

import           AST.Core             (Identifier)
-- AST to work with.
import           AST.Seman            as SAST
-- We need to know the type of objects.
import qualified Semantic.Monad       as SM (location)
import           Semantic.Monad       (SemanticAnns (..), SAnns(..), getResultingType, getTypeSAnns, getObjectSAnns)


import Debug.Termina

-- Constant expression could be global variables, should we check they are used
-- too?
useConstE :: ConstExpression -> UDM AnnotatedErrors ()
useConstE = const (return ())

useObject, defObject :: Object SemanticAnns -> UDM AnnotatedErrors ()
useObject (Variable ident ann)
  =
  let loc = SM.location ann in
  maybe
        (loc `annotateError` throwError ImpossibleError)
        (\case {
                -- Nothing, we can use it freely, in the normal sense of using it.
                DynamicSubtype _ ->
                return ()
                ;
            -- We can use Options only once!
                Option _ ->
                loc  `annotateError` addUseOnlyOnce ident
                ;
            -- Mutable Options??
                _ ->
                loc `annotateError` safeAddUse ident
        }) (getTypeSAnns ann)
useObject (VectorIndexExpression obj e _ann)
  = useObject obj >> useExpression e
useObject (MemberAccess obj _i _ann)
  = useObject obj
useObject (Dereference obj _ann)
  = useObject obj
useObject (DereferenceMemberAccess obj i ann)
  = (SM.location ann) `annotateError` safeAddUse i
  >> useObject obj
useObject (VectorSliceExpression obj eB eT _ann)
  = useObject obj >> useConstE eB >> useConstE eT
-- TODO Use Object undyn?
useObject (Undyn obj _ann)
  = useObject obj

defObject (Variable ident ann)
  = (SM.location ann) `annotateError` defVariable ident
defObject (VectorIndexExpression obj e _ann)
  = useExpression e
  >> useObject obj
defObject (MemberAccess obj _i _ann)
  = useObject obj
defObject (Dereference obj _ann)
  = useObject obj
defObject (DereferenceMemberAccess obj i ann)
  = (SM.location ann) `annotateError` safeAddUse i
  >> useObject obj
defObject (VectorSliceExpression obj eB eT _ann)
  = useObject obj >> useConstE eB >> useConstE eT
defObject (Undyn obj _ann)
  = useObject obj

useFieldAssignment :: FieldAssignment SemanticAnns -> UDM AnnotatedErrors ()
useFieldAssignment (FieldValueAssignment _ident e) = useExpression e
-- Should we also check port connections? This is `global` to taks level :shrug:
useFieldAssignemnt _ = return ()

getObjectType :: Object SemanticAnns -> UDM Errors (AccessKind, TypeSpecifier)
getObjectType = maybe (throwError ImpossibleError) return . getObjectSAnns . getAnnotation

useExpression :: Expression SemanticAnns -> UDM AnnotatedErrors ()
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
useExpression (MemberFunctionAccess obj ident args ann) = do
    useObject obj
    obj_type <- annotateError (SM.location ann) (getObjectType obj)
    case obj_type  of
      (_, Port (Pool {})) ->
        annotateError (SM.location ann) (case ident of
          "alloc" -> do
            case args of
              -- I don't think we can have expression computing variables here.
              [ReferenceExpression Mutable (Variable avar _anni) _ann] -> allocOO avar
              _ -> throwError ImpossibleErrorBadAllocArg
          _ -> throwError ImpossibleError) -- Pools only have alloc
      (_, Port (MsgQueue {})) ->
        case ident of
          "send" -> do
            case args of
              [AccessObject (Variable var _), ReferenceExpression Mutable res_obj _] -> annotateError (SM.location ann) (useDynVar var) >> useObject res_obj
              _ -> annotateError (SM.location ann) (throwError ImpossibleErrorBadSendArg)
          _ -> mapM_ useExpression args -- receive, receive_timed, try_receive
      _ -> mapM_ useExpression args
  -- when requesting stuff from a pool with alloc.
--  | ident == "alloc"
--  = useObject obj
--  >> (annotateError (SM.location ann)
--     $ case args of
--        -- I don't think we can have expression computing variables here.
--        [ReferenceExpression Mutable (Variable avar _anni) _ann] ->
--          (allocOO avar)
--        _ -> throwError ImpossibleErrorBadAllocArg)
  -- >> mapM_ useExpression args
  -- other functions
--  | ident == "send"
--    = useObject obj
--    >> error "TODO"
    -- (annotateError (SM.location ann))
    --    $ case args of
    --         _ -> error "TODO"
--  | otherwise
--    = useObject obj >> mapM_ useExpression args
useExpression (DerefMemberFunctionAccess obj _ident args _ann)
  = useObject obj >> mapM_ useExpression args
useExpression (VectorInitExpression e _size _ann)
  = useExpression e
useExpression (FieldAssignmentsExpression _ident fs _ann)
  = mapM_ useFieldAssignment fs
useExpression (EnumVariantExpression _ident _ident2 es _ann)
  = mapM_ useExpression es
useExpression (OptionVariantExpression opt _ann)
  = case opt of
        None   -> return ()
        Some e -> useExpression e
useExpression (FunctionExpression _ident args _ann)
  = mapM_ useExpression args

useDefBlockRet :: BlockRet SemanticAnns -> UDM AnnotatedErrors ()
useDefBlockRet bret =
  maybe (return ()) useExpression (returnExpression (blockRet bret))
  >> useDefBlock (blockBody bret)

-- Not so sure about this.
useDefBlock :: Block SemanticAnns -> UDM AnnotatedErrors ()
useDefBlock = mapM_ useDefStmt . reverse

useDefStmt :: Statement SemanticAnns -> UDM AnnotatedErrors ()
useDefStmt (Declaration ident _accK tyS initE ann)
  -- variable def is defined
  =
  (SM.location ann) `annotateError`
  case tyS of
    Option _ -> defVariableOO ident
    -- DynamicSubtype _ ->  This is not possible
    DynamicSubtype _ -> throwError (DefiningDyn ident)
    -- Dynamic are only declared on match statements
    _        -> defVariable ident
  -- Use everithing in the |initE|
  >> useExpression initE
-- All branches should have the same used Only ones.
useDefStmt (AssignmentStmt obj e _ann)
  = useExpression e
  >> useObject obj
  -- DONE [Q1] Still not super sure, but acceptable.
useDefStmt (IfElseStmt eCond bTrue elseIfs bFalse ann)
  = do
  -- All sets generated for all different branches.
  sets <- runEncaps
                ([ useDefBlock bTrue >> get
                 , useDefBlock bFalse >> get
                ]
                ++
                 map ((\l -> mapM_ useDefStmt l >> get) . reverse . elseIfBody) elseIfs
                )
  -- Rule here is, all branches should have the same onlyonce behaviour.
  let (usedOO, usedDyns)
        = foldr (\s (oo,dd) -> (usedOption s : oo, usedDyn s : dd)) ([],[]) sets -- (map usedOption sets)
  unless (sameMaps usedOO)
    ((SM.location ann) `annotateError` throwError DifferentOnlyOnce)
  unless (sameSets usedDyns)
    ((SM.location ann) `annotateError` throwError DifferentDynsSets)
  -- We get all uses
  let normalUses = M.unions (map usedSet sets)
  --TODO remove continueWith
  continueWith (head usedOO) normalUses (head usedDyns)
useDefStmt (ForLoopStmt itIdent _itTy eB eE mBrk block ann)
  =
    -- Iterator body can alloc and free/give memory.
    -- Interator bode runs with empty UsedOnly and should return it that way.
    -- It can only use variables /only/ only if they are declared inside the
    -- iterator body, and freed and everything.
    runEncapsulated
      ( emptyOO
        >> useDefBlock block
        >> get
        >>= \st ->
          -- No Option should be in the state
          unless (M.null (usedOption st)) ((SM.location ann) `annotateError` throwError ForMoreOOpt)
          >>
          -- No Dyn should be in the state
          unless (M.null (usedDyn st)) ((SM.location ann) `annotateError` throwError ForMoreODyn)
          >>
          -- If everything goes okay, return used variables
          return (usedSet st)
      )
    -- We add all normal use variables
    >>= unionUsed
    -- Definition of iteration varialbe.
    >> (SM.location ann) `annotateError` defVariable itIdent
    -- Use expression over begin, end, and break condition.
    >> maybe (return ()) useExpression mBrk
    >> useExpression eB
    >> useExpression eE
useDefStmt (MatchStmt e mcase ann)
  = -- Similar to the case ifElse and For
  (useExpression e)
  >>
  -- Depending on expression |e| type
  -- we handle OptionDyn special case properly.
  maybe ((SM.location ann) `annotateError`throwError ImpossibleErrorMatchGetType)
    (\case
        Option (DynamicSubtype _) ->
          case mcase of
            [x,y] ->
              let (mo,mn) = destroyOptionDyn (x,y)
              in runEncapsEOO [mo,mn]
            _ -> (SM.location ann) `annotateError` throwError InternalOptionMissMatch;
        -- Otherwise, it is a simple use variable.
        _ -> runEncapsEOO (map ( (>> get) . useMCase) mcase);
    )
    (getResultingType $ ty_ann $ getAnnotation e)
  >>= \sets ->
  -- Get all OO sets
  let
    (usedOOpt, usedDyns, usedN)
     = foldr (\s (opts,dyns,norms) ->
               ( usedOption s : opts
               , usedDyn s : dyns
               , usedSet s : norms)) ([],[],[]) sets
  in
  -- Should all be the same
  unless (sameMaps usedOOpt)
        ((SM.location ann) `annotateError` throwError DifferentOnlyOnceMatch)
  >>
  unless (sameSets usedDyns)
        ((SM.location ann) `annotateError` throwError DifferentDynsSetsMatch)
  -- Then continue addin uses
  >> unionS
        (head usedOOpt)
        (M.unions usedN)
        (head usedDyns)
useDefStmt (SingleExpStmt e _ann)
  = useExpression e
useDefStmt (Free obj _ann)
  = case obj of
      Variable var ann
        -> (SM.location ann) `annotateError` useDynVar var
      -- Idk If we can free whatever object, can we?
      _
        -> useObject obj

destroyOptionDyn
  :: (MatchCase SemanticAnns, MatchCase SemanticAnns)
  -> (UDM AnnotatedErrors UDSt , UDM AnnotatedErrors UDSt)
destroyOptionDyn (ml, mr)
  =
  let (mOpt, mNone) = if matchIdentifier ml == "Some" then (ml,mr) else (mr,ml)
  in
    ( useDefBlock (matchBody mOpt)
      >>
     (SM.location (matchAnnotation mOpt)) `annotateError` defDynVar (head (matchBVars mOpt))
     >> get
    , useDefBlock (matchBody mNone) >> get)

-- General case, not when it is Option Dyn
useMCase :: MatchCase SemanticAnns -> UDM AnnotatedErrors ()
useMCase (MatchCase _mIdent bvars blk ann)
  =
  useDefBlock blk
  >> (SM.location ann) `annotateError` mapM_ defVariable bvars

sameMaps :: [OOVarSt] -> Bool
sameMaps [] = False
sameMaps (x:xs) = all (M.null . M.difference x) xs

sameSets :: [VarSet] -> Bool
sameSets [] = False
sameSets (x:xs) = all (M.null . M.difference x) xs

useDefCMemb :: ClassMember SemanticAnns -> UDM AnnotatedErrors ()
useDefCMemb (ClassField fdef ann)
  = (SM.location ann) `annotateError` defVariable (fieldIdentifier fdef)
useDefCMemb (ClassMethod _ident _tyret bret _ann)
  = useDefBlockRet bret
useDefCMemb (ClassProcedure _ident ps blk ann)
  = useDefBlock blk
  >> mapM_ (annotateError (SM.location ann) . defVariable . paramIdentifier) ps
useDefCMemb (ClassViewer _ident ps _tyret bret ann)
  = useDefBlockRet bret
  >> mapM_ (annotateError (SM.location ann) . defVariable . paramIdentifier) ps

useDefTypeDef :: TypeDef SemanticAnns -> UDM AnnotatedErrors ()
useDefTypeDef (Class _k _id members _mods)
  -- First we go through uses
  = mapM_ useDefCMemb muses
  -- Then all definitions (ClassFields)
  >> mapM_ useDefCMemb mdefs
  where
    (muses,mdefs) = foldr (\m (u,d)->
                             case m of {
                               ClassField {} -> (u, m:d);
                               _             -> (m:u, d)
                                       }) ([],[]) members
useDefTypeDef (Struct {}) = return ()
useDefTypeDef (Enum {}) = return ()

-- Globals
useDefFrag :: AnnASTElement SemanticAnns -> UDM AnnotatedErrors ()
useDefFrag (Function _ident ps _ty blk _mods anns)
 = useDefBlockRet blk
 >> mapM_ ((annotateError (SM.location anns)) . defVariable . paramIdentifier) ps
useDefFrag (GlobalDeclaration {})
  = return ()
useDefFrag (TypeDefinition tyDef _ann)
  = (useDefTypeDef tyDef)

runUDFrag :: AnnASTElement SemanticAnns -> Maybe AnnotatedErrors
runUDFrag =
  either Just (const Nothing)
  . fst
  . runComputation
  . useDefFrag

runUDAnnotatedProgram :: AnnotatedProgram  SemanticAnns -> Maybe AnnotatedErrors
runUDAnnotatedProgram
  = safeHead
  . filter isJust
  . map runUDFrag
  where
    safeHead []     = Nothing
    safeHead (x:xs) = x

runUDTerminaProgram :: TerminaProgram SemanticAnns -> Maybe AnnotatedErrors
runUDTerminaProgram = runUDAnnotatedProgram . frags
