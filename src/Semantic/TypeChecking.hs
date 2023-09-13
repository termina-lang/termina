{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

-- | Semantic Analysis Module i.e. Type checking
-- In particular this module, describes a mapping from |AST Parser.Annotation|
-- to ~|AST SemanticAnnotations|~ | SemanAST {ParserInfo , TypeInfo}|

module Semantic.TypeChecking where

-- Debugging
-- import           Debugging

-- Termina Ast and Utils
import           Annotations
import           AST                  as PAST
import           Utils.AST
import           Utils.CoreAST
import           Utils.TypeSpecifier

-- Termina Semantic AST
import qualified SemanAST             as SAST

-- Just annotations from Parser
import qualified Parsing              as Parser (Annotation (..))

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import           Semantic.Types
-- Error module
import           Control.Monad.Except (MonadError (..))
import           Semantic.Errors
-- Semantic Monad
import           Semantic.Monad

----------------------------------------
-- Libaries and stuff

import           Data.List            (find, foldl', map, nub, sortOn, (\\))
import           Data.Maybe

-- import Control.Monad.State as ST
import           Control.Monad

import qualified Data.Graph           as Graph

type SemanticPass t = t Parser.Annotation -> SemanticMonad (t SemanticAnns)

----------------------------------------
-- Function type-checking binary operations.
-- It returns resulting type of using an operation.
typeOfOps :: Locations -> PAST.Op -> PAST.TypeSpecifier -> PAST.TypeSpecifier -> SemanticMonad SemanticAnns
typeOfOps locs op lty rty =
  either
  -- | Check if there was an error, if not, returns the first type.
  (return . SemAnn locs . ETy)
  (throwError . annotateError locs . uncurry (EOpMismatch op))
  $ typeOfOps' op lty rty
  where
    typeOfOps' :: Op -> TypeSpecifier -> TypeSpecifier -> Either TypeSpecifier (TypeSpecifier,TypeSpecifier)
    -- Alg ops Same numeric type
    typeOfOps' Multiplication tyl tyr     = cmpNumTy tyl tyl tyr
    typeOfOps' Division tyl tyr           = cmpNumTy tyl tyl tyr
    typeOfOps' Addition tyl tyr           = cmpNumTy tyl tyl tyr
    typeOfOps' Subtraction tyl tyr        = cmpNumTy tyl tyl tyr
    -- shifts both numeric but may not be the same
    -- Q2
    typeOfOps' BitwiseLeftShift tyl tyr   = justNumTy tyl tyl tyr
    typeOfOps' BitwiseRightShift tyl tyr  = justNumTy tyl tyl tyr
    -- >, =>, <, <= : some numeric type.
    typeOfOps' RelationalLT tyl tyr       = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalLTE tyl tyr      = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalGT tyl tyr       = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalGTE tyl tyr      = cmpNumTy Bool tyl tyr
    -- Equiality: TODO I think we said structural equality, but not sure.
    typeOfOps' RelationalEqual tyl tyr    = sameTyOne tyl tyr
    typeOfOps' RelationalNotEqual tyl tyr = sameTyOne tyl tyr
    -- Bitwise. I guess this is like C so nums?
    typeOfOps' BitwiseAnd tyl tyr         = cmpNumTy tyl tyl tyr
    typeOfOps' BitwiseOr  tyl tyr         = cmpNumTy tyl tyl tyr
    typeOfOps' BitwiseXor tyl tyr         = cmpNumTy tyl tyl tyr
    -- Logical And/Or bool
    typeOfOps' LogicalAnd tyl tyr         = justBoolTy tyl tyr
    typeOfOps' LogicalOr  tyl tyr         = justBoolTy tyl tyr
    sameTyOne t t' =
      if groundTyEq t t'
      then Left Bool
      else Right (t,t')
    cmpNumTy tres t t' =
      if groundTyEq t t'
      then Left tres
      else Right (t,t')
    justNumTy tres t t' =
      if numTy t && numTy t'
      then Left tres
      else Right (t,t')
    justBoolTy t t' =
      if boolTy t && boolTy t'
      then Left Bool
      else Right (t,t')

-- | Type assignment list of param expressions.
paramTy :: Parser.Annotation -> [Parameter] -> [Expression Parser.Annotation] -> SemanticMonad [SAST.Expression SemanticAnns]
paramTy _ann [] [] = return []
paramTy ann (p : ps) (a : as) =
  checkParamTy (paramTypeSpecifier p) a
  >>= \tyed_exp -> (tyed_exp :) <$> paramTy ann ps as
  where checkParamTy pTy expression = mustBeTy pTy =<< expressionType expression
paramTy ann (_p : _) [] = throwError $ annotateError ann EFunParams
paramTy ann [] (_a : _) = throwError $ annotateError ann EFunParams

objectType
  :: (Parser.Annotation -> Identifier -> SemanticMonad TypeSpecifier)
  -- ^ Scope of variables
  -> Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns)
objectType getVarTy (Variable ident ann) =
  SAST.Variable ident . buildExpAnn ann <$> getVarTy ann ident
objectType getVarTy (VectorIndexExpression obj idx ann) =
  typeObject getVarTy obj >>= \(obj_typed , obj_ty) ->
  case obj_ty of
    Vector ty_elems _vexp ->
      typeExpression idx >>= \(idx_typed , idx_ty) ->
        if numTy idx_ty
        then return $ SAST.VectorIndexExpression obj_typed idx_typed $ buildExpAnn ann ty_elems
        else throwError $ annotateError ann (ENumTs [idx_ty])
    ty -> throwError $ annotateError ann (EVector ty)
objectType getVarTy (MemberAccess obj ident ann) =
  --
  typeObject getVarTy obj  >>= \(obj_typed , obj_ty) ->
  -- Only complex types are the ones defined by the user
  case obj_ty of
    DefinedType dident -> getGlobalTy ann dident >>=
      \case{
        -- Either a struct
        Struct _identTy fields _mods ->
            let mfield = find ((ident ==) . fieldIdentifier) fields in
              maybe
              (throwError $ annotateError ann (EMemberAccessNotMember ident))
              (return . SAST.MemberAccess obj_typed ident . buildExpAnn ann . fieldTypeSpecifier)
              mfield
        ;
        -- Or a class
        Class _identTy cls _mods ->
          -- TODO Class access?
          -- Find |ident| field in the class.
          case findClassField ident cls of
            Nothing -> throwError $ annotateError ann (EMemberAccessNotMember ident)
            -- type |t| and the type inside |a| should be the same, no?
            Just (t , _a) -> return (SAST.MemberAccess obj_typed ident (buildExpAnn ann t))
        ;
        -- Other types do not have members.
        ty -> throwError $ annotateError ann (EMemberAccessUDef (fmap (fmap forgetSemAnn) ty))
      }
    ty -> throwError $ annotateError ann (EMemberAccess ty)
objectType getVarTy (Dereference obj ann) =
  typeObject getVarTy obj >>= \(obj_typed , obj_ty ) ->
  case obj_ty of
   Reference ty -> return $ SAST.Dereference obj_typed $ buildExpAnn ann ty
   ty           -> throwError $ annotateError ann $ ETypeNotReference ty
objectType getVarTy (ParensObject obj anns) =
  typeObject getVarTy obj >>= \(obj_typed, obj_ty) -> return $ SAST.ParensObject obj_typed $ buildExpAnn anns obj_ty

----------------------------------------
-- These two functions are useful, one lookups in read+write environments,
-- the other one in write+environments
rhsObject :: Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns, TypeSpecifier)
rhsObject = typeObject getRHSVarTy

lhsObject :: Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns, TypeSpecifier)
lhsObject = typeObject getLHSVarTy
----------------------------------------

typeObject
  :: (Parser.Annotation -> Identifier -> SemanticMonad TypeSpecifier)
  -> Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns, TypeSpecifier)
typeObject identTy =
  (\typed_o -> (typed_o , ) <$> getObjType typed_o) <=< objectType identTy
  where
    getObjType = maybe (throwError $ annotateError internalErrorSeman EUnboxingObjectExpr) return . getTySpec . ty_ann . getAnnotation

data BoxedExpression
  = Objects (SAST.Object SemanticAnns) TypeSpecifier
  | Expr (SAST.Expression SemanticAnns) TypeSpecifier

-- | Function |expressionType| takes an expression from the parser, traverse it
-- annotating each node with its type.
-- Since we are also creating new nodes (|Undyn| annotations), instead of just
-- traversing, we are actually /creating/ a new tree with implicit
-- constructions.
expressionType
  :: Expression Parser.Annotation
  -> SemanticMonad (SAST.Expression SemanticAnns)
-- Object access
expressionType (AccessObject obj)
  = AccessObject . fst <$> rhsObject obj
expressionType (Constant c pann) =
  -- | Constants
  SAST.Constant c . buildExpAnn pann <$>
  case c of
    -- Rules *(eConstTrue)* and *(eConstFalse)*
    B _b -> return Bool
    -- Typing constant integers
    I tyI i ->
      -- DONE Q8
      -- Source expression | z : tyI|
      if numTy tyI -- First we check if |tyI| is a numeric type.
      then -- Then we check that |z \subseteq [[ tyI ]]|
        checkIntConstant pann tyI i >>
        return tyI
      else throwError (annotateError pann $ ENumTs [tyI])
    -- Rule *(eConstChar)*
    C _c -> return Char
expressionType (Casting e nty pann) = do
  -- | Casting Expressions.
  typed_exp <- expressionType e
  type_exp <- getExpType typed_exp
  -- expressionType e >>= getExpType >>= \ety ->
  if casteableTys type_exp nty -- ety \subseteq nty
  then return (SAST.Casting typed_exp nty (buildExpAnn pann nty))
  else throwError (annotateError pann $ ECasteable type_exp nty)
expressionType (BinOp op le re pann) = do
  -- | Binary operation typings
  tyle' <- expressionType le
  type_le' <- getExpType tyle'
  tyre' <- expressionType re
  type_re' <- getExpType tyre'
  (tyle, type_le) <- maybe (return (tyle', type_le')) (\t -> (,t) <$> unDynExp tyle') (isDyn type_le')
  (tyre, type_re) <- maybe (return (tyre', type_re')) (\t -> (,t) <$> unDynExp tyre') (isDyn type_re')
  SAST.BinOp op tyle tyre <$> typeOfOps pann op type_le type_re
expressionType (ReferenceExpression rhs_e pann) =
  -- | Reference Expression
  -- TODO [Q15]
  rhsObject rhs_e >>= \(obj_typed, obj_type) ->
  return (SAST.ReferenceExpression obj_typed (buildExpAnn pann (Reference obj_type)))
-- Function call?
expressionType (FunctionExpression fun_name args pann) =
  -- | Function Expression.  A tradicional function call
  getFunctionTy pann fun_name >>= \(params, retty) ->
  flip (SAST.FunctionExpression fun_name) (buildExpAnn pann retty) <$> paramTy pann params args
expressionType (MemberMethodAccess obj ident args ann) =
  rhsObject obj  >>= \(obj_typed , obj_ty) ->
  case obj_ty of
    DefinedType dident -> getGlobalTy ann dident >>=
      \case{
         Class _identTy cls _mods ->
         case findClassMethod ident cls of
           Nothing -> throwError $ annotateError ann (EMemberAccessNotMethod ident)
           Just (ps, anns) ->
             let (psLen , asLen ) = (length ps, length args) in
             if psLen == asLen
             then SAST.MemberMethodAccess obj_typed ident
                 <$> zipWithM (\p e -> mustBeTy (paramTypeSpecifier p) =<< expressionType e) ps args
                 <*> maybe (throwError $ annotateError internalErrorSeman EMemberMethodType) (return . buildExpAnn ann) (getTypeSAnns anns)
             else if psLen < asLen
             then throwError $ annotateError ann EMemberMethodExtraParams
             else throwError $ annotateError ann EMemberMethodMissingParams
         ;
         -- Other User defined types do not define methods
         ty -> throwError $ annotateError ann (EMemberMethodUDef (fmap (fmap forgetSemAnn) ty))
      }
    Pool ty_pool _sz ->
      case ident of
        "alloc" -> case args of
                      [refM] ->
                        typeExpression refM >>= \(typed_ref , type_ref) ->
                        case type_ref of
                          (Reference (Option (DynamicSubtype tyref))) ->
                            if groundTyEq ty_pool tyref
                            then return $ SAST.MemberMethodAccess obj_typed ident [typed_ref] (buildExpAnn ann Unit)
                            else throwError $ annotateError ann (EPoolsWrongArgType tyref)
                          _ -> throwError $ annotateError ann (EPoolsWrongArgTypeW type_ref)
                      _ -> throwError $ annotateError ann EPoolsWrongNumArgs
        _ -> throwError $ annotateError ann (EPoolsMethods ident)
    ty -> throwError $ annotateError ann (EMethodAccessNotClass ty)
expressionType (FieldValuesAssignmentsExpression id_ty fs pann) =
  -- | Field Type
  catchError
    (getGlobalTy pann id_ty )
    (\_ -> throwError $ annotateError pann (ETyNotStructFound id_ty))
  >>= \case{
   Struct _ ty_fs _mods  ->
       flip (SAST.FieldValuesAssignmentsExpression id_ty)
            (buildExpAnn pann (DefinedType id_ty))
       <$>
       checkFieldValues pann ty_fs fs
       ;
   x -> throwError $ annotateError pann (ETyNotStruct id_ty (fmap (fmap forgetSemAnn) x));
  }
expressionType (EnumVariantExpression id_ty variant args pann) =
  -- | Enum Variant
  catchError
    (getGlobalTy pann id_ty)
    (\_ -> throwError $ annotateError pann (ETyNotEnumFound id_ty))
  >>= \case{
   Enum _ ty_vs _mods ->
     case find ((variant ==) . variantIdentifier) ty_vs of
       Nothing -> throwError $ annotateError pann (EEnumVariantNotFound variant)
       Just (EnumVariant _ ps) ->
         let (psLen , asLen ) = (length ps, length args) in
         if psLen == asLen
         then flip (SAST.EnumVariantExpression id_ty variant) (buildExpAnn pann (DefinedType id_ty))
             <$> zipWithM (\p e -> mustBeTy p =<< expressionType e) ps args
         else if psLen < asLen
         then throwError $ annotateError pann EEnumVariantExtraParams
         else throwError $ annotateError pann EEnumVariantMissingParams
    ;
   x -> throwError $ annotateError pann (ETyNotEnum id_ty (fmap (fmap forgetSemAnn) x))
  }
-- IDEA Q4
expressionType (VectorInitExpression iexp kexp@(KC constE) pann) = do
-- | Vector Initialization
  (typed_init , type_init) <- typeExpression iexp
  -- Check that the type is correct
  _ <- checkConstant pann constE
  return (SAST.VectorInitExpression typed_init kexp (buildExpAnn pann (Vector type_init kexp)))
expressionType (VectorInitExpression _ lexp pann) = throwError $ annotateError pann (EVectorConst lexp)
-- DONE [Q5]
-- TODO [Q17]
expressionType (ParensExpression e anns) =
  typeExpression e >>= \(typed_e, type_e) -> return $ ParensExpression typed_e $ buildExpAnn anns type_e
expressionType (OptionVariantExpression vexp anns) =
  case vexp of
    None -> return $ OptionVariantExpression None (buildExpAnn anns (Option Unit))
    Some e -> do
      (typed_e, type_e) <- typeExpression e
      case type_e of
          DynamicSubtype _ -> return $ SAST.OptionVariantExpression (Some typed_e) (buildExpAnn anns (Option type_e))
          _ -> throwError $ annotateError anns (EOptionVariantNotDynamic type_e)

-- Zipping list of same length
zipSameLength ::  ([b] -> e) -> ([a] -> e) -> (a -> b -> c) -> [a] -> [b] -> Either e [c]
zipSameLength = zipSameLength' []
  where
    -- Tail recursive version
    zipSameLength' :: [c] -> ([b] -> e) -> ([a] -> e) -> (a -> b -> c) -> [a] -> [b] -> Either e [c]
    zipSameLength' acc _ _ _ [] [] = Right acc
    zipSameLength' acc erra errb f (a : as) (b : bs) = zipSameLength' (f a b : acc) erra errb f as bs
    zipSameLength' _ erra _ _ [] bs = Left (erra bs)
    zipSameLength' _ _ errb _ as [] = Left (errb as)
--

checkFieldValue
  :: Parser.Annotation
  -> FieldDefinition
  -> FieldValueAssignment Parser.Annotation
  -> SemanticMonad (SAST.FieldValueAssignment SemanticAnns)
checkFieldValue loc (FieldDefinition fid fty) (FieldValueAssignment faid faexp) =
  if fid == faid
  then
    SAST.FieldValueAssignment faid <$> (expressionType faexp >>= mustBeTy fty)
  else throwError $ annotateError loc (EFieldMissing [fid])

checkFieldValues
  :: Parser.Annotation
  -> [FieldDefinition]
  -> [FieldValueAssignment Parser.Annotation]
  -> SemanticMonad [SAST.FieldValueAssignment SemanticAnns]
checkFieldValues loc fds fas = checkSortedFields sorted_fds sorted_fas []
  where
    tError = throwError . annotateError loc
    sorted_fds = sortOn fieldIdentifier fds
    sorted_fas = sortOn fieldAssigIdentifier fas
    -- Same length monadic Zipwith
    checkSortedFields [] [] xs = return $ reverse xs
    checkSortedFields [] es _ = tError (EFieldExtra (fmap fieldAssigIdentifier es))
    checkSortedFields ms [] _ = tError (EFieldMissing (fmap fieldIdentifier ms))
    checkSortedFields (d:ds) (a:as) acc =
      checkFieldValue loc d a >>= checkSortedFields ds as . (:acc)

retStmt :: ReturnStmt Parser.Annotation -> SemanticMonad (SAST.ReturnStmt SemanticAnns)
retStmt (ReturnStmt Nothing anns) = return $ SAST.ReturnStmt Nothing (buildExpAnn anns Unit)
retStmt (ReturnStmt (Just e) anns)
  = typeExpression e >>= \(typed_e, e_type) ->
  -- ReturnStmt (Just ety) . buildExpAnn anns <$> getExpType ety
  return $ ReturnStmt (Just typed_e) (buildExpAnn anns e_type)

retblockType :: BlockRet Parser.Annotation -> SemanticMonad (SAST.BlockRet SemanticAnns)
retblockType (BlockRet bbody rete) = BlockRet <$> blockType bbody <*> retStmt rete

blockType :: Block Parser.Annotation -> SemanticMonad (SAST.Block SemanticAnns)
blockType = mapM statementTySimple

-- | Type checking statements. We should do something about Break
-- Rules here are just environment control.
statementTySimple :: Statement Parser.Annotation -> SemanticMonad (SAST.Statement SemanticAnns)
-- Declaration semantic analysis
statementTySimple (Declaration lhs_id lhs_type expr anns) =
  -- Check type is alright
  checkTypeDefinition anns lhs_type >>
  -- Expression and type must match
  expressionType expr >>= mustBeTy lhs_type >>= \ety ->
  -- Insert variables in the local environment
  insertLocalVar anns lhs_id lhs_type >>
  -- Return annotated declaration
  return (Declaration lhs_id lhs_type ety (buildStmtAnn anns))
statementTySimple (AssignmentStmt lhs_o rhs_expr anns) = do
{- TODO Q19 && Q20 -}
  (lhs_o_typed', lhs_o_type') <- lhsObject lhs_o
  let (lhs_o_typed, lhs_o_type) = maybe (lhs_o_typed', lhs_o_type') (unDyn lhs_o_typed',) (isDyn lhs_o_type')
  rhs_expr_typed' <- expressionType rhs_expr
  type_rhs' <- getExpType rhs_expr_typed'
  rhs_expr_typed <- maybe (return rhs_expr_typed') (\_ -> unDynExp rhs_expr_typed') (isDyn type_rhs')
  ety <- mustBeTy lhs_o_type rhs_expr_typed
  return $ AssignmentStmt lhs_o_typed ety $ buildStmtAnn anns
statementTySimple (IfElseStmt cond_expr tt_branch elifs otherwise_branch anns) =
  IfElseStmt
    <$> (mustBeTy Bool =<< expressionType cond_expr)
    <*> localScope (blockType tt_branch)
    <*> mapM (\case {
                 ElseIf eCond eBd ann ->
                   ElseIf <$> (mustBeTy Bool =<< expressionType eCond)
                          <*> localScope (blockType eBd)
                          <*> return (buildStmtAnn ann)
                    }) elifs
    <*> localScope (blockType otherwise_branch)
    <*> return (buildStmtAnn anns)
-- Here we could implement some abstract interpretation analysis
statementTySimple (ForLoopStmt it_id from_expr to_expr mWhile body_stmt ann) = do
  (typed_fromexpr, from_ty) <- typeExpression from_expr
  (typed_toexpr, to_ty) <- typeExpression to_expr
  -- Both boundaries should have the same numeric type
  if sameNumTy from_ty to_ty
  then
    ForLoopStmt it_id typed_fromexpr typed_toexpr
      <$> (case mWhile of
                Nothing -> return Nothing
                Just whileC -> typeExpression whileC >>= \(typed_whileC , type_whileC) ->
                    if groundTyEq Bool type_whileC
                    then return (Just typed_whileC)
                    else throwError $ annotateError ann (EForWhileTy type_whileC)
          )
      <*> addTempVars ann [(it_id, from_ty)] (blockType body_stmt)
      <*> return (buildStmtAnn ann)
  else
    throwError $ annotateError ann EBadRange
statementTySimple (SingleExpStmt expr anns) =
  flip SingleExpStmt (buildStmtAnn anns) <$> expressionType expr
statementTySimple (MatchStmt matchE cases ann) =
  typeExpression matchE >>= \(typed_matchE , type_matchE) ->
  case type_matchE of
    DefinedType t -> getGlobalTy ann t >>=
        \case {
          Enum _ident flsDef _mods ->
          -- Sort both lists by identifiers
          let ord_flsDef = sortOn variantIdentifier flsDef in
          let ord_cases = sortOn matchIdentifier cases in
          case zipSameLength
                (const (annotateError ann EMatchExtraCases))
                (const (annotateError ann EMatchExtraCases))
                matchCaseType ord_cases ord_flsDef of
            Left e -> throwError e
            Right cs -> flip (MatchStmt typed_matchE) (buildStmtAnn ann) <$> sequence cs
          ;
          _ -> throwError $ annotateError ann $ EMatchNotEnum t
        }
    Option t ->
      let ord_cases = sortOn matchIdentifier cases in
      optionCases ord_cases >>= flip unless (throwError $  annotateError ann EMatchOptionBad)
      >>
      MatchStmt typed_matchE <$> zipWithM matchCaseType ord_cases [EnumVariant "None" [],EnumVariant "Some" [t]] <*> pure (buildStmtAnn ann)
    _ -> throwError $  annotateError ann $ EMatchWrongType type_matchE
    where
      optionCases [a,b] = return $ (optionNone a && optionSome b) || (optionSome a && optionNone b)
      optionCases _ = throwError $ annotateError ann EMatchOptionBadArgs
      optionNone c =
        matchIdentifier c == "None"
          && Prelude.null (matchBVars c)
      optionSome c =
        matchIdentifier c == "Some"
           && length (matchBVars c) == 1

matchCaseType :: MatchCase Parser.Annotation -> EnumVariant -> SemanticMonad (SAST.MatchCase SemanticAnns)
matchCaseType c (EnumVariant vId vData) = matchCaseType' c vId vData
  where
    matchCaseType' (MatchCase cIdent bVars bd ann) supIdent tVars
      | cIdent == supIdent =
        if length bVars == length tVars then
        flip (SAST.MatchCase cIdent bVars) (buildStmtAnn ann) <$> addTempVars ann (zip bVars tVars) (blockType bd)
        else throwError $ annotateError internalErrorSeman EMatchCaseInternalError
      | otherwise = throwError $ annotateError internalErrorSeman $ EMatchCaseBadName cIdent supIdent

----------------------------------------
-- Programs Semantic Analyzer
-- For now all are kinda the same thing but eventually they should not :shrug:
----------------------------------------

-- Keeping only type information
-- TODO Check ident is not defined?
globalCheck :: Global Parser.Annotation -> SemanticMonad (SAST.Global SemanticAnns)
globalCheck (Volatile ident ty addr mods anns) =
  checkTypeDefinition anns ty >>
  -- Check TypeSpecifier is correct
  return (Volatile ident ty addr mods (buildGlobalAnn anns (SVolatile ty)))
-- DONE [Q13]
globalCheck (Static ident ty (Just sexp@(Constant _address _a)) mods anns) =
  checkTypeDefinition anns ty >>
  expressionType sexp >>= \sexp_ty ->
  return (Static ident ty (Just sexp_ty) mods (buildGlobalAnn anns (SStatic ty)))
globalCheck (Static _ _ _ _ anns) = throwError $ annotateError anns EStaticK
--
globalCheck (Shared ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> (mustBeTy ty =<< expressionType expr)
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (Shared ident ty exprty mods (buildGlobalAnn anns (SShared ty)))
-- TODO [Q14]
globalCheck (Const ident ty expr mods anns) =
  checkTypeDefinition anns ty >>
  Const ident ty
  <$> (mustBeTy ty =<< expressionType expr)
  <*> pure mods
  <*> pure (buildGlobalAnn anns (SConst ty))

-- Here we actually only need Global
programSeman :: AnnASTElement Parser.Annotation -> SemanticMonad (SAST.AnnASTElement SemanticAnns)
programSeman (Task ident ps ty bret mods anns) =
  checkTypeDefinition anns ty >>
  (Task ident ps ty
  <$> -- | Add
    (addTempVars anns
    -- | list of variables
      (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) ps)
    -- | analyze the body and check the returining type
      (retblockType bret >>= \bret' ->
        blockRetTy ty bret' >> return bret'
         ))
  <*> pure mods
  <*> pure (buildGlobal anns (GTask ps ty)))
programSeman (Function ident ps mty bret mods anns) =
  maybe (return ()) (checkTypeDefinition anns) mty >>
  (Function ident ps mty
  <$> (addTempVars anns
          (fmap (\ p -> (paramIdentifier p , paramTypeSpecifier p)) ps)
          (retblockType bret) >>= \ typed_bret ->
    (maybe
      -- | Procedure
      (blockRetTy Unit)
      -- | Function
      blockRetTy
      mty) typed_bret >> return typed_bret
      )
  <*> pure mods
  <*> pure (buildGlobal anns (GFun ps (fromMaybe Unit mty))))
programSeman (Handler ident ps ty bret mods anns) =
  checkTypeDefinition anns ty >>
  Handler ident ps ty
  <$> (addTempVars anns (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) ps)
                  (retblockType bret) >>= \ typed_bret ->
            blockRetTy ty typed_bret >>
            return typed_bret
      )
  <*> pure mods
  <*> pure (buildGlobal anns (GHand ps ty))
programSeman (GlobalDeclaration gbl) =
  -- TODO Add global declarations
  GlobalDeclaration <$> globalCheck gbl
programSeman (TypeDefinition tydef ann) =
  typeDefCheck ann tydef >>= \t ->
    -- let stdef = semanticTypeDef t in
    -- and we can add it to the global environment.
    -- insertGlobalTy ann stdef >>
    return (TypeDefinition t (buildGlobalTy ann (semanticTypeDef t)))
programSeman (ModuleInclusion _ident _mods _anns) = undefined

semanticTypeDef :: SAST.TypeDef SemanticAnns -> SemanTypeDef SemanticAnns
semanticTypeDef (Struct i f m)  = Struct i f m
semanticTypeDef (Union i f m)   = Union i f m
semanticTypeDef (Enum i e m)    = Enum i e m
semanticTypeDef (Class i cls m) = Class i (Data.List.map kClassMember cls) m

typeExpression :: Expression Locations -> SemanticMonad (SAST.Expression SemanticAnns , TypeSpecifier)
typeExpression e = expressionType e >>= \typed_e -> (typed_e, ) <$> getExpType typed_e

-- | Function checking that constant expressions are correct.
-- Here we have that syntact constant expressions are quite right, but we want
-- to check that integers are correct.
checkConstant :: Locations -> Const -> SemanticMonad Const
checkConstant loc t@(I type_c c) =
  -- |type_c| is correct
  checkTypeDefinition loc type_c >>
  checkIntConstant loc type_c c >>
  return t
checkConstant _ t = pure t

checkIntConstant :: Locations -> TypeSpecifier -> Integer -> SemanticMonad ()
checkIntConstant loc tyI i =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I tyI i))

-- Type definition
-- Here I am traversing lists serveral times, I prefer to be clear than
-- proficient for the time being.
typeDefCheck :: Locations -> TypeDef Locations -> SemanticMonad (SAST.TypeDef SemanticAnns)
-- Check Type definitions https://hackmd.io/@termina-lang/SkglB0mq3#Struct-definitions
typeDefCheck ann (Struct ident fs mds)
  -- Check every type is well-defined
  = when (Prelude.null fs) (throwError $ annotateError ann (EStructDefEmptyStruct ident))
  >> mapM_ (fieldDefinitionTy ann) fs
  -- TODO mods?
  -- Check names are unique
  >> checkUniqueNames ann EStructDefNotUniqueField (Data.List.map fieldIdentifier fs)
  -- Return same struct
  >> return (Struct ident fs mds)
typeDefCheck ann (Union ident fs mds )
  = when (Prelude.null fs) (throwError $ annotateError ann (EUnionDefEmptyUnion ident))
  >> mapM_ (fieldDefinitionTy ann) fs
  -- TODO mods?
  -- Check names are unique
  >> checkUniqueNames ann EUnionDefNotUniqueField (Data.List.map fieldIdentifier fs)
  -- Return same struct
  >> return (Union ident fs mds)
typeDefCheck ann (Enum ident evs mds)
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Enumeration-definitions
  = when (Prelude.null evs) (throwError $ annotateError ann (EEnumDefEmpty ident))
  -- Type checking
  >> mapM_ (enumDefinitionTy ann) evs
  -- Check names are unique.
  >> checkUniqueNames ann EEnumDefNotUniqueField (Data.List.map variantIdentifier evs)
  -- Return the same definition.
  >> return (Enum ident evs mds)
typeDefCheck ann (Class ident cls mds)
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Classes
  -- check that it defines at least one method.
  = when (emptyClass cls) (throwError $ annotateError ann (EClassEmptyMethods ident))
  -- TODO loop free
  -- Split definitions in tree, fields, no self reference and self reference.
  -- plus check that all types are well define (see |classTypeChecking|)
  >> foldM
    (\(fs, nsl, sl) cl ->
       case cl of
         ClassField fs_id fs_ty annCF
           -> checkTypeDefinition annCF fs_ty
           >> simpleTyorFail annCF fs_ty
           >> let checkFs = ClassField fs_id fs_ty (buildExpAnn annCF fs_ty)
              in return (checkFs : fs, nsl ,sl )
         nslm@(ClassMethod _fm_id fm_tys NoSelf _body annCM)
           -> mapM_ (checkTypeDefinition annCM . paramTypeSpecifier) fm_tys
           >> return (fs, nslm : nsl, sl)
         slm@(ClassMethod _fm_id fm_tys Self _body annCM)
           ->  mapM_ (checkTypeDefinition annCM . paramTypeSpecifier) fm_tys
           >> return (fs, nsl, slm : sl )
        )
    ([],[],[]) cls
  >>= \(fls  -- Fields do not need type checking :shrug:
       , nsl -- NoSelf methods do not depend on the other ones.
       , sl  -- Self methods can induce undefined behaviour
       -- introduce a semi-well formed type.
       )->
  do
  -- Now we can go method by method checking everything is well typed.
  -- No Self
    nslChecked <- mapM (\case
             -- Filtered cases
             ClassField {} -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
             ClassMethod _ _ Self _ _ -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
             -- Interesting case
             ClassMethod identCM ps NoSelf body annCM ->
               localScope (
               -- Insert params types
                 insertLocalVariables annCM (Data.List.map (\p -> (paramIdentifier p, paramTypeSpecifier p)) ps)
                 >>
              -- Type check body
                 flip (ClassMethod identCM ps NoSelf) (buildExpAnn annCM Unit) <$> blockType body
                 )
         ) nsl
  -- Methos with Self references.
  -- Get depndencies
    let dependencies = Data.List.map (\case{
                                         -- This shouldn't happen here but I doesn't add anything
                                         l@(ClassField identCF _ _) -> (l, identCF, []);
                                         -- This is the interesting one.
                                         l@(ClassMethod identCM _ps _self bs _ann) ->
                                           (l, identCM
                                           -- This is weird, can we have "self.f1(53).f2"
                                           , concatMap (\case{ ("self", [ ids ]) -> [ ids ];
                                                               a -> error ("In case the impossible happens >>> " ++ show a);
                                                              }
                                                         . depToList
                                                       )
                                             (getDepBlock bs) )
                                         ;
                                     }) sl
  -- Build dependency graph and functions from internal Node representations to ClassMembers
    let (selfMethodDepGraph, vertexF , _KeyF) = Graph.graphFromEdges dependencies
    let vertexToIdent = (\(n,_,_) -> n) . vertexF
  -- Generate a solution with possible loops.
    let topSortOrder = Data.List.map vertexToIdent $ Graph.topSort selfMethodDepGraph
  -- Type check in order, if a method is missing is because there is a loop.
    slChecked <-
      foldM (\prevMembers newMember ->
          -- Intermediate Class type only containing fields, no self methods and
          -- previous (following the topsort order) to the current method.
          let clsType = Class ident
                              -- Function |kClassMember| /erases/ body of methods.
                              -- When typing, we do not need them
                              (Data.List.map kClassMember (fls ++ nslChecked ++ prevMembers))
                              mds  in
          localScope $
          insertGlobalTy ann  clsType >>
          insertLocalVar ann "self" (Reference (DefinedType ident)) >>
          -- Now analyze new member.
          case newMember of
            -- Filtered Cases
             ClassField {} -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
             ClassMethod _ _ NoSelf _ _ -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
            -- Interesting case
             ClassMethod mIdent mps Self mbody mann ->
               -- Insert method arguments as local variables.
               insertLocalVariables
                    mann
                    (Data.List.map (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps)
               -- Type check body and build the class method back.
               >>
              (:prevMembers) . flip (ClassMethod mIdent mps Self) (buildExpAnn mann Unit) <$> blockType mbody
      ) [] topSortOrder
    return (Class ident (fls ++ nslChecked ++ slChecked) mds)

----------------------------------------
-- Field definition helpers.
fieldDefinitionTy :: Locations -> FieldDefinition -> SemanticMonad ()
fieldDefinitionTy ann f
  -- First we check its type is well-defined
  = checkTypeDefinition ann tyFD
  -- and that it is simply (see simple types).
  >> simpleTyorFail ann tyFD
  -- we just return it.
  where
    tyFD = fieldTypeSpecifier f

-- Enum Variant definition helpers.
enumDefinitionTy :: Locations -> EnumVariant -> SemanticMonad ()
enumDefinitionTy ann ev
  = mapM_ (\ty -> checkTypeDefinition ann ty >> simpleTyorFail ann ty) ev_tys
  where
    ev_tys = assocData ev

----------------------------------------
-- Class Definition Helpers

-- Empty Class Methods
emptyClass :: [ ClassMember' expr lha a ] -> Bool
emptyClass = Data.List.foldl' (\ b x -> case x of { ClassField {} -> b; ClassMethod {} -> False}) True

classDependencies
  :: PAST.ClassMember a
  -> (Identifier, [ClassDep])
classDependencies (ClassField fs_id _ty _ann)
  -- Definition does not depends on anything.
  = (fs_id , [])
classDependencies (ClassMethod fm_id _ NoSelf _body _ann)
  -- No Self methods do not generate dependencies within the same class
  = (fm_id , [])
classDependencies (ClassMethod fm_id _ Self bs _ann)
  -- We need to get all invocations to other methods in self.
  = (fm_id, getDepBlock bs)

----------------------------------------
checkUniqueNames :: Locations -> ([Identifier] -> Errors Locations) -> [Identifier] -> SemanticMonad ()
checkUniqueNames ann err is =
  if allUnique is then return () else throwError $ annotateError ann (err (repeated is))
-----------------------------------------
-- TODO Improve this two functions.
-- nub is O(n^2)
allUnique :: Eq a => [a] -> Bool
allUnique xs = nub xs == xs

repeated :: Eq a => [a] -> [a]
repeated xs = nub $ xs Data.List.\\ nub xs
-----------------------------------------

-- Adding Global elements to the environment.
programAdd :: SAST.AnnASTElement SemanticAnns -> SemanticMonad ()
programAdd (Task ident args retType _bd _mods anns) =
  insertGlobal ident (GTask args retType)
  (annotateError (location anns) (EUsedTaskName ident))
programAdd (Function ident args mretType _bd _mods anns) =
  insertGlobal ident (GFun args (fromMaybe Unit mretType))
  (annotateError (location anns) (EUsedFunName ident))
programAdd (Handler ident ps ty _bd _mods anns) =
  insertGlobal ident (GHand ps ty)
  (annotateError (location anns) (EUsedHandlerName ident))
programAdd (GlobalDeclaration glb) =
  let (global_name, sem) =
        case glb of
          Volatile ident type_spec _me _mod _ann ->
            (ident, SVolatile type_spec)
          Static ident type_spec _me _mod _ann ->
            (ident, SStatic type_spec)
          Shared ident type_spec _me _mod _ann ->
            (ident, SShared type_spec)
          Const ident type_spec _e _mod _ann ->
            (ident, SConst type_spec)
          in
  insertGlobal global_name (GGlob sem)
  (annotateError (location (getAnnotation glb)) (EUsedGlobalName global_name))
programAdd (TypeDefinition ty anns) =
  let type_name = identifierType ty in
    case ty_ann anns of
      GTy semTy@(GType _) ->
        insertGlobal
          type_name semTy
          (annotateError (location anns) $ EUsedTypeName type_name)
      _ -> throwError (annotateError internalErrorSeman EInternalNoGTY)
programAdd (ModuleInclusion {}) = error "TODO"

--- Exectuing Type Checking
typeCheckRunE :: PAST.AnnotatedProgram Parser.Annotation
  -> (Either SemanticErrors (SAST.AnnotatedProgram SemanticAnns) , ExpressionState)
typeCheckRunE = runTypeChecking initialExpressionSt  . mapM checkAndAdd
    where
      checkAndAdd t = programSeman t >>= \t' -> programAdd t' >> return t'

typeCheckRun :: PAST.AnnotatedProgram Parser.Annotation
  -> Either SemanticErrors (SAST.AnnotatedProgram SemanticAnns)
typeCheckRun = fst . typeCheckRunE
