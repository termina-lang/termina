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
import           AST.Parser                  as PAST
import           Utils.AST.Parser
import           Utils.AST.Core
import           Utils.TypeSpecifier

-- Termina Semantic AST
import qualified AST.Seman as SAST

-- Just annotations from Parser
import qualified Parser.Parsing              as Parser (Annotation (..))

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

import           Data.List            (find, map, nub, sortOn, (\\))
import           Data.Maybe

-- import Control.Monad.State as ST
import           Control.Monad

import qualified Data.Graph           as Graph

type SemanticPass t = t Parser.Annotation -> SemanticMonad (t SemanticAnns)

----------------------------------------
-- Function type-checking binary operations.
-- It returns resulting type of using an operation.
binOpType :: Locations -> PAST.Op -> PAST.TypeSpecifier -> PAST.TypeSpecifier -> SemanticMonad SemanticAnns
binOpType locs op lty rty =
  either
  -- | Check if there was an error, if not, returns the first type.
  (return . SemAnn locs . ETy . SimpleType)
  (throwError . annotateError locs . uncurry (EOpMismatch op))
  $ binOpType' op lty rty
  where
    binOpType' :: Op -> TypeSpecifier -> TypeSpecifier -> Either TypeSpecifier (TypeSpecifier,TypeSpecifier)
    -- Alg ops Same numeric type
    binOpType' Multiplication tyl tyr     = cmpNumTy tyl tyl tyr
    binOpType' Division tyl tyr           = cmpNumTy tyl tyl tyr
    binOpType' Addition tyl tyr           = cmpNumTy tyl tyl tyr
    binOpType' Subtraction tyl tyr        = cmpNumTy tyl tyl tyr
    -- shifts both numeric but may not be the same
    -- Q2
    binOpType' BitwiseLeftShift tyl tyr   = justNumTy tyl tyl tyr
    binOpType' BitwiseRightShift tyl tyr  = justNumTy tyl tyl tyr
    -- >, =>, <, <= : some numeric type.
    binOpType' RelationalLT tyl tyr       = cmpNumTy Bool tyl tyr
    binOpType' RelationalLTE tyl tyr      = cmpNumTy Bool tyl tyr
    binOpType' RelationalGT tyl tyr       = cmpNumTy Bool tyl tyr
    binOpType' RelationalGTE tyl tyr      = cmpNumTy Bool tyl tyr
    -- Equiality: TODO I think we said structural equality, but not sure.
    binOpType' RelationalEqual tyl tyr    = sameTyOne tyl tyr
    binOpType' RelationalNotEqual tyl tyr = sameTyOne tyl tyr
    -- Bitwise. I guess this is like C so nums?
    binOpType' BitwiseAnd tyl tyr         = cmpNumTy tyl tyl tyr
    binOpType' BitwiseOr  tyl tyr         = cmpNumTy tyl tyl tyr
    binOpType' BitwiseXor tyl tyr         = cmpNumTy tyl tyl tyr
    -- Logical And/Or bool
    binOpType' LogicalAnd tyl tyr         = justBoolTy tyl tyr
    binOpType' LogicalOr  tyl tyr         = justBoolTy tyl tyr
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
-- This function performs the following operations:
-- - It types the expressions that make up the call argument list of a function.
-- - It checks that the type of each of the arguments matches the type set in the function definition.
paramType ::
  -- | Annotation of the function call
  Parser.Annotation
  -- | List of parameters
  -> [Parameter]
  -- | List of arguments of the function call
  -> [Expression Parser.Annotation]
  -> SemanticMonad [SAST.Expression SemanticAnns]
paramType _ann [] [] = return []
paramType ann (p : ps) (a : as) =
  checkParamTy (paramTypeSpecifier p) a
  >>= \tyed_exp -> (tyed_exp :) <$> paramType ann ps as
  where checkParamTy pTy expression = mustBeTy pTy =<< expressionType expression
paramType ann (_p : _) [] = throwError $ annotateError ann EFunParams
paramType ann [] (_a : _) = throwError $ annotateError ann EFunParams

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an error.
unboxObjectSAnns :: SAST.Object SemanticAnns -> SemanticMonad (AccessKind, TypeSpecifier)
unboxObjectSAnns = maybe (throwError $ annotateError internalErrorSeman EUnboxingObjectExpr) return . getObjectSAnns . getAnnotation

objectType ::
  -- | Scope of variables. It returns its access kind (mutable, immutable or private) and its type
  (Parser.Annotation -> Identifier -> SemanticMonad (AccessKind, TypeSpecifier))
  -- The object to type
  -> Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns)
objectType getVarTy (Variable ident ann) =
  SAST.Variable ident . uncurry (buildExpAnnObj ann) <$> getVarTy ann ident
objectType getVarTy (VectorIndexExpression obj idx ann) = do
  typed_obj <- objectType getVarTy obj
  (obj_ak, obj_ty) <- unboxObjectSAnns typed_obj
  case obj_ty of
    Vector ty_elems _vexp ->
      typeExpression idx >>= \(idx_typed , idx_ty) ->
        if numTy idx_ty
        then return $ SAST.VectorIndexExpression typed_obj idx_typed $ buildExpAnnObj ann obj_ak ty_elems
        else throwError $ annotateError ann (ENumTs [idx_ty])
    ty -> throwError $ annotateError ann (EVector ty)
objectType _ (MemberAccess obj ident ann) = do
  -- | Attention on deck!
  -- This is a temporary solution pending confirmation that it works in all cases. 
  -- Problem: you cannot access the fields of a global object, you can only access
  -- the procedures in the case of shared resources. To avoid accessing the fields
  -- of a global object, we have adopted the following solution: when accessing the
  -- field of an object, only the local objects are available, not the global ones. 
  -- This way, only the fields of objects that are in the local environment of the
  -- function can be accessed.
  typed_obj <- objectType getLocalObjTy obj
  (obj_ak , obj_ty) <- unboxObjectSAnns typed_obj
  -- Only complex types are the ones defined by the user
  case obj_ty of
    DefinedType dident -> getGlobalTy ann dident >>=
      \case{
        -- Either a struct
        Struct _identTy fields _mods ->
            let mfield = find ((ident ==) . fieldIdentifier) fields in
              maybe
              (throwError $ annotateError ann (EMemberAccessNotMember ident))
              (return . SAST.MemberAccess typed_obj ident . buildExpAnnObj ann obj_ak . fieldTypeSpecifier) mfield
        ;
        -- Or a class
        Class _ _identTy cls _mods ->
          -- TODO Class access?
          -- Find |ident| field in the class.
          case findClassField ident cls of
            Nothing -> throwError $ annotateError ann (EMemberAccessNotMember ident)
            -- type |t| and the type inside |a| should be the same, no?
            Just (t , _a) -> return (SAST.MemberAccess typed_obj ident (buildExpAnn ann t))
        ;
        -- Other types do not have members.
        ty -> throwError $ annotateError ann (EMemberAccessUDef (fmap (fmap forgetSemAnn) ty))
      }
    ty -> throwError $ annotateError ann (EMemberAccess ty)
objectType getVarTy (Dereference obj ann) = do
  typed_obj <- objectType getVarTy obj
  (_, obj_ty) <- unboxObjectSAnns typed_obj
  case obj_ty of
    Reference ak ty -> return $ SAST.Dereference typed_obj $ buildExpAnnObj ann ak ty
    ty              -> throwError $ annotateError ann $ ETypeNotReference ty
objectType getVarTy (VectorSliceExpression obj lower upper anns) = do
  typed_obj <- objectType getVarTy obj
  (obj_ak, obj_ty) <- unboxObjectSAnns typed_obj
  unless (numConstExpression lower) (throwError (annotateError anns (ELowerBoundConst lower)))
  unless (numConstExpression upper) (throwError (annotateError anns (EUpperBoundConst upper)))
  case obj_ty of
    Vector ty_elems (KC (I sizeTy size)) ->
      case (lower, upper) of
        (KC (I lwTy lowerInt), KC (I upTy upperInt)) -> do
          unless (groundTyEq lwTy upTy) (throwError $ annotateError anns (EBoundsTypeMismatch lwTy upTy))
          unless (groundTyEq sizeTy lwTy) (throwError $ annotateError anns (EBoundsAndVectorTypeMismatch lwTy upTy))
          when (lowerInt > upperInt) (throwError $ annotateError anns (EBoundsLowerGTUpper lowerInt upperInt))
          when (upperInt > size) (throwError $ annotateError anns (EUpperBoundGTSize upperInt size))
          return $ SAST.VectorSliceExpression typed_obj lower upper $ buildExpAnnObj anns obj_ak (Vector ty_elems (KC (I lwTy (upperInt - lowerInt))))
        _ -> error "This should not happen, we already checked that the bounds are constant integers."
    ty -> throwError $ annotateError anns (EVector ty)
objectType getVarTy (DereferenceMemberAccess obj ident ann) = do
  typed_obj <- objectType getVarTy obj
  (_, obj_ty) <- unboxObjectSAnns typed_obj
  case obj_ty of
    Reference ak refTy ->
      case refTy of
        DefinedType dident -> getGlobalTy ann dident >>=
          \case{
            -- Either a struct
            Struct _identTy fields _mods ->
                let mfield = find ((ident ==) . fieldIdentifier) fields in
                  maybe
                  (throwError $ annotateError ann (EMemberAccessNotMember ident))
                  (return . SAST.DereferenceMemberAccess typed_obj ident . buildExpAnnObj ann ak . fieldTypeSpecifier) mfield
            ;
            -- Or a class
            Class _ _identTy cls _mods ->
              -- TODO Class access?
              -- Find |ident| field in the class.
              case findClassField ident cls of
                Nothing -> throwError $ annotateError ann (EMemberAccessNotMember ident)
                -- type |t| and the type inside |a| should be the same, no?
                Just (t , _a) -> return (SAST.DereferenceMemberAccess typed_obj ident (buildExpAnnObj ann ak t))
            ;
            -- Other types do not have members.
            ty -> throwError $ annotateError ann (EMemberAccessUDef (fmap (fmap forgetSemAnn) ty))
          }
        ty -> throwError $ annotateError ann (EMemberAccess ty)
    ty -> throwError $ annotateError ann $ ETypeNotReference ty

----------------------------------------
-- These two functions are useful, one lookups in read+write environments,
-- the other one in write+environments
rhsObjectType :: Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns)
rhsObjectType = objectType getRHSVarTy

lhsObjectType :: Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns)
lhsObjectType = objectType getLHSVarTy
----------------------------------------

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
  = AccessObject <$> rhsObjectType obj
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
  SAST.BinOp op tyle tyre <$> binOpType pann op type_le type_re
expressionType (ReferenceExpression refKind rhs_e pann) = do
  -- | Reference Expression
  -- TODO [Q15]
  typed_obj <- rhsObjectType rhs_e
  (_, obj_type) <- unboxObjectSAnns typed_obj
  return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (Reference refKind obj_type)))
-- Function call?
expressionType (FunctionExpression fun_name args pann) =
  -- | Function Expression.  A tradicional function call
  getFunctionTy pann fun_name >>= \(params, retty) ->
  flip (SAST.FunctionExpression fun_name) (buildExpAnnApp pann params retty)
  <$> paramType pann params args
----------------------------------------
expressionType (MemberFunctionAccess obj ident args ann) = do
  obj_typed <- rhsObjectType obj
  (_, obj_ty) <- unboxObjectSAnns obj_typed
  case obj_ty of
    DefinedType dident -> getGlobalTy ann dident >>=
      \case{
         Class _ _identTy cls _mods ->
         case findClassProcedure ident cls of
           Nothing -> throwError $ annotateError ann (EMemberAccessNotMethod ident)
           Just (ps, anns) ->
             let (psLen , asLen ) = (length ps, length args) in
             if psLen == asLen
             then SAST.MemberFunctionAccess obj_typed ident
                 <$> zipWithM (\p e -> mustBeTy (paramTypeSpecifier p) =<< expressionType e) ps args
                 <*> maybe (throwError $ annotateError internalErrorSeman EMemberMethodType) (return . buildExpAnnApp ann ps) (getTypeSAnns anns)
             else if psLen < asLen
             then throwError $ annotateError ann EMemberMethodExtraParams
             else throwError $ annotateError ann EMemberMethodMissingParams
         ;
         -- Other User defined types do not define methods
         ty -> throwError $ annotateError ann (EMemberMethodUDef (fmap (fmap forgetSemAnn) ty))
      }
    Pool ty_pool _sz ->
      case ident of
        "alloc" ->
          case args of
            [refM] ->
              typeExpression refM >>= \(typed_ref , type_ref) ->
                            case type_ref of
                              (Reference _ (Option (DynamicSubtype tyref))) ->
                                if groundTyEq ty_pool tyref
                                then return $ SAST.MemberFunctionAccess obj_typed ident [typed_ref] (buildExpAnnApp ann [Parameter "item" type_ref] Unit)
                                else throwError $ annotateError ann (EPoolsWrongArgType tyref)
                              _ -> throwError $ annotateError ann (EPoolsWrongArgTypeW type_ref)
            _ -> throwError $ annotateError ann EPoolsWrongNumArgs
        _ -> throwError $ annotateError ann (EPoolsWrongProcedure ident)
    MsgQueue ty _size ->
      case ident of
        "send" ->
          case args of
            [arg] -> typeExpression arg >>= \(arg_typed, arg_type) ->
              case isDyn arg_type of
                Just t ->
                  if groundTyEq t ty
                  then return $ MemberFunctionAccess obj_typed ident [arg_typed] (buildExpAnn ann Unit)
                  else throwError $ annotateError ann $ EMsgQueueWrongType t ty
                _ -> throwError $ annotateError ann $ EMsgQueueSendArgNotDyn arg_type
            _ -> throwError $ annotateError ann ENoMsgQueueSendWrongArgs
        "receive" ->
          case args of
            [arg] -> typeExpression arg >>= \(arg_typed, arg_type) ->
              case arg_type of
                -- & Option<'dyn T>
                Reference _ (Option (DynamicSubtype t)) ->
                  if groundTyEq t ty
                  then return $ MemberFunctionAccess obj_typed ident [arg_typed] (buildExpAnn ann Unit)
                  else throwError $ annotateError ann $ EMsgQueueWrongType t ty
                _ -> throwError $ annotateError ann $ EMsgQueueRcvWrongArgTy arg_type
            _ -> throwError $ annotateError ann ENoMsgQueueRcvWrongArgs
        _ -> throwError $ annotateError ann $ EMsgQueueWrongProcedure ident
    ty -> throwError $ annotateError ann (EFunctionAccessNotResource ty)
----------------------------------------
expressionType (FieldAssignmentsExpression id_ty fs pann) =
  -- | Field Type
  catchError
    (getGlobalTy pann id_ty )
    (\_ -> throwError $ annotateError pann (ETyNotStructFound id_ty))
  >>= \case{
   Struct _ ty_fs _mods  ->
       flip (SAST.FieldAssignmentsExpression id_ty)
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
  -> FieldAssignment Parser.Annotation
  -> SemanticMonad (SAST.FieldAssignment SemanticAnns)
checkFieldValue loc (FieldDefinition fid fty) (FieldValueAssignment faid faexp) =
  if fid == faid
  then
    SAST.FieldValueAssignment faid <$> (expressionType faexp >>= mustBeTy fty)
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc (FieldDefinition fid fty) (FieldAddressAssignment faid addr) =
  if fid == faid
  then
    case fty of 
      Location _ -> return $ SAST.FieldAddressAssignment faid addr
      _ -> throwError $ annotateError loc (EFieldAddress fid)
  else throwError $ annotateError loc (EFieldMissing [fid])

checkFieldValues
  :: Parser.Annotation
  -> [FieldDefinition]
  -> [FieldAssignment Parser.Annotation]
  -> SemanticMonad [SAST.FieldAssignment SemanticAnns]
checkFieldValues loc fds fas = checkSortedFields sorted_fds sorted_fas []
  where
    tError = throwError . annotateError loc
    getFid = \case {
      FieldValueAssignment fid _ -> fid;
      FieldAddressAssignment fid _ -> fid;
    }
    sorted_fds = sortOn fieldIdentifier fds
    sorted_fas = sortOn getFid fas
    -- Same length monadic Zipwith
    checkSortedFields [] [] xs = return $ reverse xs
    checkSortedFields [] es _ = tError (EFieldExtra (fmap getFid es))
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
-- Free statement semantic
statementTySimple (Free obj anns) = do
  obj_typed <- rhsObjectType obj
  (_, type_obj) <- unboxObjectSAnns obj_typed
  if isJust (isDyn type_obj)
  then return (Free obj_typed (buildStmtAnn anns))
  else throwError (annotateError anns (EFreeNotDyn type_obj))
-- Declaration semantic analysis
statementTySimple (Declaration lhs_id lhs_ak lhs_type expr anns) =
  -- Check type is alright
  checkTypeDefinition anns lhs_type >>
  -- Expression and type must match
  expressionType expr >>= mustBeTy lhs_type >>= 
    \ety ->
  -- Insert object in the corresponding environment
  -- If the object is mutable, then we insert it in the local mutable environment
  -- otherwise we insert it in the read-only environment
    (case lhs_ak of
      Mutable -> insertLocalMutObj anns lhs_id lhs_type
      Immutable -> insertROImmObj anns lhs_id lhs_type
      -- | This should not happen since the parser can only generate declarations
      -- of mutable and immutable objects.
      Private -> throwError $ annotateError internalErrorSeman EUnboxingObjectExpr) >>
  -- Return annotated declaration
  return (Declaration lhs_id lhs_ak lhs_type ety (buildStmtAnn anns))
statementTySimple (AssignmentStmt lhs_o rhs_expr anns) = do
{- TODO Q19 && Q20 -}
  lhs_o_typed' <- lhsObjectType lhs_o
  (_, lhs_o_type') <- unboxObjectSAnns lhs_o_typed'
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
statementTySimple (ForLoopStmt it_id it_ty from_expr to_expr mWhile body_stmt anns) = do
  -- Check the iterator is of numeric type
  unless (numTy it_ty) (throwError $ annotateError anns (EForIteratorWrongType it_ty))
  -- Both boundaries should have the same numeric type
  (typed_fromexpr, from_ty) <- typeExpression from_expr
  (typed_toexpr, to_ty) <- typeExpression to_expr
  -- If the from and to expressions are not numeric, and of the same type of the
  -- iterator, then we must throw an error
  unless (sameNumTy it_ty from_ty) (throwError $ annotateError anns EBadRange)
  unless (sameNumTy it_ty to_ty) (throwError $ annotateError anns EBadRange)
  ForLoopStmt it_id it_ty typed_fromexpr typed_toexpr
    <$> (case mWhile of
              Nothing -> return Nothing
              Just whileC -> typeExpression whileC >>= \(typed_whileC , type_whileC) ->
                  if groundTyEq Bool type_whileC
                  then return (Just typed_whileC)
                  else throwError $ annotateError anns (EForWhileTy type_whileC)
        )
    <*> addLocalMutObjs anns [(it_id, it_ty)] (blockType body_stmt)
    <*> return (buildStmtAnn anns)
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
        flip (SAST.MatchCase cIdent bVars) (buildStmtAnn ann) <$> addLocalMutObjs ann (zip bVars tVars) (blockType bd)
        else throwError $ annotateError internalErrorSeman EMatchCaseInternalError
      | otherwise = throwError $ annotateError internalErrorSeman $ EMatchCaseBadName cIdent supIdent

----------------------------------------
-- Programs Semantic Analyzer
-- For now all are kinda the same thing but eventually they should not :shrug:
----------------------------------------

-- Keeping only type information
-- TODO Check ident is not defined?
globalCheck :: Global Parser.Annotation -> SemanticMonad (SAST.Global SemanticAnns)
globalCheck (Task ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> (mustBeTy ty =<< expressionType expr)
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Task ident ty exprty mods (buildGlobalAnn anns (STask ty)))
globalCheck (Handler ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> (mustBeTy ty =<< expressionType expr)
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Handler ident ty exprty mods (buildGlobalAnn anns (SHandler ty)))
globalCheck (Resource ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> (mustBeTy ty =<< expressionType expr)
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Resource ident ty exprty mods (buildGlobalAnn anns (SResource ty)))
-- TODO [Q14]
globalCheck (Const ident ty expr mods anns) =
  checkTypeDefinition anns ty >>
  Const ident ty
  <$> (mustBeTy ty =<< expressionType expr)
  <*> pure mods
  <*> pure (buildGlobalAnn anns (SConst ty))

-- Here we actually only need Global
programSeman :: AnnASTElement Parser.Annotation -> SemanticMonad (SAST.AnnASTElement SemanticAnns)
programSeman (Function ident ps mty bret mods anns) =
  maybe (return ()) (checkTypeDefinition anns) mty >>
  (Function ident ps mty
  <$> (addLocalMutObjs anns
          (fmap (\ p -> (paramIdentifier p , paramTypeSpecifier p)) ps)
          (retblockType bret) >>= \ typed_bret ->
    maybe
      -- | Procedure
      (blockRetTy Unit)
      -- | Function
      blockRetTy
      mty typed_bret >> return typed_bret
      )
  <*> pure mods
  <*> pure (buildGlobal anns (GFun ps (fromMaybe Unit mty))))
programSeman (GlobalDeclaration gbl) =
  -- TODO Add global declarations
  GlobalDeclaration <$> globalCheck gbl
programSeman (TypeDefinition tydef ann) =
  typeDefCheck ann tydef >>= \t ->
    -- let stdef = semanticTypeDef t in
    -- and we can add it to the global environment.
    -- insertGlobalTy ann stdef >>
    return (TypeDefinition t (buildGlobalTy ann (semanticTypeDef t)))

semanticTypeDef :: SAST.TypeDef SemanticAnns -> SemanTypeDef SemanticAnns
semanticTypeDef (Struct i f m)  = Struct i f m
semanticTypeDef (Enum i e m)    = Enum i e m
semanticTypeDef (Class kind i cls m) = Class kind i (Data.List.map kClassMember cls) m

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
typeDefCheck ann (Enum ident evs mds)
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Enumeration-definitions
  = when (Prelude.null evs) (throwError $ annotateError ann (EEnumDefEmpty ident))
  -- Type checking
  >> mapM_ (enumDefinitionTy ann) evs
  -- Check names are unique.
  >> checkUniqueNames ann EEnumDefNotUniqueField (Data.List.map variantIdentifier evs)
  -- Return the same definition.
  >> return (Enum ident evs mds)
typeDefCheck ann (Class kind ident members mds)
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Classes
  -- check that it defines at least one method.
  = 
  -- TODO: Check class well-formedness depending on its kind
  -- when (emptyClass cls) (throwError $ annotateError ann (EClassEmptyMethods ident))
  -- TODO loop free
  -- Split definitions in fields, methods, procedures and viewers.
  -- plus check that all types are well define (see |classTypeChecking|)
  foldM
    (\(fs, prcs, mths, vws) cl ->
        case cl of
          ClassField fs_id fs_ty annCF
            -> checkTypeDefinition annCF fs_ty
              >> simpleTyorFail annCF fs_ty
              >> let checkFs = SAST.ClassField fs_id fs_ty (buildExpAnn annCF fs_ty)
                in return (checkFs : fs, prcs, mths, vws)
          prc@(ClassProcedure _fp_id fp_tys mty _body annCP)
            -> maybe (return ()) (checkTypeDefinition annCP) mty 
              >> mapM_ (checkTypeDefinition annCP . paramTypeSpecifier) fp_tys >>
              return (fs, prc : prcs, mths, vws)
          mth@(ClassMethod _fm_id mty _body annCM)
            -> maybe (return ()) (checkTypeDefinition annCM) mty
            >> return (fs, prcs, mth : mths, vws)
          view@(ClassViewer _fv_id fv_tys mty _body annCV)
            -> checkTypeDefinition annCV mty 
              >> mapM_ (checkTypeDefinition annCV . paramTypeSpecifier) fv_tys
            >> return (fs, prcs, mths, view : vws)
        )
    ([],[],[],[]) members
  >>= \(fls  -- Fields do not need type checking :shrug:
        , prcs -- Procedures.
        , mths  -- Methods
        , vws  -- Viewers
       -- introduce a semi-well formed type.
       )->
  do
  -- Now we can go function by function checking everything is well typed.
  -- Get depndencies
    let dependencies = 
          Data.List.map (
            \case{
              -- This shouldn't happen here but I doesn't add anything
              l@(ClassField identCF _ _) -> (l, identCF, []);
              l@(ClassProcedure identCM _ps _mty bs _ann) ->
                  (l, identCM
                  , concatMap (
                    \case { ("self", [ ids ]) -> [ ids ];
                            a -> error ("In case the impossible happens >>> " ++ show a);
                          } . depToList) (getDepBlock bs));
              l@(ClassMethod identCM _mty bs _ann) ->
                  (l, identCM
                  , concatMap (
                    \case { ("self", [ ids ]) -> [ ids ];
                            a -> error ("In case the impossible happens >>> " ++ show a);
                          } . depToList) (getDepBlock bs));
              l@(ClassViewer identCV _ps _mty bs _ann) ->
                  (l, identCV
                  , concatMap (
                    \case { ("self", [ ids ]) -> [ ids ];
                            a -> error ("In case the impossible happens >>> " ++ show a);
                          } . depToList) (getDepBlock bs));
            }) (prcs ++ mths ++ vws)
  -- Build dependency graph and functions from internal Node representations to ClassMembers
    let (dependencyGraph, vertexF , _KeyF) = Graph.graphFromEdges dependencies
    let vertexToIdent = (\(n,_,_) -> n) . vertexF
  -- Generate a solution with possible loops.
    let topSortOrder = Data.List.map vertexToIdent $ Graph.topSort dependencyGraph
  -- Type check in order, if a method is missing is because there is a loop.
    fnChecked <-
      foldM (\prevMembers newMember ->
        -- Intermediate Class type only containing fields, no self methods and
        -- previous (following the topsort order) to the current method.
        let clsType = Class kind ident
                  -- Function |kClassMember| /erases/ body of methods.
                  -- When typing, we do not need them
                  (Data.List.map kClassMember (fls ++ prevMembers))
                  mds in
        localScope $ do
          insertGlobalTy ann clsType
          insertLocalMutObj ann "self" (Reference Private (DefinedType ident))
          -- Now analyze new member.
          case newMember of
            -- Filtered Cases
            ClassField {} -> throwError (annotateError internalErrorSeman EClassTyping)
            -- Interesting case
            ClassProcedure mIdent mps mty mbody mann -> do
              typed_bret <- addLocalMutObjs mann (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps) (retblockType mbody)
              maybe (blockRetTy Unit) blockRetTy mty typed_bret
              let newPrc = SAST.ClassProcedure mIdent mps mty  typed_bret (buildExpAnn mann (fromMaybe Unit mty))
              return (newPrc : prevMembers)
            ClassMethod mIdent mty mbody mann -> do
              typed_bret <- retblockType mbody 
              maybe (blockRetTy Unit) blockRetTy mty typed_bret
              let newMth = SAST.ClassMethod mIdent mty  typed_bret (buildExpAnn mann (fromMaybe Unit mty))
              return (newMth : prevMembers)
            ClassViewer mIdent mps ty mbody mann -> do
              typed_bret <- addLocalMutObjs mann (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps) (retblockType mbody)
              blockRetTy ty typed_bret
              let newVw = SAST.ClassViewer mIdent mps ty typed_bret (buildExpAnn mann ty)
              return (newVw : prevMembers) 
        ) [] topSortOrder
    return (Class kind ident (fls ++ fnChecked) mds)

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

-- TODO: Check class well-formedness depending on its kind
-- emptyClass :: [ ClassMember' expr lha a ] -> Bool
-- emptyClass = Data.List.foldl' (\ b x -> case x of { ClassField {} -> b; ClassMethod {} -> False}) True

classDependencies
  :: PAST.ClassMember a
  -> (Identifier, [ClassDep])
classDependencies (ClassField fs_id _ty _ann)
  -- Definition does not depends on anything.
  = (fs_id , [])
classDependencies (ClassMethod fm_id _ blk _ann)
  -- We need to get all invocations to other methods in self.
  = (fm_id, getDepBlock blk)
classDependencies (ClassProcedure fp_id _ _ blk _ann)
  -- Procedures do not depend on anything.
  = (fp_id, getDepBlock blk)
classDependencies (ClassViewer fp_id _ _ blk _ann)
  -- Procedures do not depend on anything.
  = (fp_id, getDepBlock blk)

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
programAdd (Function ident args mretType _bd _mods anns) =
  insertGlobal ident (GFun args (fromMaybe Unit mretType))
  (annotateError (location anns) (EUsedFunName ident))
programAdd (GlobalDeclaration glb) =
  let (global_name, sem) =
        case glb of
          Task ident type_spec _me _mod _ann ->
            (ident, STask type_spec)
          Resource ident type_spec _me _mod _ann ->
            (ident, SResource type_spec)
          Handler ident type_spec _me _mod _ann ->
            (ident, SHandler type_spec)
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

--- Exectuing Type Checking
typeCheckRunE :: PAST.AnnotatedProgram Parser.Annotation
  -> (Either SemanticErrors (SAST.AnnotatedProgram SemanticAnns) , ExpressionState)
typeCheckRunE = runTypeChecking initialExpressionSt  . mapM checkAndAdd
    where
      checkAndAdd t = programSeman t >>= \t' -> programAdd t' >> return t'

typeCheckRun :: PAST.AnnotatedProgram Parser.Annotation
  -> Either SemanticErrors (SAST.AnnotatedProgram SemanticAnns)
typeCheckRun = fst . typeCheckRunE
