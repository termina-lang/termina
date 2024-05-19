-- | Semantic Analysis Module i.e. Type checking
-- This module defines a mapping from |AST Parser.Annotation|
-- to |AST SemanticAnnotations|, | SemanAST {ParserInfo , TypeInfo}|

module Semantic.TypeChecking where

-- Termina Ast and Utils
import           Annotations
import           AST.Parser                  as PAST
import           Utils.AST.Parser
import           Utils.AST.Core
import           Utils.TypeSpecifier

-- Top Sort
import Extras.TopSort (TopE(..) , topSortFromDepList)
import qualified Data.Map.Strict as M

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

import    qualified       Data.List  (foldl',find, map, nub, sortOn, (\\), sort)
import           Data.Maybe

-- import Control.Monad.State as ST
import           Control.Monad

type SemanticPass t = t Parser.Annotation -> SemanticMonad (t SemanticAnns)

----------------------------------------
-- Function type-checking binary operations.
-- It returns resulting type of using an operation.


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
  expressionType (Just (paramTypeSpecifier p)) rhsObjectType a
  >>= \tyed_exp -> (tyed_exp :) <$> paramType ann ps as
paramType ann (_p : _) [] = throwError $ annotateError ann EFunParams
paramType ann [] (_a : _) = throwError $ annotateError ann EFunParams

constParamType ::
  -- | Annotation of the function call
  Parser.Annotation
  -- | List of parameters
  -> [ConstParameter]
  -- | List of arguments of the function call
  -> [ConstExpression Parser.Annotation]
  -> SemanticMonad [SAST.ConstExpression SemanticAnns]
constParamType _ann [] [] = return []
constParamType ann (p : ps) (a : as) =
  constExpressionType (paramTypeSpecifier $ unConstParam p) a
  >>= \tyed_exp -> (tyed_exp :) <$> constParamType ann ps as

constParamType ann (_p : _) [] = throwError $ annotateError ann EFunParams
constParamType ann [] (_a : _) = throwError $ annotateError ann EFunParams

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an internal error.
unboxObjectSAnns :: SAST.Object SemanticAnns -> SemanticMonad (AccessKind, TypeSpecifier)
unboxObjectSAnns = maybe (throwError $ annotateError internalErrorSeman EUnboxingObjectExpr) return . getObjectSAnns . getAnnotation

unboxConstSAnns :: SAST.ConstExpression SemanticAnns -> SemanticMonad TypeSpecifier
unboxConstSAnns = maybe (throwError $ annotateError internalErrorSeman EUnboxingConstExpr) return . getTypeSAnns . getAnnotation

unboxLocalEnvKind :: EnvKind -> SemanticMonad AccessKind
unboxLocalEnvKind (LocalKind ak) = return ak
unboxLocalEnvKind ConstKind = return Immutable
unboxLocalEnvKind _ = throwError $ annotateError internalErrorSeman EUnboxingLocalEnvKind

memberFieldAccessType :: Parser.Annotation -> TypeSpecifier -> Identifier -> SemanticMonad TypeSpecifier
memberFieldAccessType ann obj_ty ident =
  case obj_ty of
    Location obj_ty' -> memberFieldAccessType ann obj_ty' ident
    DefinedType dident -> getGlobalTy internalErrorSeman dident >>=
      \case{
        -- Either a struct
        Struct _identTy fields _mods ->
            let mfield = Data.List.find ((ident ==) . fieldIdentifier) fields in
              maybe
              (throwError $ annotateError ann (EMemberAccessNotMember ident))
              (return . fieldTypeSpecifier) mfield
        ;
        -- Or a class
        Class _ _identTy cls _implements _mods ->
          -- TODO Class access?
          -- Find |ident| field in the class.
          case findClassField ident cls of
            Nothing -> throwError $ annotateError ann (EMemberAccessNotMember ident)
            -- type |t| and the type inside |a| should be the same, no?
            Just (t , _a) -> return t
        ;
        -- Other types do not have members.
        ty -> throwError $ annotateError ann (EMemberAccessUDef (fmap forgetSemAnn ty))
      }
    ty -> throwError $ annotateError ann (EMemberAccess ty)


objectType ::
  -- | Scope of variables. It returns its access kind (mutable, immutable or private) and its type
  (Parser.Annotation -> Identifier -> SemanticMonad (EnvKind, TypeSpecifier))
  -- The object to type
  -> Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns)
objectType getVarTy (Variable ident ann) = do
  (ek, ty) <- getVarTy ann ident
  ak <- unboxLocalEnvKind ek
  return $ SAST.Variable ident (buildExpAnnObj ann ak ty)
objectType getVarTy (ArrayIndexExpression obj idx ann) = do
  typed_obj <- objectType getVarTy obj
  (obj_ak, obj_ty) <- unboxObjectSAnns typed_obj
  case obj_ty of
    Array ty_elems _vexp -> do
        idx_typed  <- expressionType (Just USize) rhsObjectType idx
        return $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann obj_ak ty_elems
    Reference ref_ak (Array ty_elems _vexp) -> do
        idx_typed  <- expressionType (Just USize) rhsObjectType idx
        return $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann ref_ak ty_elems
    ty -> throwError $ annotateError ann (EArray ty)
objectType _ (MemberAccess obj ident ann) = do
  -- | Attention on deck!
  -- This is a temporary solution pending confirmation that it works in all cases. 
  -- Problem: you cannot access the fields of a global object, you can only access
  -- the procedures in the case of shared resources. To avoid accessing the fields
  -- of a global object, we have adopted the following solution: when accessing the
  -- field of an object, only the local objects are available, not the global ones. 
  -- This way, only the fields of objects that are in the local environment of the
  -- function can be accessed.
  typed_obj' <- objectType (\loc ident' -> do
    (ak, ts) <- getLocalObjTy loc ident'
    return (LocalKind ak, ts)) obj
  (obj_ak', obj_ty') <- unboxObjectSAnns typed_obj'
  let (typed_obj, obj_ak, obj_ty) =
        maybe (typed_obj', obj_ak', obj_ty') (unDyn typed_obj', Mutable, ) (isDyn obj_ty')
  fts <- memberFieldAccessType ann obj_ty ident
  return $ SAST.MemberAccess typed_obj ident $ buildExpAnnObj ann obj_ak fts
objectType getVarTy (Dereference obj ann) = do
  typed_obj <- objectType getVarTy obj
  (_, obj_ty) <- unboxObjectSAnns typed_obj
  case obj_ty of
    Reference ak ty -> return $ SAST.Dereference typed_obj $ buildExpAnnObj ann ak ty
    ty              -> throwError $ annotateError ann $ ETypeNotReference ty
objectType getVarTy (ArraySlice obj lower upper anns) = do
  typed_obj <- objectType getVarTy obj
  (obj_ak, obj_ty) <- unboxObjectSAnns typed_obj
  typed_lower <- expressionType (Just USize) rhsObjectType lower
  typed_upper <- expressionType (Just USize) rhsObjectType upper
  case obj_ty of
    Array ty_elems _ -> do
      return $ SAST.ArraySlice typed_obj typed_lower typed_upper $ buildExpAnnObj anns obj_ak (Slice ty_elems)
    ty -> throwError $ annotateError anns (EArray ty)
objectType getVarTy (DereferenceMemberAccess obj ident ann) = do
  typed_obj <- objectType getVarTy obj
  (_, obj_ty) <- unboxObjectSAnns typed_obj
  case obj_ty of
    Reference ak refTy ->
      memberFieldAccessType ann refTy ident >>=
        \fts -> return $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnObj ann ak fts
    ty -> throwError $ annotateError ann $ ETypeNotReference ty

memberFunctionAccessType ::
  Parser.Annotation
  -> TypeSpecifier
  -> Identifier
  -> [ConstExpression Parser.Annotation]
  -> [Expression Parser.Annotation]
  -> SemanticMonad (([ConstParameter], [SAST.ConstExpression SemanticAnns]), ([Parameter], [SAST.Expression SemanticAnns]), TypeSpecifier)
memberFunctionAccessType ann obj_ty ident constArgs args =
  -- Calling a self method or viewer. We must not allow calling a procedure.
  case obj_ty of
    DefinedType dident -> getGlobalTy ann dident >>=
      \case{
        -- This case corresponds to a call to an inner method or viewer from the self object.
        Class _ _identTy cls _provides _mods ->
          case findClassProcedure ident cls of
            Just _ -> throwError $ annotateError ann (EMemberAccessInvalidProcedureCall ident)
            Nothing ->
              case findClassViewerOrMethod ident cls of
                Just (cps, ps, _, anns) -> do
                  let (psLen , asLen ) = (length ps, length args)
                      (cpsLen, casLen) = (length cps, length constArgs)
                  -- Check that the number of parameters are OK
                  when (cpsLen < casLen) (throwError $ annotateError ann EMemberMethodExtraConstParams)
                  when (cpsLen > casLen) (throwError $ annotateError ann EMemberMethodMissingConstParams)
                  when (psLen < asLen) (throwError $ annotateError ann EMemberMethodExtraParams)
                  when (psLen > asLen) (throwError $ annotateError ann EMemberMethodMissingParams)
                  typed_args <- zipWithM (\p e -> expressionType (Just (paramTypeSpecifier p)) rhsObjectType e) ps args
                  typed_constArgs <- zipWithM (\p e -> do constExpressionType (paramTypeSpecifier $ unConstParam p) e) cps constArgs
                  fty <- maybe (throwError $ annotateError internalErrorSeman EMemberMethodType) return (getTypeSAnns anns)
                  return ((cps, typed_constArgs), (ps, typed_args), fty)
                Nothing -> throwError $ annotateError ann (EMemberAccessNotFunction ident)
          ;
        -- Other User defined types do not define methods
        ty -> throwError $ annotateError ann (EMemberFunctionUDef (fmap forgetSemAnn ty))
      }
    AccessPort (DefinedType dident) -> getGlobalTy ann dident >>=
      \case{
         Interface _identTy cls _mods ->
         case findInterfaceProcedure ident cls of
           Nothing -> throwError $ annotateError ann (EMemberAccessNotProcedure ident)
           Just (cps, ps, anns) -> do
              let (psLen , asLen) = (length ps, length args)
              when (psLen < asLen) (throwError $ annotateError ann (EProcedureCallExtraParams (ident, ps, location anns) (fromIntegral asLen)))
              when (psLen > asLen) (throwError $ annotateError ann (EProcedureCallMissingParams (ident, ps, location anns) (fromIntegral asLen)))
              typed_args <- zipWithM (\p e -> expressionType (Just (paramTypeSpecifier p)) rhsObjectType e) ps args
              typed_constArgs <- zipWithM (\p e -> do
                  constExpressionType (paramTypeSpecifier $ unConstParam p) e) cps constArgs
              fty <- maybe (throwError $ annotateError internalErrorSeman EMemberMethodType) return (getTypeSAnns anns)
              return ((cps, typed_constArgs), (ps, typed_args), fty)
         ;
         -- Other User defined types do not define methods
         ty -> throwError $ annotateError ann (EMemberFunctionUDef (fmap forgetSemAnn ty))
      }
    AccessPort (Allocator ty_pool) ->
      case ident of
        "alloc" ->
          case args of
            [refM] -> do
              typed_ref <- expressionType (Just (Reference Mutable (Option (DynamicSubtype ty_pool))) ) rhsObjectType refM
              return (([], []), ([Parameter "opt" (Reference Mutable (Option (DynamicSubtype ty_pool)))], [typed_ref]), Unit)
            _ -> throwError $ annotateError ann EPoolsWrongNumArgs
        "free" ->
          case args of
            [element] -> do
              element_typed <- expressionType (Just (DynamicSubtype ty_pool)) rhsObjectType element
              element_type <- getExpType element_typed
              case element_type of
                  (DynamicSubtype tyref) ->
                      unless (groundTyEq ty_pool tyref) (throwError $ annotateError ann (EPoolsWrongArgTypeW element_type)) >>
                      return (([], []), ([Parameter "element" element_type], [element_typed]), Unit)
                  _ -> throwError $ annotateError ann (EPoolsWrongArgTypeW element_type)
            _ -> throwError $ annotateError ann EPoolsWrongNumArgs
        _ -> throwError $ annotateError ann (EPoolsWrongProcedure ident)
    OutPort ty ->
      case ident of
        -- send(T)
        "send" ->
          case args of
            [element] -> do
              -- Type the first argument element
              element_typed <- expressionType (Just ty) rhsObjectType element
              case element_typed of
                (SAST.AccessObject (SAST.Variable {})) -> return ()
                _ -> throwError $ annotateError ann EMsgQueueSendArgNotObject
              return (([], []), ([Parameter "element" ty] , [element_typed]), Unit)
            _ -> throwError $ annotateError ann ENoMsgQueueSendWrongArgs
        _ -> throwError $ annotateError ann $ EMsgQueueWrongProcedure ident
    ty -> throwError $ annotateError ann (EFunctionAccessNotResource ty)

----------------------------------------
-- These two functions are useful, one lookups in read+write environments,
-- the other one in write+environments
rhsObjectType, lhsObjectType, globalObjectType :: Object Parser.Annotation
  -> SemanticMonad (SAST.Object SemanticAnns)
rhsObjectType = objectType getRHSVarTy
lhsObjectType = objectType getLHSVarTy
globalObjectType = objectType getGlobalVarTy
----------------------------------------

constExpressionType :: TypeSpecifier -> ConstExpression Parser.Annotation -> SemanticMonad (SAST.ConstExpression SemanticAnns)
constExpressionType expected_ty (KC constant ann) = do
  checkConstant ann expected_ty constant
  return $ SAST.KC constant (buildExpAnn ann expected_ty)
constExpressionType expected_ty (KV identifier ann) = do
  ty <- getConstTy ann identifier
  sameOrErr ann expected_ty ty
  return $ SAST.KV identifier (buildExpAnn ann ty)

-- | Function |expressionType| takes an expression from the parser, traverse it
-- annotating each node with its type.
-- Since we are also creating new nodes (|Undyn| annotations), instead of just
-- traversing, we are actually /creating/ a new tree with implicit
-- constructions.
expressionType ::
  -- | Expected type of the expression
  Maybe TypeSpecifier ->
  -- | Function used to type objects (depends on the scope)
  (Object Parser.Annotation -> SemanticMonad (SAST.Object SemanticAnns))
  -- | Expression to type
  -> Expression Parser.Annotation
  -> SemanticMonad (SAST.Expression SemanticAnns)
-- Object access
expressionType expectedType objType (AccessObject obj) = do
    -- | Type the object
    typed_obj <- objType obj
    -- | Get the type of the object
    (_, obj_type) <- unboxObjectSAnns typed_obj
    case (expectedType, obj_type) of
      (Just (DynamicSubtype ts), DynamicSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        sameOrErr (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      (Just ts, DynamicSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        sameOrErr (getAnnotation obj) ts ts'
        -- If we have an dyn and expect an undyned type:
        return $ SAST.AccessObject (unDyn typed_obj)
      (Just ts, ts') -> do
        -- If the type must be an expected type, then check it.
        sameOrErr (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      -- If we are requesting an object without an expected type, then we must
      -- be casting the result. Thus, we must return an undyned object.
      (Nothing, DynamicSubtype _)->
        return $ SAST.AccessObject (unDyn typed_obj)
      (Nothing, _) ->
        return $ SAST.AccessObject typed_obj
expressionType (Just expectedType) _ (Constant c pann) = do
  checkConstant pann expectedType c
  return $ SAST.Constant c (buildExpAnn pann expectedType)
expressionType Nothing _ (Constant c@(I _ (Just ts)) pann) = do
  return $ SAST.Constant c (buildExpAnn pann ts)
expressionType Nothing _ (Constant c@(I _ Nothing) pann) = do
  throwError $ annotateError pann $ EConstantWithoutKnownType c
expressionType Nothing _ (Constant c@(B {}) pann) = do
  return $ SAST.Constant c (buildExpAnn pann Bool)
expressionType Nothing _ (Constant c@(C {}) pann) = do
  return $ SAST.Constant c (buildExpAnn pann Char)
expressionType expectedType objType (Casting e nty pann) = do
  maybe (return ()) (sameOrErr pann nty) expectedType
  -- | Casting Expressions.
  typed_exp <- expressionType Nothing objType e
  type_exp <- getExpType typed_exp
  if casteableTys type_exp nty
  then return (SAST.Casting typed_exp nty (buildExpAnn pann nty))
  else throwError (annotateError pann $ ECasteable type_exp nty)
expressionType expectedType objType (BinOp op le re pann) = do

  case op of
    Addition -> sameNumType
    Subtraction -> sameNumType
    Multiplication -> sameNumType
    Division -> sameNumType
    Modulo -> sameNumType
    BitwiseLeftShift -> leftExpNumType
    BitwiseRightShift -> leftExpNumType
    RelationalEqual -> sameGroundTyBool
    RelationalNotEqual -> sameGroundTyBool
    RelationalLT -> sameNumTyBool
    RelationalLTE -> sameNumTyBool
    RelationalGT -> sameNumTyBool
    RelationalGTE -> sameNumTyBool
    BitwiseAnd -> sameNumType
    BitwiseOr  -> sameNumType
    BitwiseXor -> sameNumType
    LogicalAnd -> sameBoolType
    LogicalOr  -> sameBoolType

  where

    -- | This function checks the that the lhs and the rhs are both
    -- equal to the expected type and that the expected type is
    -- a numeric type. This function is used to check the
    -- binary expressions multiplication, division, addition and subtraction.
    sameNumType :: SemanticMonad (SAST.Expression SemanticAnns)
    sameNumType =
      case expectedType of
        ty@(Just ty') -> do
          unless (numTy ty') (throwError $ annotateError pann (EExpectedNumType ty'))
          tyle <- expressionType ty objType le
          tyre <- expressionType ty objType re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (numTy tyle_ty) (throwError $ annotateError pann (EExpectedNumType tyle_ty))
          unless (numTy tyre_ty) (throwError $ annotateError pann (EExpectedNumType tyre_ty))
          unless (groundTyEq tyle_ty tyre_ty) (throwError $ annotateError pann (EOpMismatch op tyle_ty tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann tyre_ty)

    leftExpNumType :: SemanticMonad (SAST.Expression SemanticAnns)
    leftExpNumType =
      case expectedType of
        ty@(Just ty') -> do
          tyle <- expressionType ty objType le
          tyre <- expressionType Nothing objType re
          tyre_ty <- getExpType tyre
          unless (numTy ty') (throwError $ annotateError pann (EExpectedNumType ty'))
          unless (posTy tyre_ty) (throwError $ annotateError pann (EExpectedPosType tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (numTy tyle_ty) (throwError $ annotateError pann (EExpectedNumType tyle_ty))
          unless (posTy tyre_ty) (throwError $ annotateError pann (EExpectedPosType tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann tyle_ty)

    sameGroundTyBool :: SemanticMonad (SAST.Expression SemanticAnns)
    sameGroundTyBool =
      case expectedType of
        (Just Bool) -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (groundTyEq tyle_ty tyre_ty) (throwError $ annotateError pann (EOpMismatch op tyle_ty tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        Nothing -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (groundTyEq tyle_ty tyre_ty) (throwError $ annotateError pann (EOpMismatch op tyle_ty tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        _ -> throwError $ annotateError pann EExpectedType

    sameNumTyBool :: SemanticMonad (SAST.Expression SemanticAnns)
    sameNumTyBool =
      case expectedType of
        (Just Bool) -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (groundTyEq tyle_ty tyre_ty) (throwError $ annotateError pann (EOpMismatch op tyle_ty tyre_ty))
          unless (numTy tyle_ty) (throwError $ annotateError pann (EExpectedNumType tyle_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        Nothing -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (groundTyEq tyle_ty tyre_ty) (throwError $ annotateError pann (EOpMismatch op tyle_ty tyre_ty))
          unless (numTy tyle_ty) (throwError $ annotateError pann (EExpectedNumType tyle_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        _ -> throwError $ annotateError pann EExpectedType

    sameBoolType :: SemanticMonad (SAST.Expression SemanticAnns)
    sameBoolType =
      case expectedType of
        (Just Bool) -> do
          tyle <- expressionType (Just Bool) objType le
          tyre <- expressionType (Just Bool) objType re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        Nothing -> do
          tyle <- expressionType Nothing objType le
          tyre <- expressionType Nothing objType re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (groundTyEq tyle_ty tyre_ty) (throwError $ annotateError pann (EOpMismatch op tyle_ty tyre_ty))
          unless (boolTy tyle_ty) (throwError $ annotateError pann (EMismatch Bool tyle_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        _ -> throwError $ annotateError pann EExpectedType

expressionType expectedType objType (ReferenceExpression refKind rhs_e pann) = do
  -- TODO [Q15] ??
  typed_obj <- objType rhs_e
  (_, obj_type) <- unboxObjectSAnns typed_obj
  case obj_type of
    DynamicSubtype ty -> do
      maybe (return ()) (sameOrErr pann (Reference refKind ty)) expectedType
      return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (Reference refKind ty)))
    Slice ty -> do
      case expectedType of
        Just rtype@(Reference ak (Array ts size)) -> do
          unless (groundTyEq ty ts) (throwError $ annotateError pann $ EMismatch ts ty)
          unless (refKind == ak) (throwError $ annotateError pann $ EMismatchAccessKind refKind ak)
          return (SAST.ArraySliceExpression refKind typed_obj size (buildExpAnn pann rtype))
        _ -> throwError $ annotateError pann EExpectedType
    _ -> do
      maybe (return ()) (sameOrErr pann (Reference refKind obj_type)) expectedType
      return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (Reference refKind obj_type)))
---------------------------------------
-- | Function Expression.  A tradicional function call
expressionType expectedType _ (FunctionExpression fun_name constArgs args pann) = do
  (constParams, params, retty) <- getFunctionTy pann fun_name
  let expAnn = buildExpAnnApp pann constParams params retty
  typed_constArgs <- constParamType pann constParams constArgs
  typed_args <- paramType pann params args
  maybe (return ()) (sameOrErr pann retty) expectedType
  return $ SAST.FunctionExpression fun_name typed_constArgs typed_args expAnn

----------------------------------------
expressionType expectedType objType (MemberFunctionAccess obj ident constArgs args ann) = do
  obj_typed <- objType obj
  (_, obj_ty) <- unboxObjectSAnns obj_typed
  ((cps, typed_constArgs), (ps, typed_args), fty) <- memberFunctionAccessType ann obj_ty ident constArgs args
  maybe (return ()) (sameOrErr ann fty) expectedType
  return $ SAST.MemberFunctionAccess obj_typed ident typed_constArgs typed_args (buildExpAnnApp ann cps ps fty)
expressionType expectedType objType (DerefMemberFunctionAccess obj ident constArgs args ann) = do
  obj_typed <- objType obj
  (_, obj_ty) <- unboxObjectSAnns obj_typed
  case obj_ty of
    Reference _ refTy -> do
      -- NOTE: We have reused the code from MemberFunctionAccess, but we must take into
      -- account that, for the time being, when we are accessing a member function through
      -- a reference, the object (self) can only be of a user-defined class type. There
      -- cannot be references to ports. 
      ((cps, typed_constArgs), (ps, typed_args), fty) <- memberFunctionAccessType ann refTy ident constArgs args
      maybe (return ()) (sameOrErr ann fty) expectedType
      return $ SAST.DerefMemberFunctionAccess obj_typed ident typed_constArgs typed_args (buildExpAnnApp ann cps ps fty)
    ty -> throwError $ annotateError ann $ ETypeNotReference ty
----------------------------------------
expressionType expectedType objType (FieldAssignmentsExpression id_ty fs pann) =
  -- | Field Type
  catchError
    (getGlobalTy pann id_ty )
    (\_ -> throwError $ annotateError pann (ETyNotStructFound id_ty))
  >>= \case{
    Struct _ ty_fs _mods  ->
      maybe (return ()) (sameOrErr pann (DefinedType id_ty)) expectedType >>
      flip (SAST.FieldAssignmentsExpression id_ty) (buildExpAnn pann (DefinedType id_ty))
        <$> checkFieldValues pann objType ty_fs fs;
    Class _clsKind _ident members _provides _mods ->
      let fields = [fld | (ClassField fld@(FieldDefinition {}) _) <- members] in
        maybe (return ()) (sameOrErr pann (DefinedType id_ty)) expectedType >>
        flip (SAST.FieldAssignmentsExpression id_ty)
            (buildExpAnn pann (DefinedType id_ty))
        <$>
        checkFieldValues pann objType fields fs;
   x -> throwError $ annotateError pann (ETyNotStruct id_ty (fmap forgetSemAnn x));
  }
expressionType expectedType objType (EnumVariantExpression id_ty variant args pann) =
  -- | Enum Variant
  catchError
    (getGlobalTy pann id_ty)
    (\_ -> throwError $ annotateError pann (ETyNotEnumFound id_ty))
  >>= \case{
   Enum _ ty_vs _mods ->
     case Data.List.find ((variant ==) . variantIdentifier) ty_vs of
       Nothing -> throwError $ annotateError pann (EEnumVariantNotFound variant)
       Just (EnumVariant _ ps) ->
         let (psLen , asLen ) = (length ps, length args) in
         if psLen == asLen
         then
            maybe (return ()) (sameOrErr pann (DefinedType id_ty)) expectedType >>
            flip (SAST.EnumVariantExpression id_ty variant) (buildExpAnn pann (DefinedType id_ty))
             <$> zipWithM (\p e -> expressionType (Just p) objType e) ps args
         else if psLen < asLen
         then throwError $ annotateError pann EEnumVariantExtraParams
         else throwError $ annotateError pann EEnumVariantMissingParams
    ;
   x -> throwError $ annotateError pann (ETyNotEnum id_ty (fmap forgetSemAnn x))
  }
-- IDEA Q4
expressionType expectedType objType (ArrayInitExpression iexp size pann) = do
-- | Array Initialization
  case expectedType of
    Just (Array ts _) -> do
      typed_init <- expressionType (Just ts) objType iexp
      checkSize pann size
      return $ SAST.ArrayInitExpression typed_init size (buildExpAnn pann (Array ts size))
    Just ts -> throwError $ annotateError pann (EArray ts)
    _ -> throwError $ annotateError pann EExpectedType
-- DONE [Q5]
-- TODO [Q17]
expressionType expectedType objType (OptionVariantExpression vexp anns) =
  case expectedType of
    Just (Option ts) -> do
      case vexp of
        None -> return $ SAST.OptionVariantExpression None (buildExpAnn anns (Option ts))
        Some e -> do
          typed_e <- expressionType (Just ts) objType e
          return $ SAST.OptionVariantExpression (Some typed_e) (buildExpAnn anns (Option ts))
    _ -> throwError $ annotateError anns EExpectedType
expressionType expectedType objType (IsEnumVariantExpression obj id_ty variant_id pann) = do
  obj_typed <- objType obj
  (_, obj_ty) <- unboxObjectSAnns obj_typed
  lhs_ty <- case obj_ty of
    DefinedType dident ->
      catchError (getGlobalTy pann dident) (\_ -> throwError $ annotateError pann (EIsVariantNotEnum obj_ty))
    _ -> throwError $ annotateError pann (EIsVariantNotEnum obj_ty)
  case lhs_ty of
    Enum lhs_enum ty_vs _mods ->
      if lhs_enum == id_ty
      then
        case Data.List.find ((variant_id ==) . variantIdentifier) ty_vs of
          Just (EnumVariant {}) -> do
            maybe (return ()) (sameOrErr pann Bool) expectedType
            return $ SAST.IsEnumVariantExpression obj_typed id_ty variant_id (buildExpAnn pann Bool)
          Nothing -> throwError $ annotateError pann (EEnumVariantNotFound variant_id)
      else throwError $ annotateError pann (EIsVariantEnumMismatch lhs_enum id_ty)
    x -> throwError $ annotateError pann (ETyNotEnum id_ty (fmap forgetSemAnn x))
expressionType expectedType objType (IsOptionVariantExpression obj variant_id pann) = do
  obj_typed <- objType obj
  (_, obj_ty) <- unboxObjectSAnns obj_typed
  case obj_ty of
    (Option {}) -> do
      maybe (return ()) (sameOrErr pann Bool) expectedType
      return $ SAST.IsOptionVariantExpression obj_typed variant_id (buildExpAnn pann Bool)
    _ -> throwError $ annotateError pann (EIsVariantNotOption obj_ty)

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
  -> (Object Parser.Annotation -> SemanticMonad (SAST.Object SemanticAnns))
  -> FieldDefinition
  -> FieldAssignment Parser.Annotation
  -> SemanticMonad (SAST.FieldAssignment SemanticAnns)
checkFieldValue loc objType (FieldDefinition fid fty) (FieldValueAssignment faid faexp pann) =
  if fid == faid
  then
    flip (SAST.FieldValueAssignment faid) (buildStmtAnn pann) <$> expressionType (Just fty) objType faexp
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc _ (FieldDefinition fid fty) (FieldAddressAssignment faid addr pann) =
  if fid == faid
  then
    case fty of
      Location _ -> return $ SAST.FieldAddressAssignment faid addr (buildExpAnn pann fty)
      _ -> throwError $ annotateError loc (EFieldNotFixedLocation fid)
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc _ (FieldDefinition fid fty) (FieldPortConnection InboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalGEnTy loc sid >>=
    \gentry ->
    case fty of
      SinkPort _ _  ->
        case gentry of
          -- TODO: Check that the type of the inbound port and the type of the emitter match
          GGlob (SEmitter ets) -> return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildSinkPortConnAnn pann ets)
          _ -> throwError $ annotateError loc $ EInboundPortNotEmitter sid
      InPort _ _  ->
        case gentry of
          -- TODO: Check that the type of the inbound port and the type of the emitter match
          GGlob (SChannel cts) -> return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildInPortConnAnn pann cts)
          _ -> throwError $ annotateError loc $ EInboundPortNotChannel sid
      _ -> throwError $ annotateError loc (EFieldNotPort fid)
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc _ (FieldDefinition fid fty) (FieldPortConnection OutboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalGEnTy loc sid >>=
    \gentry ->
    case fty of
      OutPort _ ->
        case gentry of
          -- TODO: Check that the type of the outbound port and the type of the channel match
          GGlob (SChannel cts) -> return $ SAST.FieldPortConnection OutboundPortConnection pid sid (buildOutPortConnAnn pann cts)
          _ -> throwError $ annotateError loc $ EOutboundPortNotChannel sid
      _ -> throwError $ annotateError loc (EFieldNotPort fid)
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc _ (FieldDefinition fid fty) (FieldPortConnection AccessPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalGEnTy loc sid >>=
    \gentry ->
    case fty of
      AccessPort (Allocator {}) ->
        case gentry of
          -- TODO: Check that the types match
          GGlob (SResource (Pool {})) -> return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildStmtAnn pann)
          _ -> throwError $ annotateError loc $ EAccessPortNotPool sid
      AccessPort (DefinedType iface) ->
        getGlobalTy loc iface >>=
          \case {
            Interface _ members _ ->
              -- Check that the resource provides the interface
              case gentry of
                GGlob (SResource rts@(DefinedType {})) ->
                  let procs = [SemanProcedure procid | (InterfaceProcedure procid _ _ _) <- members] in
                  return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAccessPortConnAnn pann rts procs)
                _ -> throwError $ annotateError loc $ EAccessPortNotResource sid
              ;
            _ -> throwError $ annotateError loc (EAccessPortNotInterface (DefinedType iface))
          }
      _ -> throwError $ annotateError loc (EFieldNotPort fid)
  else throwError $ annotateError loc (EFieldMissing [fid])

checkFieldValues
  :: Parser.Annotation
  -> (Object Parser.Annotation -> SemanticMonad (SAST.Object SemanticAnns))
  -> [FieldDefinition]
  -> [FieldAssignment Parser.Annotation]
  -> SemanticMonad [SAST.FieldAssignment SemanticAnns]
checkFieldValues loc objType fds fas = checkSortedFields sorted_fds sorted_fas []
  where
    tError = throwError . annotateError loc
    getFid = \case {
      FieldValueAssignment fid _ _ -> fid;
      FieldAddressAssignment fid _ _ -> fid;
      FieldPortConnection _ fid _ _ -> fid;
    }
    sorted_fds = Data.List.sortOn fieldIdentifier fds
    sorted_fas = Data.List.sortOn getFid fas
    -- Same length monadic Zipwith
    checkSortedFields [] [] xs = return $ reverse xs
    checkSortedFields [] es _ = tError (EFieldExtra (fmap getFid es))
    checkSortedFields ms [] _ = tError (EFieldMissing (fmap fieldIdentifier ms))
    checkSortedFields (d:ds) (a:as) acc =
      checkFieldValue loc objType d a >>= checkSortedFields ds as . (:acc)

retStmt :: Maybe TypeSpecifier -> ReturnStmt Parser.Annotation -> SemanticMonad (SAST.ReturnStmt SemanticAnns)
retStmt Nothing (ReturnStmt Nothing anns) = do
  return $ SAST.ReturnStmt Nothing (buildExpAnn anns Unit)
retStmt (Just ts) (ReturnStmt (Just e) anns) = do
  typed_e <- expressionType (Just ts) rhsObjectType e
  -- ReturnStmt (Just ety) . buildExpAnn anns <$> getExpType ety
  return $ ReturnStmt (Just typed_e) (buildExpAnn anns ts)
retStmt (Just ty) (ReturnStmt Nothing anns) = throwError $ annotateError anns $ EReturnValueExpected ty
retStmt Nothing (ReturnStmt _ anns) = throwError $ annotateError anns EReturnValueNotVoid

retblockType :: Maybe TypeSpecifier -> BlockRet Parser.Annotation -> SemanticMonad (SAST.BlockRet SemanticAnns)
retblockType ts (BlockRet bbody rete) = BlockRet <$> blockType bbody <*> retStmt ts rete

blockType :: Block Parser.Annotation -> SemanticMonad (SAST.Block SemanticAnns)
blockType = mapM statementTySimple

-- | Type checking statements. We should do something about Break
-- Rules here are just environment control.
statementTySimple :: Statement Parser.Annotation -> SemanticMonad (SAST.Statement SemanticAnns)
-- Declaration semantic analysis
statementTySimple (Declaration lhs_id lhs_ak lhs_type expr anns) =
  -- Check type is alright
  checkTypeDefinition anns lhs_type >>
  -- Expression and type must match
  expressionType (Just lhs_type) rhsObjectType expr >>= mustBeTy lhs_type >>=
    \ety ->
  -- Insert object in the corresponding environment
  -- If the object is mutable, then we insert it in the local mutable environment
  -- otherwise we insert it in the read-only environment
    (case lhs_ak of
      Mutable -> insertLocalMutObj anns lhs_id lhs_type
      Immutable -> insertLocalImmutObj anns lhs_id lhs_type
      -- | This should not happen since the parser can only generate declarations
      -- of mutable and immutable objects.
      Private -> throwError $ annotateError internalErrorSeman EUnboxingObjectExpr) >>
  -- Return annotated declaration
  return (Declaration lhs_id lhs_ak lhs_type ety (buildStmtAnn anns))
statementTySimple (AssignmentStmt lhs_o rhs_expr anns) = do
{- TODO Q19 && Q20 -}
  lhs_o_typed' <- lhsObjectType lhs_o
  (lhs_o_ak', lhs_o_type') <- unboxObjectSAnns lhs_o_typed'
  let (lhs_o_typed, lhs_o_ak, lhs_o_type) =
        maybe (lhs_o_typed', lhs_o_ak', lhs_o_type') (unDyn lhs_o_typed', Mutable, ) (isDyn lhs_o_type')
  unless (lhs_o_ak /= Immutable) (throwError $ annotateError anns EAssignmentToImmutable)
  rhs_expr_typed' <- expressionType (Just lhs_o_type) rhsObjectType rhs_expr
  type_rhs' <- getExpType rhs_expr_typed'
  rhs_expr_typed <- maybe (return rhs_expr_typed') (\_ -> unDynExp rhs_expr_typed') (isDyn type_rhs')
  ety <- mustBeTy lhs_o_type rhs_expr_typed
  return $ AssignmentStmt lhs_o_typed ety $ buildStmtAnn anns
statementTySimple (IfElseStmt cond_expr tt_branch elifs otherwise_branch anns) = do
  when (not (null elifs) && isNothing otherwise_branch) (throwError $ annotateError anns EIfElseNoOtherwise)
  IfElseStmt
    <$> expressionType (Just Bool) rhsObjectType cond_expr
    <*> localScope (blockType tt_branch)
    <*> mapM (\case {
                 ElseIf eCond eBd ann ->
                   ElseIf <$> expressionType (Just Bool) rhsObjectType eCond
                          <*> localScope (blockType eBd)
                          <*> return (buildStmtAnn ann)
                    }) elifs
    <*> maybe (return Nothing) (fmap Just . localScope . blockType) otherwise_branch
    <*> return (buildStmtAnn anns)
-- Here we could implement some abstract interpretation analysis
statementTySimple (ForLoopStmt it_id it_ty from_expr to_expr mWhile body_stmt anns) = do
  -- Check the iterator is of numeric type
  unless (numTy it_ty) (throwError $ annotateError anns (EForIteratorWrongType it_ty))
  -- Both boundaries should have the same numeric type
  -- Since the type of the boundaries will force the type of the iterator, we must
  -- explicitely define the types of the boundaries.
  typed_fromexpr <- constExpressionType it_ty from_expr
  typed_toexpr <- constExpressionType it_ty to_expr
  ForLoopStmt it_id it_ty typed_fromexpr typed_toexpr
    <$> (case mWhile of
              Nothing -> return Nothing
              Just whileC -> do
                  typed_whileC <- addLocalImmutObjs anns [(it_id, it_ty)] $
                      expressionType (Just Bool) rhsObjectType whileC
                  return (Just typed_whileC)
        )
    <*> addLocalImmutObjs anns [(it_id, it_ty)] (blockType body_stmt)
    <*> return (buildStmtAnn anns)
statementTySimple (SingleExpStmt expr anns) =
  flip SingleExpStmt (buildStmtAnn anns) <$> expressionType (Just Unit) rhsObjectType expr
statementTySimple (MatchStmt matchE cases ann) = do
  typed_matchE <- expressionType Nothing rhsObjectType matchE
  type_matchE <- getExpType typed_matchE
  case type_matchE of
    DefinedType t -> getGlobalTy ann t >>=
        \case {
          Enum _ident flsDef _mods ->
          -- Sort both lists by identifiers
          let ord_flsDef = Data.List.sortOn variantIdentifier flsDef in
          let ord_cases = Data.List.sortOn matchIdentifier cases in
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
      let ord_cases = Data.List.sortOn matchIdentifier cases in
      optionCases ord_cases >>= flip unless (throwError $  annotateError ann EMatchOptionBad)
      >>
      MatchStmt typed_matchE <$> zipWithM matchCaseType ord_cases [EnumVariant "None" [],EnumVariant "Some" [t]] <*> pure (buildStmtAnn ann)
    _ -> throwError $  annotateError ann $ EMatchWrongType type_matchE
    where
      optionCases :: [MatchCase Parser.Annotation] -> SemanticMonad Bool
      optionCases [a,b] = return $ (optionNone a && optionSome b) || (optionSome a && optionNone b)
      optionCases _ = throwError $ annotateError ann EMatchOptionBadArgs
      optionNone :: MatchCase Parser.Annotation -> Bool
      optionNone c =
        matchIdentifier c == "None"
          && Prelude.null (matchBVars c)
      optionSome ::MatchCase Parser.Annotation -> Bool
      optionSome c =
        matchIdentifier c == "Some"
           && length (matchBVars c) == 1

matchCaseType :: MatchCase Parser.Annotation -> EnumVariant -> SemanticMonad (SAST.MatchCase SemanticAnns)
matchCaseType c (EnumVariant vId vData) = matchCaseType' c vId vData
  where
    matchCaseType' (MatchCase cIdent bVars bd ann) supIdent tVars
      | cIdent == supIdent =
        if length bVars == length tVars then
        flip (SAST.MatchCase cIdent bVars) (buildStmtAnn ann) <$> addLocalImmutObjs ann (zip bVars tVars) (blockType bd)
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
              Just expr -> Just <$> expressionType (Just ty) globalObjectType expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Task ident ty exprty mods (buildGlobalAnn anns (STask ty)))
globalCheck (Handler ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> expressionType (Just ty) globalObjectType expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Handler ident ty exprty mods (buildGlobalAnn anns (SHandler ty)))
globalCheck (Resource ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> expressionType (Just ty) globalObjectType expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Resource ident ty exprty mods (buildGlobalAnn anns (SResource ty)))
globalCheck (Emitter ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> expressionType (Just ty) globalObjectType expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  glb <- case ty of
        -- | We are going to manually map de Emitter to the proper type
        (DefinedType "Interrupt") -> return $ SEmitter ty
        (DefinedType "PeriodicTimer") -> return $  SEmitter ty
        (DefinedType "SystemInit") -> return $ SEmitter ty
        _ -> throwError $ annotateError internalErrorSeman EInternalNoGTY
  return (SAST.Emitter ident ty exprty mods (buildGlobalAnn anns glb))
globalCheck (Channel ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> expressionType (Just ty) globalObjectType expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Channel ident ty exprty mods (buildGlobalAnn anns (SChannel ty)))
-- TODO [Q14]
globalCheck (Const ident ty expr mods anns) = do
  checkTypeDefinition anns ty
  typed_expr <- constExpressionType ty expr
  return (SAST.Const ident ty typed_expr mods (buildGlobalAnn anns (SConst ty)))

parameterTypeChecking :: Locations -> Parameter -> SemanticMonad ()
parameterTypeChecking anns p =
    let typeSpec = paramTypeSpecifier p in
     -- If the type specifier is a dyn, then we must throw an EArgHasDyn error
     -- since we cannot have dynamic types as parameters.
    unless (parameterTy typeSpec) (throwError (annotateError anns (EInvalidParameterType p))) >>
    checkTypeDefinition anns typeSpec

constParameterTypeChecking :: Locations -> ConstParameter -> SemanticMonad ()
constParameterTypeChecking anns (ConstParameter p) =
    let typeSpec = paramTypeSpecifier p in
    unless (numTy typeSpec) (throwError (annotateError anns (EConstParameterNotNum p)))

returnTypeChecking :: Locations -> TypeSpecifier -> SemanticMonad ()
returnTypeChecking anns ty =
  unless (parameterTy ty) (throwError (annotateError anns (EInvalidReturnType ty))) >>
  checkTypeDefinition anns ty

-- Here we actually only need Global
programSeman :: AnnASTElement Parser.Annotation -> SemanticMonad (SAST.AnnASTElement SemanticAnns)
programSeman (Function ident cps ps mty bret mods anns) =
  ----------------------------------------
  -- Check the return type 
  maybe (return ()) (returnTypeChecking anns) mty >>
  -- Check generic const parameters
  forM_ cps (constParameterTypeChecking anns) >>
  addLocalConstants anns
      (fmap (\(ConstParameter p) -> (paramIdentifier p , paramTypeSpecifier p)) cps) checkFunction

  where
    checkFunction :: SemanticMonad (SAST.AnnASTElement SemanticAnns)
    checkFunction =
          -- Check regular params
        forM_ ps (parameterTypeChecking anns) >>
          Function ident cps ps mty
            <$> (addLocalImmutObjs anns
                  (fmap (\p -> (paramIdentifier p , paramTypeSpecifier p)) ps)
                  (retblockType mty bret) >>= \ typed_bret ->
                maybe
                    -- Procedure
                    (blockRetTy Unit)
                    -- Function
                    blockRetTy
                    mty typed_bret >> return typed_bret)
            <*> pure mods
            <*> pure (buildGlobal anns (GFun cps ps (fromMaybe Unit mty)))
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
semanticTypeDef (Class kind i cls ps m) = Class kind i (Data.List.map kClassMember cls) ps m
semanticTypeDef (Interface i cls m) = Interface i cls m

interfaceProcedureTy :: InterfaceMember Locations -> SemanticMonad (SAST.InterfaceMember SemanticAnns)
interfaceProcedureTy (InterfaceProcedure ident cps ps annIP) = do
  mapM_ (checkTypeDefinition annIP . paramTypeSpecifier) ps
  return $ InterfaceProcedure ident cps ps (buildExpAnn annIP Unit)

-- | This function type checks the members of a class depending on its kind.
checkClassKind :: Locations -> Identifier -> ClassKind 
  -> ([SAST.ClassMember SemanticAnns], 
      [PAST.ClassMember Locations], 
      [PAST.ClassMember Locations])
  -> [Identifier] -> SemanticMonad ()
-- | Resource class type checking
checkClassKind anns clsId ResourceClass (fs, prcs, acts) provides = do
  -- A resource must provide at least one interface
  when (null provides) (throwError $ annotateError anns (EResourceClassNoProvides clsId))
  -- A resource must not have any actions
  case acts of
    [] -> return ()
    (ClassAction actionId _ _ _ ann):_  -> 
        throwError $ annotateError ann (EResourceClassAction (clsId, anns) actionId)
    _ -> throwError (annotateError internalErrorSeman EClassTyping)
  -- Check that the resource class does not define any in and out ports
  mapM_ (
    \case {
      ClassField (FieldDefinition fs_id fs_ty) annCF ->
        case fs_ty of
          InPort _ _ -> throwError $ annotateError (location annCF) (EResourceClassInPort (clsId, anns) fs_id)
          OutPort _ -> throwError $ annotateError (location annCF) (EResourceClassOutPort (clsId, anns) fs_id)
          _ -> return ()
      ;
      _ -> return ();
    }) fs
  -- Check that all the procedures are provided
  providedProcedures <- concat <$> foldM (\acc ifaceId ->
    catchError (getGlobalTy anns ifaceId)
      (\_ -> throwError $ annotateError anns (EInterfaceNotFound ifaceId)) >>= \case {
      Interface _ iface_prcs _ -> return $ map (, ifaceId) iface_prcs : acc;
      _ -> throwError $ annotateError anns (EMismatchIdNotInterface ifaceId)
    }) [] provides
  let sorted_provided = Data.List.sortOn (\(InterfaceProcedure ifaceId _ _ _, _) -> ifaceId) providedProcedures
  let sorted_prcs = Data.List.sortOn (
        \case {
          (ClassProcedure prcId _ _ _ _) -> prcId;
          _ -> error "internal error: checkClassKind"
        }) prcs
  -- Check that all procedures are provided and that the parameters match
  checkSortedProcedures sorted_provided sorted_prcs

  where

    checkSortedProcedures :: [(InterfaceMember SemanticAnns, Identifier)] -> [ClassMember Locations] -> SemanticMonad ()
    checkSortedProcedures [] [] = return ()
    checkSortedProcedures [] ((ClassProcedure prcId _ _ _ ann):_) = throwError $ annotateError ann (EProcedureNotFromProvidedInterfaces (clsId, anns) prcId)
    checkSortedProcedures ((InterfaceProcedure procId _ _ _, ifaceId):_) [] = throwError $ annotateError anns (EMissingProcedure ifaceId procId)
    checkSortedProcedures ((InterfaceProcedure prcId cps ps pann, ifaceId):ds) ((ClassProcedure prcId' cps' ps' _ ann):as) =
      unless (prcId == prcId') (throwError $ annotateError anns (EMissingProcedure ifaceId prcId)) >> do
      let cpsLen = length cps
          cpsLen' = length cps'
          psLen = length ps
          psLen' = length ps'
      when (cpsLen < cpsLen') (throwError $ annotateError ann (EProcedureExtraConstParams (ifaceId, prcId, cps, location pann) (fromIntegral cpsLen')))
      when (cpsLen > cpsLen') (throwError $ annotateError ann (EProcedureMissingConstParams (ifaceId, prcId, cps, location pann) (fromIntegral cpsLen')))
      when (psLen < psLen') (throwError $ annotateError ann (EProcedureExtraParams (ifaceId, prcId, ps, location pann) (fromIntegral psLen')))
      when (psLen > psLen') (throwError $ annotateError ann (EProcedureMissingParams (ifaceId, prcId, ps, location pann) (fromIntegral psLen')))
      zipWithM_ (checkProcedureConstParam ann) cps cps'
      zipWithM_ (checkProcedureParam ann) ps ps'
      checkSortedProcedures ds as
    checkSortedProcedures _ _ = throwError (annotateError internalErrorSeman EClassTyping)

    checkProcedureConstParam :: Locations -> ConstParameter -> ConstParameter -> SemanticMonad ()
    checkProcedureConstParam ann (ConstParameter (Parameter _ ts)) (ConstParameter (Parameter ident ts')) =
      unless (groundTyEq ts ts') (throwError $ annotateError ann (EProcedureConstParamMismatch ident ts ts'))
    
    checkProcedureParam :: Locations -> Parameter -> Parameter -> SemanticMonad ()
    checkProcedureParam ann (Parameter _ ts) (Parameter ident ts') =
      unless (groundTyEq ts ts') (throwError $ annotateError ann (EProcedureParamMismatch ident ts ts'))

checkClassKind _anns _clsId _kind _members _provides = return ()
  



  

-- Type definition
-- Here I am traversing lists serveral times, I prefer to be clear than
-- proficient for the time being.
typeDefCheck :: Locations -> TypeDef Locations -> SemanticMonad (SAST.TypeDef SemanticAnns)
-- Check Type definitions https://hackmd.io/@termina-lang/SkglB0mq3#Struct-definitions
typeDefCheck ann (Struct ident fs mds) =
  -- Check every type is well-defined:
  -- Check that the struct is not empty
  when (Prelude.null fs) (throwError $ annotateError ann (EStructDefEmptyStruct ident))
  -- Check that every field is well-defined
  >> mapM_ (fieldDefinitionTy ann) fs
  -- Check field names are unique
  >> checkUniqueNames ann EStructDefNotUniqueField (Data.List.map fieldIdentifier fs)
  -- If everything is fine, return same struct
  >> return (Struct ident fs mds)
typeDefCheck ann (Enum ident evs mds) =
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Enumeration-definitions
  -- Check that the enum is not empty
  when (Prelude.null evs) (throwError $ annotateError ann (EEnumDefEmpty ident))
  -- Check the enum variants are well-defined
  >> mapM_ (enumDefinitionTy ann) evs
  -- Check names are unique
  >> checkUniqueNames ann EEnumDefNotUniqueField (Data.List.map variantIdentifier evs)
  -- If everything is fine, return the same definition.
  >> return (Enum ident evs mds)
typeDefCheck ann (Interface ident cls mds) = do
  -- Check that the interface is not empty
  when (null cls) (throwError $ annotateError ann (EInterfaceEmpty ident))
  -- Check procedure names are unique
  checkUniqueNames ann EInterfaceNotUniqueProcedure (Data.List.map (\case InterfaceProcedure ifaceId _ _ _ -> ifaceId) cls)
  -- Check that every procedure is well-defined
  procedures <- mapM interfaceProcedureTy cls
  -- If everything is fine, return the same definition.
  return (Interface ident procedures mds)
typeDefCheck ann (Class kind ident members provides mds) =
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Classes
  -- check that it defines at least one method.
  -- TODO: Check class well-formedness depending on its kind
  -- TODO: Check the class procedures belong to a provided interface
  -- when (emptyClass cls) (throwError $ annotateError ann (EClassEmptyMethods ident))
  foldM
    (\(fs, prcs, mths, vws, acts) cl ->
        case cl of
          -- ClassFields
          ClassField fld@(FieldDefinition _fs_id fs_ty) annCF
            -> checkTypeDefinition annCF fs_ty
              >> classFieldTyorFail annCF fs_ty
              >> let checkFs = SAST.ClassField fld (buildExpAnn annCF fs_ty)
                in return (checkFs : fs, prcs, mths, vws, acts)
          -- Procedures
          prc@(ClassProcedure _fp_id cfp_tys fp_tys _body annCP)
            -> mapM_ (checkTypeDefinition annCP . paramTypeSpecifier . unConstParam) cfp_tys
            >> mapM_ (checkTypeDefinition annCP . paramTypeSpecifier) fp_tys
            >> return (fs, prc : prcs, mths, vws, acts)
          -- Methods
          mth@(ClassMethod _fm_id mty _body annCM)
            -> maybe (return ()) (checkTypeDefinition annCM) mty
            >> return (fs, prcs, mth : mths, vws, acts)
          -- Viewers
          view@(ClassViewer _fv_id cfv_tys fv_tys mty _body annCV)
            -> checkTypeDefinition annCV mty
            -- Parameters cannot have dyns inside.
            >> mapM_ (constParameterTypeChecking annCV) cfv_tys
            >> mapM_ (parameterTypeChecking annCV) fv_tys
            >> return (fs, prcs, mths, view : vws, acts)
          action@(ClassAction _fa_id fa_ty mty _body annCA)
            -> checkTypeDefinition annCA mty
            >> (checkTypeDefinition annCA . paramTypeSpecifier) fa_ty
            >> return (fs, prcs, mths, vws , action : acts)
        )
    ([],[],[],[],[]) members
  >>= \(fls   -- Fields do not need type checking :shrug:
       , prcs -- Procedures.
       , mths -- Methods
       , vws  -- Viewers
       , acts -- Actions
       -- introduce a semi-well formed type.
       ) ->
  do
    checkClassKind ann ident kind (fls, prcs, acts) provides
  -- Now we can go function by function checking everything is well typed.
  ----------------------------------------
  -- Loop between methods, procedures and viewers.
  -- Assumption: dependencies are computes through `method g () {... self->f()
  -- ...}`, then `g > f`.
    let elements = prcs ++ mths ++ vws ++ acts
    -- Dependencies emplying the assumption.
    let dependencies =
          foldr (\a res -> maybe res (:res) (selfDepClass objIsSelf a)) [] elements
    -- Map from ClassNames to their definition (usefull after sorting by name and dep)
    let nameClassMap = M.fromList (map (\e -> (className e, e)) elements)
    -- Sort and see if there is a loop
    topSortOrder <- case topSortFromDepList dependencies of
            -- Tell the user a loop is in the room
            Left (ELoop loop) -> throwError (annotateError ann (EClassLoop loop))
            Left _ -> error "Internal TopSort Error"
            -- Get the proper order of inclusion and get their definitions from names.
            Right order ->
              mapM
              (maybe
                (throwError (annotateError internalErrorSeman EMissingIdentifier))
                return
                . (`M.lookup` nameClassMap)) order
  ----------------------------------------
  -- Type check in order, if a method is missing is because there is a loop.
    fnChecked <-
      foldM (\prevMembers newMember ->
        -- Intermediate Class type only containing fields, no self methods and
        -- previous (following the topsort order) to the current method.
        let clsType = Class kind ident
                  -- Function |kClassMember| /erases/ body of methods.
                  -- When typing, we do not need them
                  (Data.List.map kClassMember (fls ++ prevMembers))
                  provides mds in
        localScope $ do
          insertGlobalTy ann clsType
          -- Now analyze new member.
          case newMember of
            -- Filtered Cases
            ClassField {} -> throwError (annotateError internalErrorSeman EClassTyping)
            -- Interesting case
            ClassProcedure mIdent mcps mps blk mann -> do
              typed_blk <- addLocalImmutObjs mann (("self", Reference Private (DefinedType ident)) : fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps) (blockType blk)
              let newPrc = SAST.ClassProcedure mIdent mcps mps typed_blk (buildExpAnn mann Unit)
              return (newPrc : prevMembers)
            ClassMethod mIdent mty mbody mann -> do
              typed_bret <- addLocalImmutObjs mann [("self", Reference Private (DefinedType ident))] (retblockType mty mbody)
              maybe (blockRetTy Unit) blockRetTy mty typed_bret
              let newMth = SAST.ClassMethod mIdent mty  typed_bret (buildExpAnn mann (fromMaybe Unit mty))
              return (newMth : prevMembers)
            ClassViewer mIdent mcps mps ty mbody mann -> do
              typed_bret <- addLocalImmutObjs mann (("self", Reference Immutable (DefinedType ident)) : fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps) (retblockType (Just ty) mbody)
              blockRetTy ty typed_bret
              let newVw = SAST.ClassViewer mIdent mcps mps ty typed_bret (buildExpAnn mann ty)
              return (newVw : prevMembers)
            ClassAction mIdent p ty mbody mann -> do
              typed_bret <- addLocalImmutObjs mann (("self", Reference Mutable (DefinedType ident)) : [(paramIdentifier p, paramTypeSpecifier p)]) (retblockType (Just ty) mbody)
              blockRetTy ty typed_bret
              let newAct = SAST.ClassAction mIdent p ty typed_bret (buildExpAnn mann ty)
              return (newAct : prevMembers)
        ) [] topSortOrder
    return (SAST.Class kind ident (fls ++ fnChecked) provides mds)

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

checkUniqueNames :: Locations -> ([Identifier] -> Errors Locations) -> [Identifier] -> SemanticMonad ()
checkUniqueNames ann err is =
  if allUnique is then return () else throwError $ annotateError ann (err (repeated is))
-----------------------------------------
-- TODO Improve this two functions.
-- nub is O(n^2)
allUnique :: Eq a => [a] -> Bool
allUnique xs = Data.List.nub xs == xs

repeated :: Eq a => [a] -> [a]
repeated xs = Data.List.nub $ xs Data.List.\\ Data.List.nub xs
-----------------------------------------

-- Adding Global elements to the environment.
programAdd :: SAST.AnnASTElement SemanticAnns
  -> SemanticMonad (Identifier, SAnns (GEntry SemanticAnns))
programAdd (Function ident constParams params mretType _bd _mods anns) =
  let
    gbl = GFun constParams params (fromMaybe Unit mretType)
    el = location anns `SemAnn` gbl
  in
  insertGlobal ident el
  (EUsedFunName ident)
  >> return (ident , el)
programAdd (GlobalDeclaration glb) =
  let (global_name, sem, ann_glb) =
        case glb of
          Task ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (ty_ann ann)), ann)
          Resource ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (ty_ann ann)), ann)
          Handler ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (ty_ann ann)), ann)
          Emitter ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (ty_ann ann)), ann)
          Channel ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (ty_ann ann)), ann)
          Const ident _type_spec _e _mod ann ->
            (ident, fromJust (getGEntry (ty_ann ann)), ann)
      el = (location ann_glb `SemAnn` sem) in
  insertGlobal global_name el
  (EUsedGlobalName global_name)
  >> return (global_name , el)
programAdd (TypeDefinition ty anns) =
  let type_name = identifierType ty in
    case ty_ann anns of
      GTy semTy@(GType _) ->
        let el = location anns `SemAnn` semTy in
        insertGlobal
          type_name el
          (EUsedTypeName type_name)
        >> return (type_name , el)
      _ -> throwError (annotateError internalErrorSeman EInternalNoGTY)

--- Exectuing Type Checking
typeCheckRunE :: PAST.AnnotatedProgram Parser.Annotation
  -> (Either SemanticErrors (SAST.AnnotatedProgram SemanticAnns)
     , ExpressionState)
typeCheckRunE = runTypeChecking initialExpressionSt  . mapM checkAndAdd
    where
      checkAndAdd t = programSeman t >>= \t' -> programAdd t' >> return t'

typeCheckRun :: PAST.AnnotatedProgram Parser.Annotation
  -> Either SemanticErrors (SAST.AnnotatedProgram SemanticAnns)
typeCheckRun = fst . typeCheckRunE

-- Module TypeChecking function
typeAndGetGlobals
  -- GlobalEnv from imports
  :: GlobalEnv
  -- Current Termina Module
  -> PAST.AnnotatedProgram Parser.Annotation
  -> Either
        SemanticErrors
        (SAST.AnnotatedProgram SemanticAnns
        , [(Identifier
           , SAnns (GEntry SemanticAnns))])
typeAndGetGlobals preLoad p =
  case buildInit of
    Left err -> Left (annotateError internalErrorSeman err)
    Right intGlbs -> fst (runTypeChecking (makeInitial intGlbs) (foldM checkAddCompile ([],[]) p))
 where
   checkAddCompile (ts, gs) t = do
     tTyped <- programSeman t
     glb <- programAdd tTyped
     -- IMPORTANT: When the module is typed, the typed elements must be included at
     -- the end of the list.
     return (ts ++ [tTyped], gs ++ [glb])
   buildInit =
     Data.List.foldl'
      (\env (k,v) -> either
        Left
        (\env -> if isJust (M.lookup k env)
          then Left (EVarDefined k) else Right (M.insert k v env)
        ) env )
      (Right preLoad)
      initGlb
