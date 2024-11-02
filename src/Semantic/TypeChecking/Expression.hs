module Semantic.TypeChecking.Expression where

import Utils.Annotations
import Parser.AST as PAST
import Core.Utils

-- Termina Semantic AST
import qualified Semantic.AST as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
-- Error module
import Control.Monad.Except
import Semantic.Errors.Errors
-- Semantic Monad
import Semantic.Monad

----------------------------------------
-- Libaries and stuff

import qualified Data.List  (find, sortOn)

-- import Control.Monad.State as ST
import Parser.Types
import Semantic.TypeChecking.Check

getMemberFieldType :: Location -> TerminaType -> Identifier -> SemanticMonad TerminaType
getMemberFieldType loc obj_ty ident =
  case obj_ty of
    TFixedLocation obj_ty' -> getMemberFieldType loc obj_ty' ident
    TStruct dident -> getGlobalTypeDef loc dident >>=
      \case{
        -- Either a struct
        (Located (Struct _identTy fields _mods) strLoc) ->
            let mfield = Data.List.find ((ident ==) . fieldIdentifier) fields in
              maybe
              (throwError $ annotateError loc (EMemberAccessUnknownField (dident, strLoc) ident))
              (return . fieldTerminaType) mfield
        ;
        _ -> throwError $ annotateError Internal EUnboxingStructType
      }
    TGlobal _ dident -> getGlobalTypeDef loc dident >>=
      \case {
        -- Or a class
        Located (Class _clsKind _identTy cls _implements _mods) clsLoc ->
          -- TODO Class access?
          -- Find |ident| field in the class.
          case findClassField ident cls of
            Nothing -> throwError $ annotateError loc (EMemberAccessUnknownField (dident, clsLoc) ident)
            -- type |t| and the type inside |a| should be the same, no?
            Just (t , _a) -> return t
        ;
        -- Other types do not have members.
        _ -> throwError $ annotateError Internal EUnboxingClassType
      }
    ty -> throwError $ annotateError loc (EMemberAccessInvalidType ty)

typeObject ::
  -- | Scope of variables. It returns its access kind (mutable or immutable) and its type
  (ParserAnn -> Identifier -> SemanticMonad (AccessKind, TerminaType))
  -- The object to type
  -> Object ParserAnn
  -> SemanticMonad (SAST.Object SemanticAnn)
typeObject getVarTy (Variable ident ann) = do
  (ak, ty) <- getVarTy ann ident
  return $ SAST.Variable ident (buildExpAnnObj ann ak ty)
typeObject getVarTy (ArrayIndexExpression obj idx ann) = do
  typed_obj <- typeObject getVarTy obj
  (obj_ak, obj_ty) <- getObjType typed_obj
  case obj_ty of
    TArray ty_elems _vexp -> do
        idx_typed  <- catchMismatch (getAnnotation idx) EArrayIndexNotUSize (typeExpression (Just TUSize) typeRHSObject idx)
        return $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann obj_ak ty_elems
    TReference ref_ak (TArray ty_elems _vexp) -> do
        idx_typed  <- catchMismatch (getAnnotation idx) EArrayIndexNotUSize (typeExpression (Just TUSize) typeRHSObject idx)
        return $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann ref_ak ty_elems
    ty -> throwError $ annotateError ann (EInvalidArrayIndexing ty)
typeObject _ (MemberAccess obj ident ann) = do
  -- | Attention on deck!
  -- This is a temporary solution pending confirmation that it works in all cases. 
  -- Problem: you cannot access the fields of a global object, you can only access
  -- the procedures in the case of shared resources. To avoid accessing the fields
  -- of a global object, we have adopted the following solution: when accessing the
  -- field of an object, only the local objects are available, not the global ones. 
  -- This way, only the fields of objects that are in the local environment of the
  -- function can be accessed.
  typed_obj' <- typeObject (\loc ident' -> do
    (ak, ts) <- getLHSVarTy loc ident'
    return (ak, ts)) obj
  (obj_ak', obj_ty') <- getObjType typed_obj'
  let (typed_obj, obj_ak, obj_ty) =
        maybe (typed_obj', obj_ak', obj_ty') (unBox typed_obj', Mutable, ) (isBox obj_ty')
  fts <- getMemberFieldType ann obj_ty ident
  return $ SAST.MemberAccess typed_obj ident $ buildExpAnnObj ann obj_ak fts
typeObject getVarTy (Dereference obj ann) = do
  typed_obj <- typeObject getVarTy obj
  (_, obj_ty) <- getObjType typed_obj
  case obj_ty of
    TReference ak ty -> return $ SAST.Dereference typed_obj $ buildExpAnnObj ann ak ty
    ty              -> throwError $ annotateError ann $ EDereferenceInvalidType ty
typeObject _getVarTy (PAST.ArraySlice _obj _lower _upper anns) = do
  -- | TArray slices can only be used as part of a reference expression. If we are here,
  -- then the array slice is being used in an invalid context.
  throwError $ annotateError anns ESliceInvalidUse
typeObject getVarTy (DereferenceMemberAccess obj ident ann) = do
  typed_obj <- typeObject getVarTy obj
  (_, obj_ty) <- getObjType typed_obj
  case obj_ty of
    TReference ak rTy ->
      getMemberFieldType ann rTy ident >>=
        \fts -> return $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnObj ann ak fts
    ty -> throwError $ annotateError ann $ EDereferenceInvalidType ty

typeMemberFunctionCall ::
  ParserAnn
  -> TerminaType -- ^ type of the object
  -> Identifier -- ^ type of the member function to be called
  -> [Expression ParserAnn] -- ^ arguments
  -> SemanticMonad (([TerminaType], [SAST.Expression SemanticAnn]), TerminaType)
typeMemberFunctionCall ann obj_ty ident args =
  -- Calling a self method or viewer. We must not allow calling a procedure.
  case obj_ty of
    TGlobal _clsKind dident -> getGlobalTypeDef ann dident >>=
      \case{
        -- This case corresponds to a call to an inner method or viewer from the self object.
        Located (Class _ _identTy cls _provides _mods) _ ->
          case findClassProcedure ident cls of
            Just _ -> throwError $ annotateError ann EInvalidProcedureCallInsideMemberFunction
            Nothing ->
              case findClassViewerOrMethod ident cls of
                Just (ps, _, anns) -> do
                  let (psLen , asLen ) = (length ps, length args)
                  -- Check that the number of parameters are OK
                  when (psLen < asLen) (throwError $ annotateError ann (EMemberFunctionCallExtraArgs (ident, ps, location anns) (fromIntegral asLen)))
                  when (psLen > asLen) (throwError $ annotateError ann (EMemberFunctionCallMissingArgs (ident, ps, location anns) (fromIntegral asLen)))
                  typed_args <- zipWithM (\(p, idx) e -> 
                    catchMismatch ann (EMemberFunctionCallArgTypeMismatch (ident, p, location anns) idx)
                      (typeExpression (Just p) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
                  fty <- maybe (throwError $ annotateError Internal EUnboxingMemberFunctionType) return (getTypeSemAnn anns)
                  return ((ps, typed_args), fty)
                Nothing -> throwError $ annotateError ann (EMemberAccessNotFunction ident)
          ;
        _ -> throwError $ annotateError Internal EUnboxingClassType
      }
    TAccessPort (TInterface dident) -> getGlobalTypeDef ann dident >>=
      \case{
        Located (Interface _identTy cls _mods) _ ->
          case findInterfaceProcedure ident cls of
            Nothing -> throwError $ annotateError ann (EUnknownProcedure ident)
            Just (ps, anns) -> do
              let (psLen , asLen) = (length ps, length args)
              when (psLen < asLen) (throwError $ annotateError ann (EProcedureCallExtraArgs (ident, ps, location anns) (fromIntegral asLen)))
              when (psLen > asLen) (throwError $ annotateError ann (EProcedureCallMissingArgs (ident, ps, location anns) (fromIntegral asLen)))
              typed_args <- zipWithM (\(p, idx) e ->
                catchMismatch ann (EProcedureCallArgTypeMismatch (ident, p, location anns) idx)
                  (typeExpression (Just p) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
              return ((ps, typed_args), TUnit)
          ;
        _ -> throwError $ annotateError Internal EUnboxingClassType
      }
    TAccessPort (TAllocator ty_pool) ->
      case ident of
        "alloc" ->
          case args of
            [opt] -> do
              typed_arg <- catchMismatch ann (EProcedureCallArgTypeMismatch ("alloc", TReference Mutable (TOption (TBoxSubtype ty_pool)), Builtin) 0)
                (typeExpression (Just (TReference Mutable (TOption (TBoxSubtype ty_pool)))) typeRHSObject opt)
              return (([TReference Mutable (TOption (TBoxSubtype ty_pool))], [typed_arg]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("alloc", [TReference Mutable (TOption (TBoxSubtype ty_pool))], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("alloc", [TReference Mutable (TOption (TBoxSubtype ty_pool))], Builtin) (fromIntegral (length args)))
        "free" ->
          case args of
            [elemnt] -> do
              typed_arg <- catchMismatch ann (EProcedureCallArgTypeMismatch ("free", TBoxSubtype ty_pool, Builtin) 0)
                (typeExpression (Just (TBoxSubtype ty_pool)) typeRHSObject elemnt)
              return (([TBoxSubtype ty_pool], [typed_arg]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("free", [TBoxSubtype ty_pool], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("free", [TBoxSubtype ty_pool], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    TAccessPort (TAtomicAccess ty_atomic) ->
      case ident of
        "load" ->
          case args of
            [retval] -> do
              typed_arg <- catchMismatch ann (EProcedureCallArgTypeMismatch ("load", TReference Mutable ty_atomic, Builtin) 0)
                (typeExpression (Just (TReference Mutable ty_atomic)) typeRHSObject retval)
              return (([TReference Mutable ty_atomic], [typed_arg]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("load", [TReference Mutable ty_atomic], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("load", [TReference Mutable ty_atomic], Builtin) (fromIntegral (length args)))
        "store" ->
          case args of
            [value] -> do
              typed_value <- catchMismatch ann (EProcedureCallArgTypeMismatch ("store", ty_atomic, Builtin) 0)
                (typeExpression (Just ty_atomic) typeRHSObject value)
              return (([ty_atomic], [typed_value]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("store", [ty_atomic], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("store", [ty_atomic], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    TAccessPort (TAtomicArrayAccess ty_atomic _size) ->
      case ident of
        "load_index" ->
          case args of
            [index, retval] -> do
              typed_idx <- catchMismatch ann (EProcedureCallArgTypeMismatch ("load_index", TUSize, Builtin) 0)
                (typeExpression (Just TUSize) typeRHSObject index)
              typed_ref <- catchMismatch ann (EProcedureCallArgTypeMismatch ("load_index", TReference Mutable ty_atomic, Builtin) 1)
                (typeExpression (Just (TReference Mutable ty_atomic)) typeRHSObject retval)
              return (([TUSize, TReference Mutable ty_atomic], [typed_idx, typed_ref]), TUnit)
            _ -> if length args < 2 then
              throwError $ annotateError ann (EProcedureCallMissingArgs ("load_index", [TUSize, TReference Mutable ty_atomic], Builtin) (fromIntegral (length args)))
              else throwError $ annotateError ann (EProcedureCallExtraArgs ("load_index", [TUSize, TReference Mutable ty_atomic], Builtin) (fromIntegral (length args)))
        "store_index" ->
          case args of
            [index, retval] -> do
              typed_idx <- catchMismatch ann (EProcedureCallArgTypeMismatch ("store_index", TUSize, Builtin) 0)
                (typeExpression (Just TUSize) typeRHSObject index)
              typed_value <- catchMismatch ann (EProcedureCallArgTypeMismatch ("store_index", ty_atomic, Builtin) 1)
                (typeExpression (Just ty_atomic) typeRHSObject retval)
              return (([TUSize, ty_atomic], [typed_idx, typed_value]), TUnit)
            _ -> if length args < 2 then
              throwError $ annotateError ann (EProcedureCallMissingArgs ("store_index", [TUSize, ty_atomic], Builtin) (fromIntegral (length args)))
              else throwError $ annotateError ann (EProcedureCallExtraArgs ("store_index", [TUSize, ty_atomic], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    TOutPort ty ->
      case ident of
        -- send(T)
        "send" ->
          case args of
            [elemnt] -> do
              -- Type the first argument element
              typed_element <- catchMismatch ann (EOutboundPortArgTypeMismatch ty) (typeExpression (Just ty) typeRHSObject elemnt)
              return (([ty] , [typed_element]), TUnit)
            _ -> throwError $ annotateError ann (EOutboundPortSendInvalidNumArgs (fromIntegral $ length args))
        _ -> throwError $ annotateError ann $ EOutboundPortInvalidProcedure ident
    ty -> throwError $ annotateError ann (EMemberFunctionCallInvalidType ty)

----------------------------------------
-- These functions are useful:
-- typeLHSObject looks up in write local environment
-- typeRHSObject looks up in read+write local environment
-- tyeGlobalObject looks up only in the global environment
typeRHSObject, typeLHSObject, typeGlobalObject :: Object ParserAnn
  -> SemanticMonad (SAST.Object SemanticAnn)
typeRHSObject = typeObject getRHSVarTy
typeLHSObject = typeObject getLHSVarTy
typeGlobalObject = typeObject getGlobalVarTy
----------------------------------------

typeModifier :: Location -> Modifier -> SemanticMonad SAST.Modifier
typeModifier _loc (Modifier ident Nothing) =
  return $ SAST.Modifier ident Nothing
typeModifier loc (Modifier ident (Just constant)) = do
  typed_const <- typeConstant loc constant
  return $ SAST.Modifier ident (Just typed_const)

typeConstant :: Location -> Const -> SemanticMonad SAST.Const
typeConstant _loc (I tInt Nothing) = return $ SAST.I tInt Nothing
typeConstant loc (I tInt (Just ts)) = do
  ty <- typeTypeSpecifier loc ts
  return $ SAST.I tInt (Just ty)
typeConstant _loc (B tBool) = return $ SAST.B tBool
typeConstant _loc (C tChar) = return $ SAST.C tChar

typeConstExpression :: TerminaType -> Expression ParserAnn -> SemanticMonad (SAST.Expression SemanticAnn)
typeConstExpression expected_ty (Constant constant ann) = do
  typed_const <- typeConstant ann constant
  checkConstant ann expected_ty typed_const
  return $ SAST.Constant typed_const (buildExpAnn ann expected_ty)
typeConstExpression expected_ty (AccessObject (Variable identifier ann)) = do
  (ty, _value) <- getConst ann identifier
  sameTyOrError ann expected_ty ty
  return $ SAST.AccessObject (SAST.Variable identifier (buildExpAnnObj ann Immutable ty))
typeConstExpression _ _ = throwError $ annotateError Internal EExpressionNotConstant

evalConstExpression :: TerminaType -> Expression ParserAnn -> SemanticMonad SAST.Const
evalConstExpression expected_ty (Constant constant ann) = do
  typed_const <- typeConstant ann constant
  checkConstant ann expected_ty typed_const
  return typed_const
evalConstExpression expected_ty (AccessObject (Variable identifier ann)) = do
  (ty, value) <- getConst ann identifier
  sameTyOrError ann expected_ty ty
  return value
evalConstExpression _ _ = throwError $ annotateError Internal EExpressionNotConstant

typeAssignmentExpression ::
  TerminaType ->
  (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn)) ->
  Expression ParserAnn ->
  SemanticMonad (SAST.Expression SemanticAnn)
----------------------------------------
-- | Struct Initializer
typeAssignmentExpression expected_type@(TStruct id_ty) typeObj (StructInitializer fs mts pann) = do
  -- | Check field type
  case mts of
    (Just ts) -> do
      init_ty <- typeTypeSpecifier pann ts
      catchMismatch pann
        (EStructInitializerTypeMismatch expected_type)
        (sameTyOrError pann expected_type init_ty)
      getGlobalTypeDef pann id_ty
    Nothing -> getGlobalTypeDef pann id_ty
  >>= \case{
    Located (Struct _ ty_fs _mods) strLoc  ->
      SAST.StructInitializer
        <$> typeFieldAssignments pann (id_ty, strLoc) typeObj ty_fs fs
        <*> pure (Just id_ty)
        <*> pure (buildExpAnn pann (TStruct id_ty));
    _ -> throwError $ annotateError Internal EUnboxingStructType;
  }
typeAssignmentExpression expected_type@(TGlobal _ id_ty) typeObj (StructInitializer fs mts pann) = do
  -- | Check field type
  case mts of
    (Just ts) -> do
      init_ty <- typeTypeSpecifier pann ts
      catchMismatch pann
        (EStructInitializerTypeMismatch expected_type)
        (sameTyOrError pann expected_type init_ty)
      getGlobalTypeDef pann id_ty
    Nothing -> getGlobalTypeDef pann id_ty
  >>= \case{
    Located (Class clsKind _ident members _provides _mods) clsLoc ->
      let fields = [fld | (ClassField fld@(FieldDefinition {}) _) <- members] in
        SAST.StructInitializer
        <$> typeFieldAssignments pann (id_ty, clsLoc) typeObj fields fs
        <*> pure (Just id_ty)
        <*> pure (buildExpAnn pann (TGlobal clsKind id_ty));
    _ -> throwError $ annotateError Internal EUnboxingClassType;
  }
typeAssignmentExpression expected_type@(TEnum id_expected) typeObj (EnumVariantInitializer id_ty variant args pann) = do
  unless (id_expected == id_ty) (throwError $ annotateError pann (EEnumInitializerExpectedTypeMismatch expected_type (TEnum id_ty)))
  -- | Enum Variant
  getEnumTy pann id_ty
  >>= \case {
    Located (Enum enumId ty_vs _mods) loc ->
      case Data.List.find ((variant ==) . variantIdentifier) ty_vs of
        Nothing -> throwError $ annotateError pann (EEnumVariantNotFound enumId variant)
        Just (EnumVariant _ ps) ->
          let (psLen , asLen ) = (length ps, length args) in
          if psLen == asLen
          then
             flip (SAST.EnumVariantInitializer id_ty variant) (buildExpAnn pann (TEnum id_ty))
              <$> zipWithM (\(ty, position) e ->
                catchMismatch pann (EEnumVariantParamTypeMismatch (enumId, loc) (variant, position, ty)) (typeExpression (Just ty) typeObj e)) (zip ps [0 :: Integer ..]) args
          else if psLen < asLen
          then throwError $ annotateError pann (EEnumVariantExtraParams (enumId, loc) (variant, ps) (fromIntegral asLen))
          else throwError $ annotateError pann (EEnumVariantMissingParams (enumId, loc) (variant, ps) (fromIntegral asLen))
     ;
    _ -> throwError $ annotateError Internal EUnboxingEnumType
  }
typeAssignmentExpression expectedType typeObj (ArrayInitializer iexp size pann) = do
-- | TArray Initialization
  case expectedType of
    TArray ts arrsize -> do
      -- | We do not need to catch any error, since it will be correctly handler
      -- by the recursive call to |typeAssignmentExpression|
      typed_init <- typeAssignmentExpression ts typeObj iexp
      unless (size == arrsize) (throwError $ annotateError pann (EArrayInitializerSizeMismatch arrsize size))
      return $ SAST.ArrayInitializer typed_init size (buildExpAnn pann (TArray ts size))
    ts -> throwError $ annotateError pann (EArrayInitializerNotArray ts)
typeAssignmentExpression expectedType typeObj (ArrayExprListInitializer exprs pann) = do
  case expectedType of
    TArray ts arrsize -> do
      typed_exprs <- mapM (\e ->
        catchMismatch (getAnnotation e)
          (EArrayExprListInitializerExprTypeMismatch ts) (typeExpression (Just ts) typeObj e)) exprs
      size_value <- getIntSize pann arrsize
      unless (length typed_exprs == fromIntegral size_value)
        (throwError $ annotateError pann (EArrayExprListInitializerSizeMismatch size_value (fromIntegral $ length typed_exprs)))
      return $ SAST.ArrayExprListInitializer typed_exprs (buildExpAnn pann (TArray ts arrsize))
    ts -> throwError $ annotateError pann (EArrayExprListInitializerNotArray ts)
typeAssignmentExpression expectedType typeObj (OptionVariantInitializer vexp anns) =
  case expectedType of
    TOption ts -> do
      case vexp of
        None -> return $ SAST.OptionVariantInitializer None (buildExpAnn anns (TOption ts))
        Some e -> do
          typed_e <- typeExpression (Just ts) typeObj e
          return $ SAST.OptionVariantInitializer (Some typed_e) (buildExpAnn anns (TOption ts))
    _ -> throwError $ annotateError anns EOptionVariantInitializerInvalidUse
typeAssignmentExpression expectedType typeObj expr = do
  let loc = getAnnotation expr
  typed_expr <- catchMismatch (getAnnotation expr) (EAssignmentExprMismatch expectedType) (typeExpression (Just expectedType) typeObj expr)
  expr_type <- getExprType typed_expr
  catchExpectedCopy loc EInvalidAssignmentExprType (copyTyOrFail loc expr_type)
  return typed_expr

-- | Function |typeExpression| takes an expression from the parser, traverse it
-- annotating each node with its type.
-- Since we are also creating new nodes (|Unbox| annotations), instead of just
-- traversing, we are actually /creating/ a new tree with implicit
-- constructions.
typeExpression ::
  -- | Expected type of the expression
  Maybe TerminaType ->
  -- | Function used to type objects (depends on the scope)
  (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -- | Expression to type
  -> Expression ParserAnn
  -> SemanticMonad (SAST.Expression SemanticAnn)
-- Object access
typeExpression expectedType typeObj (AccessObject obj) = do
    -- | Type the object
    typed_obj <- typeObj obj
    -- | Get the type of the object
    (_, obj_type) <- getObjType typed_obj
    case (expectedType, obj_type) of
      (Just (TBoxSubtype ts), TBoxSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        sameTyOrError (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      (Just ts, TBoxSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        sameTyOrError (getAnnotation obj) ts ts'
        -- If we have an box and expect an unboxed type:
        return $ SAST.AccessObject (unBox typed_obj)
      (Just ts, ts') -> do
        -- If the type must be an expected type, then check it.
        sameTyOrError (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      -- If we are requesting an object without an expected type, then we must
      -- be casting the result. Thus, we must return an unboxed object.
      (Nothing, TBoxSubtype _)->
        return $ SAST.AccessObject (unBox typed_obj)
      (Nothing, _) ->
        return $ SAST.AccessObject typed_obj
-- | Constant literals with an expected type.
typeExpression (Just expectedType) _ (Constant c pann) = do
  typed_c <- typeConstant pann c
  -- | Call the function that checks that the constant is of the expected type.
  checkConstant pann expectedType typed_c
  return $ SAST.Constant typed_c (buildExpAnn pann expectedType)
-- | Integer literals without an expected type but with a known type.
typeExpression Nothing _ (Constant c@(I _ (Just ts)) pann) = do
  ty <- typeTypeSpecifier pann ts
  typed_c <- typeConstant pann c
  return $ SAST.Constant typed_c (buildExpAnn pann ty)
-- | Integer literals without an expected type and without a known type.
-- This is an error, since we cannot infer the type of the constant.
typeExpression Nothing _ (Constant (I tInt Nothing) pann) = do
  throwError $ annotateError pann $ EConstantWithoutKnownType (SAST.I tInt Nothing)
-- | Boolean literals without an expected type.
typeExpression Nothing _ (Constant c@(B {}) pann) = do
  typed_c <- typeConstant pann c
  return $ SAST.Constant typed_c (buildExpAnn pann TBool)
-- | Character literals without an expected type.
typeExpression Nothing _ (Constant c@(C {}) pann) = do
  typed_c <- typeConstant pann c
  return $ SAST.Constant typed_c (buildExpAnn pann TChar)
typeExpression expectedType typeObj (Casting e nts pann) = do
  nty <- typeTypeSpecifier pann nts
  maybe (return ()) (sameTyOrError pann nty) expectedType
  -- | Casting Expressions.
  typed_exp <- typeExpression Nothing typeObj e
  type_exp <- getExprType typed_exp
  if casteableTys type_exp nty
  then return (SAST.Casting typed_exp nty (buildExpAnn pann nty))
  else throwError (annotateError pann $ ENotCasteable type_exp nty)
typeExpression expectedType typeObj (BinOp op le re pann) = do

  case op of
    Addition -> sameNumType
    Subtraction -> sameNumType
    Multiplication -> sameNumType
    Division -> sameNumType
    Modulo -> sameNumType
    BitwiseLeftShift -> leftNumRightPosType
    BitwiseRightShift -> leftNumRightPosType
    RelationalEqual -> sameEquatableTyBool
    RelationalNotEqual -> sameEquatableTyBool
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

    -- | This helper function checks that the type of the lhs and the rhs is
    -- the same. It also applies a function to check that the type is valid.
    sameTypeExpressions ::
      (TerminaType -> Bool)
      -- | Left hand side error constructor
      -> (TerminaType -> Error)
      -- | Right hand side error constructor
      -> (TerminaType -> Error)
      -- | Left hand side expression
      -> Expression ParserAnn
      -- | Right hand side expression
      -> Expression ParserAnn
      -- | Typed expressions
      -> SemanticMonad (TerminaType, SAST.Expression SemanticAnn, SAST.Expression SemanticAnn)
    sameTypeExpressions isValid lerror rerror lnume rnume = do
      tyle <- catchError
        (typeExpression Nothing typeObj lnume)
        (\err -> case getError err of
          -- | If the type of the left hand side is unknown, then we must
          -- check the right hand side. This could be implemented in a more
          -- efficient way, but for now, we will check the right hand side and
          -- then check it again.
          EConstantWithoutKnownType _ -> do
            tyre <- typeExpression Nothing typeObj rnume
            tyre_ty <- getExprType tyre
            unless (isValid tyre_ty) (throwError $ annotateError (getAnnotation rnume) (rerror tyre_ty))
            catchMismatch pann (EBinOpTypeMismatch op tyre_ty) (typeExpression (Just tyre_ty) typeObj lnume)
          _ -> throwError err)
      tyle_ty <- getExprType tyle
      unless (isValid tyle_ty) (throwError $ annotateError (getAnnotation lnume) (lerror tyle_ty))
      tyre <- catchMismatch pann (EBinOpTypeMismatch op tyle_ty) (typeExpression (Just tyle_ty) typeObj rnume)
      return (tyle_ty, tyle, tyre)

    -- | This function checks the that the lhs and the rhs are both
    -- equal to the expected type (if any) and that the expected type is
    -- a numeric type. This function is used to check the
    -- binary expressions multiplication, division, addition and subtraction.
    sameNumType :: SemanticMonad (SAST.Expression SemanticAnn)
    sameNumType =
      case expectedType of
        ty@(Just ty') -> do
          unless (numTy ty') (throwError $ annotateError pann (EBinOpExpectedTypeNotNum op ty'))
          tyle <- catchMismatch (getAnnotation le) (EBinOpExpectedTypeLeft op ty')
            (typeExpression ty typeObj le)
          tyre <- catchMismatch (getAnnotation re) (EBinOpExpectedTypeRight op ty')
            (typeExpression ty typeObj re)
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          (ty, tyle, tyre) <- sameTypeExpressions numTy (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty)

    -- | This function checks that the lhs is equal to the expected type (if any)
    -- and that that type is numeric. The rhs must be a positive (i.e. unsigned) type.
    -- This function is used to check the binary expressions bitwise left shift and
    -- bitwise right shift.
    leftNumRightPosType :: SemanticMonad (SAST.Expression SemanticAnn)
    leftNumRightPosType =
      case expectedType of
        ty@(Just ty') -> do
          unless (numTy ty') (throwError $ annotateError pann (EBinOpExpectedTypeNotNum op ty'))
          tyle <- catchMismatch (getAnnotation le) (EBinOpExpectedTypeLeft op ty')
            (typeExpression ty typeObj le)
          tyre <- typeExpression Nothing typeObj re
          tyre_ty <- getExprType tyre
          unless (posTy tyre_ty) (throwError $ annotateError pann (EBinOpRightTypeNotPos op tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          tyle <- typeExpression Nothing typeObj le
          tyre <- typeExpression Nothing typeObj re
          tyle_ty <- getExprType tyle
          tyre_ty <- getExprType tyre
          unless (numTy tyle_ty) (throwError $ annotateError pann (EBinOpLeftTypeNotNum op tyle_ty))
          unless (posTy tyre_ty) (throwError $ annotateError pann (EBinOpRightTypeNotPos op tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann tyle_ty)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is equatable. This function is used to check the binary expressions == and !=.
    sameEquatableTyBool :: SemanticMonad (SAST.Expression SemanticAnn)
    sameEquatableTyBool =
      case expectedType of
        (Just TBool) -> do
          (_, tyle, tyre) <-
            sameTypeExpressions eqTy
              (EBinOpLeftTypeNotEq op) (EBinOpRightTypeNotEq op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          (_, tyle, tyre) <-
            sameTypeExpressions eqTy
              (EBinOpLeftTypeNotEq op) (EBinOpRightTypeNotEq op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is numeric. This function is used to check the binary expressions <, <=, > and >=.
    sameNumTyBool :: SemanticMonad (SAST.Expression SemanticAnn)
    sameNumTyBool =
      case expectedType of
        (Just TBool) -> do
          (_, tyle, tyre) <-
            sameTypeExpressions numTy
              (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          (_, tyle, tyre) <-
            sameTypeExpressions numTy
              (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is boolean. This function is used to check the binary expressions && and ||.
    sameBoolType :: SemanticMonad (SAST.Expression SemanticAnn)
    sameBoolType =
      case expectedType of
        (Just TBool) -> do
          tyle <- catchMismatch pann (EBinOpLeftTypeNotBool op) (typeExpression (Just TBool) typeObj le)
          tyre <- catchMismatch pann (EBinOpRightTypeNotBool op) (typeExpression (Just TBool) typeObj re)
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          tyle <- catchMismatch pann (EBinOpLeftTypeNotBool op) (typeExpression (Just TBool) typeObj le)
          tyre <- catchMismatch pann (EBinOpRightTypeNotBool op) (typeExpression (Just TBool) typeObj re)
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)
typeExpression expectedType typeObj (ReferenceExpression refKind rhs_e pann) =
  case rhs_e of
    PAST.ArraySlice obj lower upper _anns -> do
      typed_obj <- typeObj obj
      (obj_ak, obj_ty) <- getObjType typed_obj
      typed_lower <- catchMismatch (getAnnotation lower) EArraySliceLowerBoundNotUSize (typeExpression (Just TUSize) typeRHSObject lower)
      typed_upper <- catchMismatch (getAnnotation upper) EArraySliceUpperBoundNotUSize (typeExpression (Just TUSize) typeRHSObject upper)
      case obj_ty of
        TArray ty _ -> do
          checkReferenceAccessKind obj_ak
          case expectedType of
            Just rtype@(TReference _ak (TArray ts _size)) -> do
              unless (sameTy ty ts) (throwError $ annotateError pann $ EMismatch ts ty)
              return (SAST.ArraySliceExpression refKind typed_obj typed_lower typed_upper (buildExpAnn pann rtype))
            Just ety -> throwError $ annotateError pann $ EMismatch (TReference refKind ty) ety
            _ -> throwError $ annotateError pann ESliceInvalidUse
        _ -> throwError $ annotateError Internal EMalformedSlice
    _ -> do
      -- | Type object
      typed_obj <- typeObj rhs_e
      -- | Get the type of the object
      (obj_ak, obj_type) <- getObjType typed_obj
      case obj_type of
        -- | If the object is of a box subtype, then the reference will be to an object of
        -- the base type. Objects of a box subtype are always immutable, BUT a reference
        -- to an object of a box subtype can be mutable. Thus, we do not need to check
        -- the access kind of the object.
        TBoxSubtype ty -> do
          -- | Check that the expected type is the same as the base type.  
          maybe (return ()) (sameTyOrError pann (TReference refKind ty)) expectedType
          return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (TReference refKind ty)))
        _ -> do
          -- | Check if the we are allowed to create that kind of reference from the object
          checkReferenceAccessKind obj_ak
          maybe (return ()) (flip (sameTyOrError pann) (TReference refKind obj_type)) expectedType
          return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (TReference refKind obj_type)))

  where

    checkReferenceAccessKind :: AccessKind -> SemanticMonad ()
    checkReferenceAccessKind obj_ak =
      case (obj_ak, refKind) of
        (Immutable, Mutable) -> throwError $ annotateError pann EMutableReferenceToImmutable
        (Private, Mutable) -> throwError $ annotateError pann EMutableReferenceToPrivate
        _ -> return ()

---------------------------------------
-- | Function Expression.  A tradicional function call
typeExpression expectedType _ (FunctionCall ident args ann) = do
  (ps, retty, funcLocation) <- getFunctionTy ann ident
  let expAnn = buildExpAnnApp ann ps retty
      (psLen , asLen) = (length ps, length args)
  -- Check that the number of parameters are OK
  when (psLen < asLen) (throwError $ annotateError ann (EFunctionCallExtraArgs (ident, ps, funcLocation) (fromIntegral asLen)))
  when (psLen > asLen) (throwError $ annotateError ann (EFunctionCallMissingArgs (ident, ps, funcLocation) (fromIntegral asLen)))
  typed_args <- zipWithM (\(p, idx) e -> catchMismatch ann (EFunctionCallArgTypeMismatch (ident, p, funcLocation) idx)
      (typeExpression (Just p) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
  maybe (return ()) (sameTyOrError ann retty) expectedType
  return $ SAST.FunctionCall ident typed_args expAnn

----------------------------------------
typeExpression expectedType typeObj (MemberFunctionCall obj ident args ann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjType obj_typed
  ((ps, typed_args), fty) <- typeMemberFunctionCall ann obj_ty ident args
  maybe (return ()) (sameTyOrError ann fty) expectedType
  return $ SAST.MemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)
typeExpression expectedType typeObj (DerefMemberFunctionCall obj ident args ann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjType obj_typed
  case obj_ty of
    TReference _ rTy -> do
      -- NOTE: We have reused the code from MemberFunctionCall, but we must take into
      -- account that, for the time being, when we are accessing a member function through
      -- a reference, the object (self) can only be of a user-defined class type. There
      -- cannot be references to ports. 
      ((ps, typed_args), fty) <- typeMemberFunctionCall ann rTy ident args
      maybe (return ()) (sameTyOrError ann fty) expectedType
      return $ SAST.DerefMemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)
    ty -> throwError $ annotateError ann $ EDereferenceInvalidType ty
typeExpression expectedType typeObj (IsEnumVariantExpression obj id_ty variant_id pann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjType obj_typed
  Located lhs_ty _ <- case obj_ty of
    TEnum lhs_id -> do
      unless (lhs_id == id_ty) (throwError $ annotateError pann (EIsVariantEnumTypeMismatch lhs_id id_ty))
      getEnumTy pann id_ty
    _ -> throwError $ annotateError pann (EIsVariantInvalidType obj_ty)
  case lhs_ty of
    Enum lhs_enum ty_vs _mods ->
      case Data.List.find ((variant_id ==) . variantIdentifier) ty_vs of
        Just (EnumVariant {}) -> do
          maybe (return ()) (sameTyOrError pann TBool) expectedType
          return $ SAST.IsEnumVariantExpression obj_typed id_ty variant_id (buildExpAnn pann TBool)
        Nothing -> throwError $ annotateError pann (EEnumVariantNotFound lhs_enum variant_id)
    _ -> throwError $ annotateError Internal EUnboxingEnumType
typeExpression expectedType typeObj (IsOptionVariantExpression obj variant_id pann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjType obj_typed
  case obj_ty of
    (TOption {}) -> do
      maybe (return ()) (sameTyOrError pann TBool) expectedType
      return $ SAST.IsOptionVariantExpression obj_typed variant_id (buildExpAnn pann TBool)
    _ -> throwError $ annotateError pann (EIsOptionVariantInvalidType obj_ty)
typeExpression _ _ (StructInitializer _ _ pann) = throwError $ annotateError pann EStructInitializerInvalidUse
typeExpression _ _ (ArrayInitializer _ _ pann) = throwError $ annotateError pann EArrayInitializerInvalidUse
typeExpression _ _ (ArrayExprListInitializer _ pann) = throwError $ annotateError pann EArrayExprListInitializerInvalidUse
typeExpression _ _ (OptionVariantInitializer _ pann) = throwError $ annotateError pann EOptionVariantInitializerInvalidUse
typeExpression _ _ (EnumVariantInitializer _ _ _ pann) = throwError $ annotateError pann EEnumVariantInitializerInvalidUse

typeFieldAssignment
  :: Location -> (Identifier, Location)
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> SAST.FieldDefinition
  -> FieldAssignment ParserAnn
  -> SemanticMonad (SAST.FieldAssignment SemanticAnn)
typeFieldAssignment loc tyDef typeObj (FieldDefinition fid fty) (FieldValueAssignment faid faexp pann) =
  if fid == faid
  then
    flip (SAST.FieldValueAssignment faid) (buildStmtAnn pann) <$> typeAssignmentExpression fty typeObj faexp
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [faid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty) (FieldAddressAssignment faid addr pann) =
  if fid == faid
  then
    case fty of
      TFixedLocation _ -> return $ SAST.FieldAddressAssignment faid addr (buildExpAnn pann fty)
      ty -> throwError $ annotateError loc (EFieldNotFixedLocation fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [faid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty) (FieldPortConnection InboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry loc sid >>=
    \gentry ->
    case fty of
      TSinkPort ty action  ->
        case gentry of
          Located  (GGlob ets@(TGlobal EmitterClass clsId)) _ -> do
            checkEmitterDataType loc clsId ty
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildSinkPortConnAnn pann ets action)
          _ -> throwError $ annotateError loc $ ESinkPortConnectionInvalidGlobal sid
      TInPort ty action  ->
        case gentry of
          Located (GGlob cts@(TMsgQueue ty' _)) _ -> do
            catchMismatch pann (EInboundPortConnectionMsgQueueTypeMismatch sid ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildInPortConnAnn pann cts action)
          _ -> throwError $ annotateError loc $ EInboundPortConnectionInvalidObject sid
      ty -> throwError $ annotateError loc (EFieldNotSinkOrInboundPort fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [pid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty) (FieldPortConnection OutboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry loc sid >>=
    \gentry ->
    case fty of
      TOutPort ty ->
        case gentry of
          Located (GGlob cts@(TMsgQueue ty' _)) _ -> do
            catchMismatch pann (EOutboundPortConnectionMsgQueueTypeMismatch sid ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection OutboundPortConnection pid sid (buildOutPortConnAnn pann cts)
          _ -> throwError $ annotateError loc $ EOutboundPortConnectionInvalidGlobal sid
      ty -> throwError $ annotateError loc (EFieldNotOutboundPort fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [pid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty) (FieldPortConnection AccessPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry loc sid >>=
    \gentry ->
    case fty of
      TAccessPort (TAllocator ty) ->
        case gentry of
          Located (GGlob (TPool ty' s)) _ -> do
            catchMismatch pann (EAllocatorPortConnectionPoolTypeMismatch sid ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildPoolConnAnn pann ty s)
          _ -> throwError $ annotateError loc $ EAllocatorPortConnectionInvalidGlobal sid
      TAccessPort (TAtomicAccess ty) ->
        case gentry of
          Located (GGlob (TAtomic ty')) _ -> do
            catchMismatch pann (EAtomicConnectionTypeMismatch ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicConnAnn pann ty)
          _ -> throwError $ annotateError loc $ EAtomicAccessPortConnectionInvalidGlobal sid
      TAccessPort (TAtomicArrayAccess ty s) ->
        case gentry of
          Located (GGlob (TAtomicArray ty' s')) _ -> do
            catchMismatch pann (EAtomicArrayConnectionTypeMismatch ty) (sameTyOrError loc ty ty')
            unless (s == s') (throwError $ annotateError pann $ EAtomicArrayConnectionSizeMismatch s s')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicArrayConnAnn pann ty s)
          _ -> throwError $ annotateError loc $ EAtomicArrayAccessPortConnectionInvalidGlobal sid
      TAccessPort (TInterface iface) ->
        getGlobalTypeDef loc iface >>=
          \case {
            Located (Interface _ members _) _ ->
              -- Check that the resource provides the interface
              case gentry of
                Located (GGlob rts@(TGlobal ResourceClass clsId)) _ ->
                  let procs = [ProcedureSeman procid (map paramType params) | (InterfaceProcedure procid params _) <- members] in
                  getGlobalTypeDef loc clsId >>=
                  \case {
                      Located (Class _ _ _ provides _) _ ->
                        case Data.List.find (iface ==) provides of
                          Just _ -> return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAccessPortConnAnn pann rts procs)
                          _ -> throwError $ annotateError loc $ EAccessPortConnectionInterfaceNotProvided sid iface
                        ;
                      _ -> throwError $ annotateError Internal EUnboxingInterface
                  }
                _ -> throwError $ annotateError loc $ EAccessPortConnectionInvalidGlobal sid
              ;
            _ -> throwError $ annotateError Internal EUnboxingInterface
          }
      ty -> throwError $ annotateError loc (EFieldNotAccessPort fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [pid])

typeFieldAssignments
  :: Location -> (Identifier, Location)
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> [SAST.FieldDefinition]
  -> [FieldAssignment ParserAnn]
  -> SemanticMonad [SAST.FieldAssignment SemanticAnn]
typeFieldAssignments faLoc tyDef typeObj fds fas = checkSortedFields sorted_fds sorted_fas []
  where
    tError = throwError . annotateError faLoc
    getFid = \case {
      FieldValueAssignment fid _ _ -> fid;
      FieldAddressAssignment fid _ _ -> fid;
      FieldPortConnection _ fid _ _ -> fid;
    }
    sorted_fds = Data.List.sortOn fieldIdentifier fds
    sorted_fas = Data.List.sortOn getFid fas
    -- Same length monadic Zipwith
    checkSortedFields [] [] xs = return $ reverse xs
    checkSortedFields [] es _ = tError (EFieldValueAssignmentUnknownFields tyDef (fmap getFid es))
    checkSortedFields ms [] _ = tError (EFieldValueAssignmentMissingFields tyDef (fmap fieldIdentifier ms))
    checkSortedFields (d:ds) (a:as) acc =
      typeFieldAssignment faLoc tyDef typeObj d a >>= checkSortedFields ds as . (:acc)
