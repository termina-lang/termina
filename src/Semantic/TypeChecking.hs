-- | Semantic Analysis Module i.e. Type checking
-- This module defines a mapping from |AST Parser.ParserAnn|
-- to |AST SemanticAnnotations|, | SemanAST {ParserInfo , TypeInfo}|

module Semantic.TypeChecking where

-- Termina Ast and Utils
import Utils.Annotations
import AST.Parser as PAST
import Utils.AST.Parser
import Utils.AST.Core
import Utils.TypeSpecifier

-- Top Sort
import qualified Data.Map.Strict as M

-- Termina Semantic AST
import qualified AST.Seman as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
-- Error module
import Control.Monad.Except (MonadError (..), runExceptT)
import Semantic.Errors.Errors
-- Semantic Monad
import Semantic.Monad

import Extras.TopSort

----------------------------------------
-- Libaries and stuff

import qualified Data.List  (find, map, nub, sortOn, (\\))
import Data.Maybe

-- import Control.Monad.State as ST
import Control.Monad
import qualified Control.Monad.State.Strict as ST
import qualified Parser.Types as Parser


getMemberFieldType :: Parser.ParserAnn -> TypeSpecifier -> Identifier -> SemanticMonad TypeSpecifier
getMemberFieldType ann obj_ty ident =
  case obj_ty of
    Location obj_ty' -> getMemberFieldType ann obj_ty' ident
    DefinedType dident -> getGlobalTypeDef Internal dident >>=
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


typeObject ::
  -- | Scope of variables. It returns its access kind (mutable or immutable) and its type
  (Parser.ParserAnn -> Identifier -> SemanticMonad (AccessKind, TypeSpecifier))
  -- The object to type
  -> Object Parser.ParserAnn
  -> SemanticMonad (SAST.Object SemanticAnn)
typeObject getVarTy (Variable ident ann) = do
  (ak, ty) <- getVarTy ann ident
  return $ SAST.Variable ident (buildExpAnnObj ann ak ty)
typeObject getVarTy (ArrayIndexExpression obj idx ann) = do
  typed_obj <- typeObject getVarTy obj
  (obj_ak, obj_ty) <- getObjectType typed_obj
  case obj_ty of
    Array ty_elems _vexp -> do
        idx_typed  <- typeExpression (Just USize) typeRHSObject idx
        return $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann obj_ak ty_elems
    Reference ref_ak (Array ty_elems _vexp) -> do
        idx_typed  <- typeExpression (Just USize) typeRHSObject idx
        return $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann ref_ak ty_elems
    ty -> throwError $ annotateError ann (EArray ty)
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
    (ak, ts) <- getLocalObjTy loc ident'
    return (ak, ts)) obj
  (obj_ak', obj_ty') <- getObjectType typed_obj'
  let (typed_obj, obj_ak, obj_ty) =
        maybe (typed_obj', obj_ak', obj_ty') (unBox typed_obj', Mutable, ) (isBox obj_ty')
  fts <- getMemberFieldType ann obj_ty ident
  return $ SAST.MemberAccess typed_obj ident $ buildExpAnnObj ann obj_ak fts
typeObject getVarTy (Dereference obj ann) = do
  typed_obj <- typeObject getVarTy obj
  (_, obj_ty) <- getObjectType typed_obj
  case obj_ty of
    Reference ak ty -> return $ SAST.Dereference typed_obj $ buildExpAnnObj ann ak ty
    ty              -> throwError $ annotateError ann $ ETypeNotReference ty
typeObject _getVarTy (ArraySlice _obj _lower _upper anns) = do
  -- | Array slices can only be used as part of a reference expression. If we are here,
  -- then the array slice is being used in an invalid context.
  throwError $ annotateError anns ESliceInvalidUse
typeObject getVarTy (DereferenceMemberAccess obj ident ann) = do
  typed_obj <- typeObject getVarTy obj
  (_, obj_ty) <- getObjectType typed_obj
  case obj_ty of
    Reference ak refTy ->
      getMemberFieldType ann refTy ident >>=
        \fts -> return $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnObj ann ak fts
    ty -> throwError $ annotateError ann $ ETypeNotReference ty

typeMemberFunctionCall ::
  Parser.ParserAnn
  -> TypeSpecifier
  -> Identifier
  -> [Parser.Expression Parser.ParserAnn]
  -> SemanticMonad (([Parameter], [SAST.Expression SemanticAnn]), TypeSpecifier)
typeMemberFunctionCall ann obj_ty ident args =
  -- Calling a self method or viewer. We must not allow calling a procedure.
  case obj_ty of
    DefinedType dident -> getGlobalTypeDef ann dident >>=
      \case{
        -- This case corresponds to a call to an inner method or viewer from the self object.
        Class _ _identTy cls _provides _mods ->
          case findClassProcedure ident cls of
            Just _ -> throwError $ annotateError ann (EMemberAccessInvalidProcedureCall ident)
            Nothing ->
              case findClassViewerOrMethod ident cls of
                Just (ps, _, anns) -> do
                  let (psLen , asLen ) = (length ps, length args)
                  -- Check that the number of parameters are OK
                  when (psLen < asLen) (throwError $ annotateError ann EMemberMethodExtraParams)
                  when (psLen > asLen) (throwError $ annotateError ann EMemberMethodMissingParams)
                  typed_args <- zipWithM (\p e -> typeExpression (Just (paramTypeSpecifier p)) typeRHSObject e) ps args
                  fty <- maybe (throwError $ annotateError Internal EMemberMethodType) return (getTypeSemAnn anns)
                  return ((ps, typed_args), fty)
                Nothing -> throwError $ annotateError ann (EMemberAccessNotFunction ident)
          ;
        -- Other user-defined types do not define methods (yet?)
        ty -> throwError $ annotateError ann (EMemberFunctionUDef (fmap forgetSemAnn ty))
      }
    AccessPort (DefinedType dident) -> getGlobalTypeDef ann dident >>=
      \case{
         Interface _identTy cls _mods ->
         case findInterfaceProcedure ident cls of
           Nothing -> throwError $ annotateError ann (EUnknownProcedure ident)
           Just (ps, anns) -> do
              let (psLen , asLen) = (length ps, length args)
              when (psLen < asLen) (throwError $ annotateError ann (EProcedureCallExtraParams (ident, ps, location anns) (fromIntegral asLen)))
              when (psLen > asLen) (throwError $ annotateError ann (EProcedureCallMissingParams (ident, ps, location anns) (fromIntegral asLen)))
              typed_args <- zipWithM (\p e -> 
                catchMismatch ann (EProcedureCallParamTypeMismatch (ident, p, location anns)) 
                  (typeExpression (Just (paramTypeSpecifier p)) typeRHSObject e)) ps args
              return ((ps, typed_args), Unit)
         ;
         -- Other User defined types do not define methods
         ty -> throwError $ annotateError ann (EMemberFunctionUDef (fmap forgetSemAnn ty))
      }
    AccessPort (Allocator ty_pool) ->
      case ident of
        "alloc" ->
          case args of
            [opt] -> do
              typed_arg <- catchMismatch ann (EProcedureCallParamTypeMismatch ("alloc", Parameter "opt" (Reference Mutable (Option (BoxSubtype ty_pool))), Builtin))
                (typeExpression (Just (Reference Mutable (Option (BoxSubtype ty_pool)))) typeRHSObject opt)
              return (([Parameter "opt" (Reference Mutable (Option (BoxSubtype ty_pool)))], [typed_arg]), Unit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingParams ("alloc", [Parameter "opt" (Reference Mutable (Option (BoxSubtype ty_pool)))], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraParams ("alloc", [Parameter "opt" (Reference Mutable (Option (BoxSubtype ty_pool)))], Builtin) (fromIntegral (length args)))
        "free" ->
          case args of
            [elemnt] -> do
              typed_arg <- catchMismatch ann (EProcedureCallParamTypeMismatch ("free", Parameter "element" (BoxSubtype ty_pool), Builtin))
                (typeExpression (Just (BoxSubtype ty_pool)) typeRHSObject elemnt)
              return (([Parameter "element" (BoxSubtype ty_pool)], [typed_arg]), Unit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingParams ("free", [Parameter "element" (BoxSubtype ty_pool)], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraParams ("free", [Parameter "element" (BoxSubtype ty_pool)], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    AccessPort (AtomicAccess ty_atomic) ->
      case ident of
        "load" ->
          case args of
            [retval] -> do
              typed_arg <- catchMismatch ann (EProcedureCallParamTypeMismatch ("load", Parameter "retval" (Reference Mutable ty_atomic), Builtin))
                (typeExpression (Just (Reference Mutable ty_atomic)) typeRHSObject retval)
              return (([Parameter "retval" (Reference Mutable ty_atomic)], [typed_arg]), Unit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingParams ("load", [Parameter "retval" (Reference Mutable ty_atomic)], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraParams ("load", [Parameter "retval" (Reference Mutable ty_atomic)], Builtin) (fromIntegral (length args)))
        "store" ->
          case args of
            [value] -> do
              typed_value <- catchMismatch ann (EProcedureCallParamTypeMismatch ("store", Parameter "value" ty_atomic, Builtin))
                (typeExpression (Just ty_atomic) typeRHSObject value)
              return (([Parameter "value" ty_atomic], [typed_value]), Unit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingParams ("store", [Parameter "value" ty_atomic], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraParams ("store", [Parameter "value" ty_atomic], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    AccessPort (AtomicArrayAccess ty_atomic _size) ->
      case ident of
        "load_index" ->
          case args of
            [index, retval] -> do
              typed_idx <- catchMismatch ann (EProcedureCallParamTypeMismatch ("load_index", Parameter "index" USize, Builtin))
                (typeExpression (Just USize) typeRHSObject index)
              typed_ref <- catchMismatch ann (EProcedureCallParamTypeMismatch ("load_index", Parameter "retval" (Reference Mutable ty_atomic), Builtin))
                (typeExpression (Just (Reference Mutable ty_atomic)) typeRHSObject retval)
              return (([Parameter "index" USize, Parameter "retval" (Reference Mutable ty_atomic)], [typed_idx, typed_ref]), Unit)
            _ -> if length args < 2 then 
              throwError $ annotateError ann (EProcedureCallMissingParams ("load_index", [Parameter "index" USize, Parameter "retval" (Reference Mutable ty_atomic)], Builtin) (fromIntegral (length args)))
              else throwError $ annotateError ann (EProcedureCallExtraParams ("load_index", [Parameter "index" USize, Parameter "retval" (Reference Mutable ty_atomic)], Builtin) (fromIntegral (length args)))
        "store_index" ->
          case args of
            [index, retval] -> do
              typed_idx <- catchMismatch ann (EProcedureCallParamTypeMismatch ("store_index", Parameter "index" USize, Builtin))
                (typeExpression (Just USize) typeRHSObject index)
              typed_value <- catchMismatch ann (EProcedureCallParamTypeMismatch ("store_index", Parameter "value" ty_atomic, Builtin))
                (typeExpression (Just ty_atomic) typeRHSObject retval)
              return (([Parameter "index" USize, Parameter "value" ty_atomic], [typed_idx, typed_value]), Unit)
            _ -> if length args < 2 then 
              throwError $ annotateError ann (EProcedureCallMissingParams ("store_index", [Parameter "index" USize, Parameter "value" ty_atomic], Builtin) (fromIntegral (length args)))
              else throwError $ annotateError ann (EProcedureCallExtraParams ("store_index", [Parameter "index" USize, Parameter "value" ty_atomic], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    OutPort ty ->
      case ident of
        -- send(T)
        "send" ->
          case args of
            [elemnt] -> do
              -- Type the first argument element
              typed_element <- typeExpression (Just ty) typeRHSObject elemnt
              case typed_element of
                (SAST.AccessObject (SAST.Variable {})) -> return ()
                _ -> throwError $ annotateError ann EMsgQueueSendArgNotObject
              return (([Parameter "element" ty] , [typed_element]), Unit)
            _ -> throwError $ annotateError ann ENoMsgQueueSendWrongArgs
        _ -> throwError $ annotateError ann $ EMsgQueueWrongProcedure ident
    ty -> throwError $ annotateError ann (EFunctionAccessNotResource ty)

----------------------------------------
-- These functions are useful:
-- typeLHSObject looks up in write local environment
-- typeRHSObject looks up in read+write local environment
-- tyeGlobalObject looks up only in the global environment
typeRHSObject, typeLHSObject, typeGlobalObject :: Object Parser.ParserAnn
  -> SemanticMonad (SAST.Object SemanticAnn)
typeRHSObject = typeObject getRHSVarTy
typeLHSObject = typeObject getLHSVarTy
typeGlobalObject = typeObject getGlobalVarTy
----------------------------------------

typeConstExpression :: TypeSpecifier -> ConstExpression Parser.ParserAnn -> SemanticMonad (SAST.ConstExpression SemanticAnn)
typeConstExpression expected_ty (KC constant ann) = do
  checkConstant ann expected_ty constant
  return $ SAST.KC constant (buildExpAnn ann expected_ty)
typeConstExpression expected_ty (KV identifier ann) = do
  (ty, _value) <- getConst ann identifier
  checkEqTypesOrError ann expected_ty ty
  return $ SAST.KV identifier (buildExpAnn ann ty)

-- | Function |typeExpression| takes an expression from the parser, traverse it
-- annotating each node with its type.
-- Since we are also creating new nodes (|Unbox| annotations), instead of just
-- traversing, we are actually /creating/ a new tree with implicit
-- constructions.
typeExpression ::
  -- | Expected type of the expression
  Maybe TypeSpecifier ->
  -- | Function used to type objects (depends on the scope)
  (Object Parser.ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -- | Expression to type
  -> Parser.Expression Parser.ParserAnn
  -> SemanticMonad (SAST.Expression SemanticAnn)
-- Object access
typeExpression expectedType typeObj (AccessObject obj) = do
    -- | Type the object
    typed_obj <- typeObj obj
    -- | Get the type of the object
    (_, obj_type) <- getObjectType typed_obj
    case (expectedType, obj_type) of
      (Just (BoxSubtype ts), BoxSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        checkEqTypesOrError (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      (Just ts, BoxSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        checkEqTypesOrError (getAnnotation obj) ts ts'
        -- If we have an box and expect an unboxed type:
        return $ SAST.AccessObject (unBox typed_obj)
      (Just ts, ts') -> do
        -- If the type must be an expected type, then check it.
        checkEqTypesOrError (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      -- If we are requesting an object without an expected type, then we must
      -- be casting the result. Thus, we must return an unboxed object.
      (Nothing, BoxSubtype _)->
        return $ SAST.AccessObject (unBox typed_obj)
      (Nothing, _) ->
        return $ SAST.AccessObject typed_obj
-- | Constant literals with an expected type.
typeExpression (Just expectedType) _ (Constant c pann) = do
  -- | Call the function that checks that the constant is of the expected type.
  checkConstant pann expectedType c
  return $ SAST.Constant c (buildExpAnn pann expectedType)
-- | Integer literals without an expected type but with a known type.
typeExpression Nothing _ (Constant c@(I _ (Just ts)) pann) = do
  return $ SAST.Constant c (buildExpAnn pann ts)
-- | Integer literals without an expected type and without a known type.
-- This is an error, since we cannot infer the type of the constant.
typeExpression Nothing _ (Constant c@(I _ Nothing) pann) = do
  throwError $ annotateError pann $ EConstantWithoutKnownType c
-- | Boolean literals without an expected type.
typeExpression Nothing _ (Constant c@(B {}) pann) = do
  return $ SAST.Constant c (buildExpAnn pann Bool)
-- | Character literals without an expected type.
typeExpression Nothing _ (Constant c@(C {}) pann) = do
  return $ SAST.Constant c (buildExpAnn pann Char)
typeExpression expectedType typeObj (Casting e nty pann) = do
  maybe (return ()) (checkEqTypesOrError pann nty) expectedType
  -- | Casting Expressions.
  typed_exp <- typeExpression Nothing typeObj e
  type_exp <- getExpType typed_exp
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
      (TypeSpecifier -> Bool)
      -- | Left hand side error constructor
      -> (TypeSpecifier -> Error Parser.ParserAnn)
      -- | Right hand side error constructor
      -> (TypeSpecifier -> Error Parser.ParserAnn)
      -- | Left hand side expression
      -> Parser.Expression Parser.ParserAnn 
      -- | Right hand side expression
      -> Parser.Expression Parser.ParserAnn 
      -- | Typed expressions
      -> SemanticMonad (TypeSpecifier, SAST.Expression SemanticAnn, SAST.Expression SemanticAnn)
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
            tyre_ty <- getExpType tyre
            unless (isValid tyre_ty) (throwError $ annotateError (getAnnotation rnume) (rerror tyre_ty))
            catchMismatch pann (EBinOpTypeMismatch op tyre_ty) (typeExpression (Just tyre_ty) typeObj lnume)
          _ -> throwError err)
      tyle_ty <- getExpType tyle
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
          tyre_ty <- getExpType tyre
          unless (posTy tyre_ty) (throwError $ annotateError pann (EBinOpRightTypeNotPos op tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          tyle <- typeExpression Nothing typeObj le
          tyre <- typeExpression Nothing typeObj re
          tyle_ty <- getExpType tyle
          tyre_ty <- getExpType tyre
          unless (numTy tyle_ty) (throwError $ annotateError pann (EBinOpLeftTypeNotNum op tyle_ty))
          unless (posTy tyre_ty) (throwError $ annotateError pann (EBinOpRightTypeNotPos op tyre_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann tyle_ty)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is equatable. This function is used to check the binary expressions == and !=.
    sameEquatableTyBool :: SemanticMonad (SAST.Expression SemanticAnn)
    sameEquatableTyBool =
      case expectedType of
        (Just Bool) -> do
          (_, tyle, tyre) <- 
            sameTypeExpressions equatableTy 
              (EBinOpLeftTypeNotEquatable op) (EBinOpRightTypeNotEquatable op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          (_, tyle, tyre) <- 
            sameTypeExpressions equatableTy 
              (EBinOpLeftTypeNotEquatable op) (EBinOpRightTypeNotEquatable op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is numeric. This function is used to check the binary expressions <, <=, > and >=.
    sameNumTyBool :: SemanticMonad (SAST.Expression SemanticAnn)
    sameNumTyBool =
      case expectedType of
        (Just Bool) -> do
          (_, tyle, tyre) <- 
            sameTypeExpressions numTy 
              (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          (_, tyle, tyre) <- 
            sameTypeExpressions numTy 
              (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is boolean. This function is used to check the binary expressions && and ||.
    sameBoolType :: SemanticMonad (SAST.Expression SemanticAnn)
    sameBoolType =
      case expectedType of
        (Just Bool) -> do
          tyle <- catchMismatch pann (EBinOpLeftTypeNotBool op) (typeExpression (Just Bool) typeObj le)
          tyre <- catchMismatch pann (EBinOpRightTypeNotBool op) (typeExpression (Just Bool) typeObj re)
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          tyle <- catchMismatch pann (EBinOpLeftTypeNotBool op) (typeExpression (Just Bool) typeObj le)
          tyre <- catchMismatch pann (EBinOpRightTypeNotBool op) (typeExpression (Just Bool) typeObj re)
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann Bool)
typeExpression expectedType typeObj (ReferenceExpression refKind rhs_e pann) = 
  case rhs_e of
    ArraySlice obj lower upper anns -> do
      typed_obj <- typeObj obj
      (obj_ak, obj_ty) <- getObjectType typed_obj
      typed_lower <- typeExpression (Just USize) typeRHSObject lower
      typed_upper <- typeExpression (Just USize) typeRHSObject upper
      case obj_ty of
        Array ty _ -> do
          checkReferenceAccessKind obj_ak
          case expectedType of
            Just rtype@(Reference _ak (Array ts _size)) -> do
              unless (checkEqTypes ty ts) (throwError $ annotateError pann $ EMismatch ts ty)
              return (SAST.ArraySliceExpression refKind typed_obj typed_lower typed_upper (buildExpAnn pann rtype))
            Just ety -> throwError $ annotateError pann $ EMismatch (Reference refKind ty) ety
            _ -> throwError $ annotateError pann ESliceInvalidUse
        ty -> throwError $ annotateError anns (EArray ty) --}
    _ -> do
      -- | Type object
      typed_obj <- typeObj rhs_e
      -- | Get the type of the object
      (obj_ak, obj_type) <- getObjectType typed_obj
      case obj_type of
        -- | If the object is of a box subtype, then the reference will be to an object of
        -- the base type. Objects of a box subtype are always immutable, BUT a reference
        -- to an object of a box subtype can be mutable. Thus, we do not need to check
        -- the access kind of the object.
        BoxSubtype ty -> do
          -- | Check that the expected type is the same as the base type.  
          maybe (return ()) (checkEqTypesOrError pann (Reference refKind ty)) expectedType
          return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (Reference refKind ty)))
        _ -> do
          -- | Check if the we are allowed to create that kind of reference from the object
          checkReferenceAccessKind obj_ak
          maybe (return ()) (flip (checkEqTypesOrError pann) (Reference refKind obj_type)) expectedType
          return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (Reference refKind obj_type)))

  where

    checkReferenceAccessKind :: AccessKind -> SemanticMonad ()
    checkReferenceAccessKind obj_ak =
      case (obj_ak, refKind) of
        (Immutable, Mutable) -> throwError $ annotateError pann EMutableReferenceToImmutable
        _ -> return ()

---------------------------------------
-- | Function Expression.  A tradicional function call
typeExpression expectedType _ (FunctionCall ident args ann) = do
  (ps, retty, funcLocation) <- getFunctionTy ann ident
  let expAnn = buildExpAnnApp ann ps retty
      (psLen , asLen) = (length ps, length args)
  -- Check that the number of parameters are OK
  when (psLen < asLen) (throwError $ annotateError ann (EFunctionCallExtraParams (ident, ps, funcLocation) (fromIntegral asLen)))
  when (psLen > asLen) (throwError $ annotateError ann (EFunctionCallMissingParams (ident, ps, funcLocation) (fromIntegral asLen)))
  typed_args <- zipWithM (\p e -> catchMismatch ann (EFunctionCallParamTypeMismatch (ident, p, funcLocation))
      (typeExpression (Just (paramTypeSpecifier p)) typeRHSObject e)) ps args
  maybe (return ()) (checkEqTypesOrError ann retty) expectedType
  return $ SAST.FunctionCall ident typed_args expAnn

----------------------------------------
typeExpression expectedType typeObj (MemberFunctionCall obj ident args ann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjectType obj_typed
  ((ps, typed_args), fty) <- typeMemberFunctionCall ann obj_ty ident args
  maybe (return ()) (checkEqTypesOrError ann fty) expectedType
  return $ SAST.MemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)
typeExpression expectedType typeObj (DerefMemberFunctionCall obj ident args ann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjectType obj_typed
  case obj_ty of
    Reference _ refTy -> do
      -- NOTE: We have reused the code from MemberFunctionCall, but we must take into
      -- account that, for the time being, when we are accessing a member function through
      -- a reference, the object (self) can only be of a user-defined class type. There
      -- cannot be references to ports. 
      ((ps, typed_args), fty) <- typeMemberFunctionCall ann refTy ident args
      maybe (return ()) (checkEqTypesOrError ann fty) expectedType
      return $ SAST.DerefMemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)
    ty -> throwError $ annotateError ann $ ETypeNotReference ty
----------------------------------------
-- | Struct Initializer
typeExpression (Just ty@(DefinedType id_ty)) typeObj (StructInitializer fs mty pann) = do
  -- | Check field type
  case mty of
    (Just id_ty') -> do
      struct_ty <-
        catchError
          (getGlobalTypeDef pann id_ty')
          (\_ -> throwError $ annotateError pann (EStructInitializerUnknownType id_ty))
      catchMismatch pann (EStructInitializerTypeMismatch ty) (checkEqTypesOrError pann ty (DefinedType id_ty'))
      return struct_ty
    Nothing ->
      catchError
        (getGlobalTypeDef pann id_ty)
        -- | Internal error. This should not happen, since we must have checked the
        -- expected type before calling this function. 
        (\_ -> throwError $ annotateError Internal (ENoStructFound id_ty))
  >>= \case{
    Struct _ ty_fs _mods  ->
      SAST.StructInitializer
        <$> checkFieldValues pann typeObj ty_fs fs
        <*> pure (Just id_ty)
        <*> pure (buildExpAnn pann (DefinedType id_ty));
    Class _clsKind _ident members _provides _mods ->
      let fields = [fld | (ClassField fld@(FieldDefinition {}) _) <- members] in
        SAST.StructInitializer
        <$> checkFieldValues pann typeObj fields fs
        <*> pure (Just id_ty)
        <*> pure (buildExpAnn pann (DefinedType id_ty));
    x -> throwError $ annotateError pann (EStructInitializerGlobalNotStruct (fmap forgetSemAnn x));
  }
-- We shall always expect a type for the struct initializer. If we do not expect a type
-- it means that we are using the expression in the wild.
typeExpression (Just ty) _ (StructInitializer _fs _mty pann) =
  throwError $ annotateError pann (EStructInitializerExpectedTypeNotStruct ty)
typeExpression _ _ (StructInitializer _fs _mty pann) =
  throwError $ annotateError pann EStructInitializerInvalidUse
typeExpression expectedType typeObj (EnumVariantInitializer id_ty variant args pann) =
  -- | Enum Variant
  catchError
    (getGlobalTypeDef pann id_ty)
    (\_ -> throwError $ annotateError pann (ETyNotEnumFound id_ty))
  >>= \case{
   Enum _ ty_vs _mods ->
     case Data.List.find ((variant ==) . variantIdentifier) ty_vs of
       Nothing -> throwError $ annotateError pann (EEnumVariantNotFound variant)
       Just (EnumVariant _ ps) ->
         let (psLen , asLen ) = (length ps, length args) in
         if psLen == asLen
         then
            maybe (return ()) (checkEqTypesOrError pann (DefinedType id_ty)) expectedType >>
            flip (SAST.EnumVariantInitializer id_ty variant) (buildExpAnn pann (DefinedType id_ty))
             <$> zipWithM (\p e -> typeExpression (Just p) typeObj e) ps args
         else if psLen < asLen
         then throwError $ annotateError pann EEnumVariantExtraParams
         else throwError $ annotateError pann EEnumVariantMissingParams
    ;
   x -> throwError $ annotateError pann (ETyNotEnum id_ty (fmap forgetSemAnn x))
  }
-- IDEA Q4
typeExpression expectedType typeObj (ArrayInitializer iexp size pann) = do
-- | Array Initialization
  case expectedType of
    Just (Array ts arrsize) -> do
      typed_init <- typeExpression (Just ts) typeObj iexp
      unless (size == arrsize) (throwError $ annotateError pann (EArrayInitializerSizeMismatch arrsize size))
      return $ SAST.ArrayInitializer typed_init size (buildExpAnn pann (Array ts size))
    Just ts -> throwError $ annotateError pann (EArray ts)
    _ -> throwError $ annotateError pann EArrayIntitalizerInvalidUse
typeExpression expectedType typeObj (ArrayExprListInitializer exprs pann) = do
  case expectedType of
    Just (Array ts arrsize) -> do
      typed_exprs <- mapM (\e ->
        catchMismatch (getAnnotation e) 
          (EArrayExprListInitializerExprTypeMismatch ts) (typeExpression (Just ts) typeObj e)) exprs
      size_value <- getIntSize pann arrsize
      unless (length typed_exprs == fromIntegral size_value) 
        (throwError $ annotateError pann (EArrayExprListInitializerSizeMismatch size_value (fromIntegral $ length typed_exprs)))
      return $ SAST.ArrayExprListInitializer typed_exprs (buildExpAnn pann (Array ts arrsize))
    Just ts -> throwError $ annotateError pann (EArray ts)
    _ -> throwError $ annotateError pann EArrayExprListIntitalizerInvalidUse
typeExpression expectedType typeObj (OptionVariantInitializer vexp anns) =
  case expectedType of
    Just (Option ts) -> do
      case vexp of
        None -> return $ SAST.OptionVariantInitializer None (buildExpAnn anns (Option ts))
        Some e -> do
          typed_e <- typeExpression (Just ts) typeObj e
          return $ SAST.OptionVariantInitializer (Some typed_e) (buildExpAnn anns (Option ts))
    _ -> throwError $ annotateError anns EOptionVariantInitializerInvalidUse
typeExpression expectedType typeObj (IsEnumVariantExpression obj id_ty variant_id pann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjectType obj_typed
  lhs_ty <- case obj_ty of
    DefinedType dident ->
      catchError (getGlobalTypeDef pann dident) (\_ -> throwError $ annotateError pann (EIsVariantNotEnum obj_ty))
    _ -> throwError $ annotateError pann (EIsVariantNotEnum obj_ty)
  case lhs_ty of
    Enum lhs_enum ty_vs _mods ->
      if lhs_enum == id_ty
      then
        case Data.List.find ((variant_id ==) . variantIdentifier) ty_vs of
          Just (EnumVariant {}) -> do
            maybe (return ()) (checkEqTypesOrError pann Bool) expectedType
            return $ SAST.IsEnumVariantExpression obj_typed id_ty variant_id (buildExpAnn pann Bool)
          Nothing -> throwError $ annotateError pann (EEnumVariantNotFound variant_id)
      else throwError $ annotateError pann (EIsVariantEnumMismatch lhs_enum id_ty)
    x -> throwError $ annotateError pann (ETyNotEnum id_ty (fmap forgetSemAnn x))
typeExpression expectedType typeObj (IsOptionVariantExpression obj variant_id pann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjectType obj_typed
  case obj_ty of
    (Option {}) -> do
      maybe (return ()) (checkEqTypesOrError pann Bool) expectedType
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
  :: Parser.ParserAnn
  -> (Object Parser.ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> FieldDefinition
  -> Parser.FieldAssignment Parser.ParserAnn
  -> SemanticMonad (SAST.FieldAssignment SemanticAnn)
checkFieldValue loc typeObj (FieldDefinition fid fty) (FieldValueAssignment faid faexp pann) =
  if fid == faid
  then
    flip (SAST.FieldValueAssignment faid) (buildStmtAnn pann) <$> typeExpression (Just fty) typeObj faexp
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
    getGlobalEntry loc sid >>=
    \gentry ->
    case fty of
      SinkPort _ action  ->
        case gentry of
          -- TODO: Check that the type of the inbound port and the type of the emitter match
          Located  (GGlob (SEmitter ets)) _ -> 
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildSinkPortConnAnn pann ets action)
          _ -> throwError $ annotateError loc $ EInboundPortNotEmitter sid
      InPort _ action  ->
        case gentry of
          -- TODO: Check that the type of the inbound port and the type of the emitter match
          Located (GGlob (SChannel cts)) _ -> 
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildInPortConnAnn pann cts action)
          _ -> throwError $ annotateError loc $ EInboundPortNotChannel sid
      _ -> throwError $ annotateError loc (EFieldNotPort fid)
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc _ (FieldDefinition fid fty) (FieldPortConnection OutboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry loc sid >>=
    \gentry ->
    case fty of
      OutPort _ ->
        case gentry of
          -- TODO: Check that the type of the outbound port and the type of the channel match
          Located (GGlob (SChannel cts)) _ -> return $ SAST.FieldPortConnection OutboundPortConnection pid sid (buildOutPortConnAnn pann cts)
          _ -> throwError $ annotateError loc $ EOutboundPortNotChannel sid
      _ -> throwError $ annotateError loc (EFieldNotPort fid)
  else throwError $ annotateError loc (EFieldMissing [fid])
checkFieldValue loc _ (FieldDefinition fid fty) (FieldPortConnection AccessPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry loc sid >>=
    \gentry ->
    case fty of
      AccessPort (Allocator {}) ->
        case gentry of
          -- TODO: Check that the types match
          Located (GGlob (SResource (Pool ty s))) _ -> return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildPoolConnAnn pann ty s)
          _ -> throwError $ annotateError loc $ EAccessPortNotPool sid
      AccessPort (AtomicAccess {}) ->
        case gentry of
          -- TODO: Check that the types match
          Located (GGlob (SResource (Atomic ty))) _ -> return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicConnAnn pann ty)
          _ -> throwError $ annotateError loc $ EAccessPortNotAtomic sid
      AccessPort (AtomicArrayAccess ty s) ->
        case gentry of
          Located (GGlob (SResource (AtomicArray ty' s'))) _ -> do
            catchMismatch pann (EAtomicArrayConnectionTypeMismatch ty) (checkEqTypesOrError loc ty ty')
            unless (s == s') (throwError $ annotateError loc $ EAtomicArrayConnectionSizeMismatch s s')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicArrayConnAnn pann ty s)
          _ -> throwError $ annotateError loc $ EAccessPortNotAtomicArray sid
      AccessPort (DefinedType iface) ->
        getGlobalTypeDef loc iface >>=
          \case {
            Interface _ members _ ->
              -- Check that the resource provides the interface
              case gentry of
                Located (GGlob (SResource rts@(DefinedType {}))) _ ->
                  let procs = [SemanProcedure procid params | (InterfaceProcedure procid params _) <- members] in
                  return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAccessPortConnAnn pann rts procs)
                _ -> throwError $ annotateError loc $ EAccessPortNotResource sid
              ;
            _ -> throwError $ annotateError loc (EAccessPortNotInterface (DefinedType iface))
          }
      _ -> throwError $ annotateError loc (EFieldNotPort fid)
  else throwError $ annotateError loc (EFieldMissing [fid])

checkFieldValues
  :: Parser.ParserAnn
  -> (Object Parser.ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> [FieldDefinition]
  -> [Parser.FieldAssignment Parser.ParserAnn]
  -> SemanticMonad [SAST.FieldAssignment SemanticAnn]
checkFieldValues loc typeObj fds fas = checkSortedFields sorted_fds sorted_fas []
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
      checkFieldValue loc typeObj d a >>= checkSortedFields ds as . (:acc)

retStmt :: Maybe TypeSpecifier -> Parser.ReturnStmt Parser.ParserAnn -> SemanticMonad (SAST.ReturnStmt SemanticAnn)
retStmt Nothing (ReturnStmt Nothing anns) = do
  return $ SAST.ReturnStmt Nothing (buildExpAnn anns Unit)
retStmt (Just ts) (ReturnStmt (Just e) anns) = do
  typed_e <- typeExpression (Just ts) typeRHSObject e
  -- ReturnStmt (Just ety) . buildExpAnn anns <$> getExpType ety
  return $ ReturnStmt (Just typed_e) (buildExpAnn anns ts)
retStmt (Just ty) (ReturnStmt Nothing anns) = throwError $ annotateError anns $ EReturnValueExpected ty
retStmt Nothing (ReturnStmt _ anns) = throwError $ annotateError anns EReturnValueNotUnit

typeBlockRet :: Maybe TypeSpecifier -> Parser.BlockRet Parser.ParserAnn -> SemanticMonad (SAST.BlockRet SemanticAnn)
typeBlockRet ts (BlockRet bbody rete) = BlockRet <$> typeBlock bbody <*> retStmt ts rete

typeBlock :: Parser.Block Parser.ParserAnn -> SemanticMonad (SAST.Block SemanticAnn)
typeBlock = mapM typeStatement

-- | Type checking statements. We should do something about Break
-- Rules here are just environment control.
typeStatement :: Parser.Statement Parser.ParserAnn -> SemanticMonad (SAST.Statement SemanticAnn)
-- Declaration semantic analysis
typeStatement (Declaration lhs_id lhs_ak lhs_type expr anns) =
  -- Check type is alright
  checkTypeSpecifier anns lhs_type >>
  -- Expression and type must match
  typeExpression (Just lhs_type) typeRHSObject expr >>= mustBeTy lhs_type >>=
    \ety ->
  -- Insert object in the corresponding environment
  -- If the object is mutable, then we insert it in the local mutable environment
  -- otherwise we insert it in the read-only environment
    (case lhs_ak of
      Mutable -> insertLocalMutObj anns lhs_id lhs_type
      Immutable -> insertLocalImmutObj anns lhs_id lhs_type) >>
  -- Return annotated declaration
  return (Declaration lhs_id lhs_ak lhs_type ety (buildStmtAnn anns))
typeStatement (AssignmentStmt lhs_o rhs_expr anns) = do
{- TODO Q19 && Q20 -}
  lhs_o_typed' <- typeLHSObject lhs_o
  (lhs_o_ak', lhs_o_type') <- getObjectType lhs_o_typed'
  let (lhs_o_typed, lhs_o_ak, lhs_o_type) =
        maybe (lhs_o_typed', lhs_o_ak', lhs_o_type') (unBox lhs_o_typed', Mutable, ) (isBox lhs_o_type')
  unless (lhs_o_ak /= Immutable) (throwError $ annotateError anns EAssignmentToImmutable)
  rhs_expr_typed' <- typeExpression (Just lhs_o_type) typeRHSObject rhs_expr
  type_rhs' <- getExpType rhs_expr_typed'
  rhs_expr_typed <- maybe (return rhs_expr_typed') (\_ -> unBoxExp rhs_expr_typed') (isBox type_rhs')
  ety <- mustBeTy lhs_o_type rhs_expr_typed
  return $ AssignmentStmt lhs_o_typed ety $ buildStmtAnn anns
typeStatement (IfElseStmt cond_expr tt_branch elifs otherwise_branch anns) = do
  -- | Check that if the statement defines an else-if branch, then it must have an otherwise branch
  when (not (null elifs) && isNothing otherwise_branch) (throwError $ annotateError anns EIfElseNoOtherwise)
  IfElseStmt
    -- | Check that the condition is a boolean expression
    <$> typeCondExpr cond_expr
    <*> localScope (typeBlock tt_branch)
    <*> mapM (\case {
                ElseIf eCond eBd ann ->
                  ElseIf <$> typeCondExpr eCond
                         <*> localScope (typeBlock eBd)
                         <*> return (buildStmtAnn ann)
                  }) elifs
    <*> maybe (return Nothing) (fmap Just . localScope . typeBlock) otherwise_branch
    <*> return (buildStmtAnn anns)
  where
    typeCondExpr :: Parser.Expression Parser.ParserAnn -> SemanticMonad (SAST.Expression SemanticAnn)
    typeCondExpr bExpr = catchError (typeExpression (Just Bool) typeRHSObject bExpr)
      (\err -> case getError err of
        EMismatch Bool ty -> throwError $ annotateError (getAnnotation err) $ EIfElseIfCondNotBool ty
        _ -> throwError err
      )
-- Here we could implement some abstract interpretation analysis
typeStatement (ForLoopStmt it_id it_ty from_expr to_expr mWhile body_stmt anns) = do
  -- Check the iterator is of numeric type
  unless (numTy it_ty) (throwError $ annotateError anns (EForIteratorWrongType it_ty))
  -- Both boundaries should have the same numeric type
  typed_fromexpr <- typeConstExpression it_ty from_expr
  typed_toexpr <- typeConstExpression it_ty to_expr
  ForLoopStmt it_id it_ty typed_fromexpr typed_toexpr
    <$> (case mWhile of
              Nothing -> return Nothing
              Just whileC -> do
                  typed_whileC <- addLocalImmutObjs anns [(it_id, it_ty)] $
                      typeExpression (Just Bool) typeRHSObject whileC
                  return (Just typed_whileC)
        )
    <*> addLocalImmutObjs anns [(it_id, it_ty)] (typeBlock body_stmt)
    <*> return (buildStmtAnn anns)
typeStatement (SingleExpStmt expr anns) =
  flip SingleExpStmt (buildStmtAnn anns) <$> typeExpression (Just Unit) typeRHSObject expr
typeStatement (MatchStmt matchE cases ann) = do
  typed_matchE <- typeExpression Nothing typeRHSObject matchE
  type_matchE <- getExpType typed_matchE
  case type_matchE of
    DefinedType t -> getGlobalTypeDef ann t >>=
        \case {
          Enum _ident flsDef _mods ->
          -- Sort both lists by identifiers
          let ord_flsDef = Data.List.sortOn variantIdentifier flsDef in
          let ord_cases = Data.List.sortOn matchIdentifier cases in
          case zipSameLength
                (const (annotateError ann EMatchExtraCases))
                (const (annotateError ann EMatchExtraCases))
                typeMatchCase ord_cases ord_flsDef of
            Left e -> throwError e
            Right cs -> flip (MatchStmt typed_matchE) (buildStmtAnn ann) <$> sequence cs
          ;
          _ -> throwError $ annotateError ann $ EMatchNotEnum t
        }
    Option t ->
      let ord_cases = Data.List.sortOn matchIdentifier cases in
      checkOptionCases ord_cases >>= flip unless (throwError $  annotateError ann EMatchOptionBad)
      >>
      MatchStmt typed_matchE <$> zipWithM typeMatchCase ord_cases [EnumVariant "None" [],EnumVariant "Some" [t]] <*> pure (buildStmtAnn ann)
    _ -> throwError $  annotateError ann $ EMatchWrongType type_matchE
    where

      checkOptionCases :: [Parser.MatchCase Parser.ParserAnn] -> SemanticMonad Bool
      checkOptionCases [a,b] = return $ (isOptionNone a && isOptionSome b) || (isOptionSome a && isOptionNone b)
      checkOptionCases _ = throwError $ annotateError ann EMatchOptionBadArgs

      isOptionNone :: Parser.MatchCase Parser.ParserAnn -> Bool
      isOptionNone c =
        matchIdentifier c == "None"
          && Prelude.null (matchBVars c)

      isOptionSome :: Parser.MatchCase Parser.ParserAnn -> Bool
      isOptionSome c =
        matchIdentifier c == "Some"
           && length (matchBVars c) == 1

      typeMatchCase :: Parser.MatchCase Parser.ParserAnn -> EnumVariant -> SemanticMonad (SAST.MatchCase SemanticAnn)
      typeMatchCase c (EnumVariant vId vData) = typeMatchCase' c vId vData
        where
          typeMatchCase' (MatchCase cIdent bVars bd mcann) supIdent tVars
            | cIdent == supIdent =
              if length bVars == length tVars then
              flip (SAST.MatchCase cIdent bVars) (buildStmtMatchCaseAnn ann tVars) <$> addLocalImmutObjs mcann (zip bVars tVars) (typeBlock bd)
              else throwError $ annotateError Internal EMatchCaseInternalError
            | otherwise = throwError $ annotateError Internal $ EMatchCaseBadName cIdent supIdent


----------------------------------------
-- Programs Semantic Analyzer
-- For now all are kinda the same thing but eventually they should not :shrug:
----------------------------------------

-- Keeping only type information
-- TODO Check ident is not defined?
typeGlobal :: Parser.Global Parser.ParserAnn -> SemanticMonad (SAST.Global SemanticAnn)
typeGlobal (Task ident ty mexpr mods anns) = do
  checkTypeSpecifier anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> typeExpression (Just ty) typeGlobalObject expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Task ident ty exprty mods (buildGlobalAnn anns (STask ty)))
typeGlobal (Handler ident ty mexpr mods anns) = do
  checkTypeSpecifier anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> typeExpression (Just ty) typeGlobalObject expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Handler ident ty exprty mods (buildGlobalAnn anns (SHandler ty)))
typeGlobal (Resource ident ty mexpr mods anns) = do
  checkTypeSpecifier anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> typeExpression (Just ty) typeGlobalObject expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Resource ident ty exprty mods (buildGlobalAnn anns (SResource ty)))
typeGlobal (Emitter ident ty mexpr mods anns) = do
  checkTypeSpecifier anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> typeExpression (Just ty) typeGlobalObject expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  glb <- case ty of
        -- | We are going to manually map de Emitter to the proper type
        (DefinedType "Interrupt") -> return $ SEmitter ty
        (DefinedType "PeriodicTimer") -> return $  SEmitter ty
        (DefinedType "SystemInit") -> return $ SEmitter ty
        _ -> throwError $ annotateError Internal EInternalNoGTY
  return (SAST.Emitter ident ty exprty mods (buildGlobalAnn anns glb))
typeGlobal (Channel ident ty mexpr mods anns) = do
  checkTypeSpecifier anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> typeExpression (Just ty) typeGlobalObject expr
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (SAST.Channel ident ty exprty mods (buildGlobalAnn anns (SChannel ty)))
typeGlobal (Const ident ty expr mods anns) = do
  checkTypeSpecifier anns ty
  typed_expr <- typeConstExpression ty expr
  case expr of
    KC value _ -> return (SAST.Const ident ty typed_expr mods (buildGlobalAnn anns (SConst ty value)))
    KV cident _ -> do
      (_ty, value) <- getConst anns cident
      -- TODO: Check that the types match
      return (SAST.Const ident ty typed_expr mods (buildGlobalAnn anns (SConst ty value)))

checkParameterType :: Location -> Parameter -> SemanticMonad ()
checkParameterType anns p =
    let typeSpec = paramTypeSpecifier p in
     -- If the type specifier is a box, then we must throw an EArgHasBox error
     -- since we cannot have box types as parameters.
    unless (parameterTy typeSpec) (throwError (annotateError anns (EInvalidParameterType p))) >>
    checkTypeSpecifier anns typeSpec

checkReturnType :: Location -> TypeSpecifier -> SemanticMonad ()
checkReturnType anns ty =
  unless (returnValueTy ty) (throwError (annotateError anns (EInvalidReturnType ty))) >>
  checkTypeSpecifier anns ty

-- Here we actually only need Global
programSeman :: Parser.AnnASTElement Parser.ParserAnn -> SemanticMonad (SAST.AnnASTElement SemanticAnn)
programSeman (Function ident ps mty bret mods anns) =
  ----------------------------------------
  -- Check the return type 
  maybe (return ()) (checkReturnType anns) mty >>
  checkFunction

  where
    checkFunction :: SemanticMonad (SAST.AnnASTElement SemanticAnn)
    checkFunction =
          -- Check regular params
        forM_ ps (checkParameterType anns) >>
          Function ident ps mty
            <$> (addLocalImmutObjs anns
                  (fmap (\p -> (paramIdentifier p , paramTypeSpecifier p)) ps)
                  (typeBlockRet mty bret) >>= \ typed_bret ->
                maybe
                    -- Procedure
                    (blockRetTy Unit)
                    -- Function
                    blockRetTy
                    mty typed_bret >> return typed_bret)
            <*> pure mods
            <*> pure (buildGlobal anns (GFun ps (fromMaybe Unit mty)))
programSeman (GlobalDeclaration gbl) =
  -- TODO Add global declarations
  GlobalDeclaration <$> typeGlobal gbl
programSeman (TypeDefinition tydef ann) =
  typeTypeDefinition ann tydef >>= \t ->
    -- let stdef = semanticTypeDef t in
    -- and we can add it to the global environment.
    -- insertGlobalTy ann stdef >>
    return (TypeDefinition t (buildGlobalTy ann (semanticTypeDef t)))

semanticTypeDef :: SAST.TypeDef SemanticAnn -> SemanTypeDef SemanticAnn
semanticTypeDef (Struct i f m)  = Struct i f m
semanticTypeDef (Enum i e m)    = Enum i e m
semanticTypeDef (Class kind i cls ps m) = Class kind i (Data.List.map kClassMember cls) ps m
semanticTypeDef (Interface i cls m) = Interface i cls m



-- | This function type checks the members of a class depending on its kind.
checkClassKind :: Location -> Identifier -> ClassKind
  -> ([SAST.ClassMember SemanticAnn],
      [Parser.ClassMember Parser.ParserAnn],
      [Parser.ClassMember Parser.ParserAnn])
  -> [Identifier] -> SemanticMonad ()
-- | Resource class type checking
checkClassKind anns clsId ResourceClass (fs, prcs, acts) provides = do
  -- A resource must provide at least one interface
  when (null provides) (throwError $ annotateError anns (EResourceClassNoProvides clsId))
  -- A resource must not have any actions
  case acts of
    [] -> return ()
    (ClassAction actionId _ _ _ ann):_  ->
        throwError $ annotateError ann (EResourceClassAction (clsId, anns) actionId)
    _ -> throwError (annotateError Internal EClassTyping)
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
    catchError (getGlobalTypeDef anns ifaceId)
      (\_ -> throwError $ annotateError anns (EInterfaceNotFound ifaceId)) >>= \case {
      Interface _ iface_prcs _ -> return $ map (, ifaceId) iface_prcs : acc;
      _ -> throwError $ annotateError anns (EMismatchIdNotInterface ifaceId)
    }) [] provides
  let sorted_provided = Data.List.sortOn (\(InterfaceProcedure ifaceId _ _, _) -> ifaceId) providedProcedures
  let sorted_prcs = Data.List.sortOn (
        \case {
          (ClassProcedure prcId _ _ _) -> prcId;
          _ -> error "internal error: checkClassKind"
        }) prcs
  -- Check that all procedures are provided and that the parameters match
  checkSortedProcedures sorted_provided sorted_prcs

  where

    checkSortedProcedures :: [(InterfaceMember SemanticAnn, Identifier)] -> [Parser.ClassMember Parser.ParserAnn] -> SemanticMonad ()
    checkSortedProcedures [] [] = return ()
    checkSortedProcedures [] ((ClassProcedure prcId _ _ ann):_) = throwError $ annotateError ann (EProcedureNotFromProvidedInterfaces (clsId, anns) prcId)
    checkSortedProcedures ((InterfaceProcedure procId _ _, ifaceId):_) [] = throwError $ annotateError anns (EMissingProcedure ifaceId procId)
    checkSortedProcedures ((InterfaceProcedure prcId ps pann, ifaceId):ds) ((ClassProcedure prcId' ps' _ ann):as) =
      unless (prcId == prcId') (throwError $ annotateError anns (EMissingProcedure ifaceId prcId)) >> do
      let psLen = length ps
          psLen' = length ps'
      when (psLen < psLen') (throwError $ annotateError ann (EProcedureExtraParams (ifaceId, prcId, ps, location pann) (fromIntegral psLen')))
      when (psLen > psLen') (throwError $ annotateError ann (EProcedureMissingParams (ifaceId, prcId, ps, location pann) (fromIntegral psLen')))
      zipWithM_ (\p@(Parameter _ ts) (Parameter _ ts') ->
        unless (checkEqTypes ts ts') (throwError $ annotateError ann (EProcedureParamTypeMismatch (ifaceId, prcId, p, location pann) ts'))) ps ps'
      checkSortedProcedures ds as
    checkSortedProcedures _ _ = throwError (annotateError Internal EClassTyping)

checkClassKind _anns _clsId _kind _members _provides = return ()

-- Type definition
-- Here I am traversing lists serveral times, I prefer to be clear than
-- proficient for the time being.
typeTypeDefinition :: Location -> Parser.TypeDef Parser.ParserAnn -> SemanticMonad (SAST.TypeDef SemanticAnn)
-- Check Type definitions https://hackmd.io/@termina-lang/SkglB0mq3#Struct-definitions
typeTypeDefinition ann (Struct ident fs mds) =
  -- Check every type is well-defined:
  -- Check that the struct is not empty
  when (Prelude.null fs) (throwError $ annotateError ann (EStructDefEmptyStruct ident))
  -- Check that every field is well-defined
  >> mapM_ (fieldDefinitionTy ann) fs
  -- Check field names are unique
  >> checkUniqueNames ann EStructDefNotUniqueField (Data.List.map fieldIdentifier fs)
  -- If everything is fine, return same struct
  >> return (Struct ident fs mds)
typeTypeDefinition ann (Enum ident evs mds) =
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Enumeration-definitions
  -- Check that the enum is not empty
  when (Prelude.null evs) (throwError $ annotateError ann (EEnumDefEmpty ident))
  -- Check the enum variants are well-defined
  >> mapM_ (enumDefinitionTy ann) evs
  -- Check names are unique
  >> checkUniqueNames ann EEnumDefNotUniqueField (Data.List.map variantIdentifier evs)
  -- If everything is fine, return the same definition.
  >> return (Enum ident evs mds)
typeTypeDefinition ann (Interface ident cls mds) = do
  -- Check that the interface is not empty
  when (null cls) (throwError $ annotateError ann (EInterfaceEmpty ident))
  -- Check procedure names are unique
  checkUniqueNames ann EInterfaceNotUniqueProcedure (Data.List.map (\case InterfaceProcedure ifaceId _ _ -> ifaceId) cls)
  -- Check that every procedure is well-defined
  procedures <- mapM typeInterfaceProcedure cls
  -- If everything is fine, return the same definition.
  return (Interface ident procedures mds)

  where

    typeInterfaceProcedure :: InterfaceMember Location -> SemanticMonad (SAST.InterfaceMember SemanticAnn)
    typeInterfaceProcedure (InterfaceProcedure procId ps annIP) = do
      mapM_ (checkTypeSpecifier annIP . paramTypeSpecifier) ps
      return $ InterfaceProcedure procId ps (buildExpAnn annIP Unit)

typeTypeDefinition ann (Class kind ident members provides mds) =
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
            -> checkTypeSpecifier annCF fs_ty
              >> classFieldTyorFail annCF fs_ty
              >> let checkFs = SAST.ClassField fld (buildExpAnn annCF fs_ty)
                in return (checkFs : fs, prcs, mths, vws, acts)
          -- Procedures
          prc@(ClassProcedure _fp_id fp_tys _body annCP)
            -> mapM_ (checkTypeSpecifier annCP . paramTypeSpecifier) fp_tys
            >> return (fs, prc : prcs, mths, vws, acts)
          -- Methods
          mth@(ClassMethod _fm_id mty _body annCM)
            -> maybe (return ()) (checkReturnType annCM) mty
            >> return (fs, prcs, mth : mths, vws, acts)
          -- Viewers
          view@(ClassViewer _fv_id fv_tys mty _body annCV)
            -> maybe (return ()) (checkReturnType annCV) mty
            -- Parameters cannot have boxes inside.
            >> mapM_ (checkParameterType annCV) fv_tys
            >> return (fs, prcs, mths, view : vws, acts)
          action@(ClassAction _fa_id fa_ty mty _body annCA)
            -> checkReturnType annCA mty
            >> (checkTypeSpecifier annCA . paramTypeSpecifier) fa_ty
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
    let dependenciesMap =
          foldr (selfDepClass objIsSelf) M.empty elements
    let dependencies = fmap M.keys dependenciesMap
    -- Map from ClassNames to their definition (usefull after sorting by name and dep)
    let nameClassMap = M.fromList (map (\e -> (className e, e)) elements)
    -- Sort and see if there is a loop
    topSortOrder <- case topSort dependencies of
            -- Tell the user a loop is in the room
            Left (ELoop loop) -> throwError (annotateError ann (EClassLoop loop))
            Left (ENotFound dep parent) ->
              case parent of
                Nothing -> error "Internal TopSort Error. This should not happen"
                Just parentId -> do
                  let parentDepsMap = fromJust $ M.lookup parentId dependenciesMap
                  case fromJust $ M.lookup dep parentDepsMap of
                    (MemberFunctionCall _obj mident _args cann) -> throwError (annotateError cann (EMemberAccessNotFunction mident))
                    (DerefMemberFunctionCall _obj mident _args cann) -> throwError (annotateError cann (EMemberAccessNotFunction mident))
                    _ -> error "Internal TopSort Error. This should not happen"
            Left e -> error $ "Internal TopSort Error" ++ show e
            -- Get the proper order of inclusion and get their definitions from names.
            Right order ->
              mapM
              (maybe
                (throwError (annotateError Internal EMissingIdentifier))
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
            ClassField {} -> throwError (annotateError Internal EClassTyping)
            -- Interesting case
            ClassProcedure mIdent mps blk mann -> do
              typed_blk <- addLocalImmutObjs mann (("self", Reference Mutable (DefinedType ident)) : fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps) (typeBlock blk)
              let newPrc = SAST.ClassProcedure mIdent mps typed_blk (buildExpAnn mann Unit)
              return (newPrc : prevMembers)
            ClassMethod mIdent mty mbody mann -> do
              typed_bret <- addLocalImmutObjs mann [("self", Reference Mutable (DefinedType ident))] (typeBlockRet mty mbody)
              maybe (blockRetTy Unit) blockRetTy mty typed_bret
              let newMth = SAST.ClassMethod mIdent mty  typed_bret (buildExpAnn mann (fromMaybe Unit mty))
              return (newMth : prevMembers)
            ClassViewer mIdent mps mty mbody mann -> do
              typed_bret <- addLocalImmutObjs mann (("self", Reference Immutable (DefinedType ident)) : fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps) (typeBlockRet mty mbody)
              maybe (blockRetTy Unit) blockRetTy mty typed_bret
              let newVw = SAST.ClassViewer mIdent mps mty typed_bret (buildExpAnn mann (fromMaybe Unit mty))
              return (newVw : prevMembers)
            ClassAction mIdent p ty mbody mann -> do
              typed_bret <- addLocalImmutObjs mann (("self", Reference Mutable (DefinedType ident)) : [(paramIdentifier p, paramTypeSpecifier p)]) (typeBlockRet (Just ty) mbody)
              blockRetTy ty typed_bret
              let newAct = SAST.ClassAction mIdent p ty typed_bret (buildExpAnn mann ty)
              return (newAct : prevMembers)
        ) [] topSortOrder
    return (SAST.Class kind ident (fls ++ fnChecked) provides mds)

----------------------------------------
-- Field definition helpers.
fieldDefinitionTy :: Location -> FieldDefinition -> SemanticMonad ()
fieldDefinitionTy ann f
  -- First we check its type is well-defined
  = checkTypeSpecifier ann tyFD
  -- and that it is simply (see simple types).
  >> simpleTyorFail ann tyFD
  -- we just return it.
  where
    tyFD = fieldTypeSpecifier f

-- Enum Variant definition helpers.
enumDefinitionTy :: Location -> EnumVariant -> SemanticMonad ()
enumDefinitionTy ann ev
  = mapM_ (\ty -> checkTypeSpecifier ann ty >> simpleTyorFail ann ty) ev_tys
  where
    ev_tys = assocData ev

checkUniqueNames :: Location -> ([Identifier] -> Error Location) -> [Identifier] -> SemanticMonad ()
checkUniqueNames ann err is =
  if allUnique is then return ()
  else throwError $ annotateError ann (err (repeated is))
  where
    -----------------------------------------
    -- TODO Improve this two functions.
    -- nub is O(n^2)
    allUnique :: Eq a => [a] -> Bool
    allUnique xs = Data.List.nub xs == xs

    repeated :: Eq a => [a] -> [a]
    repeated xs = Data.List.nub $ xs Data.List.\\ Data.List.nub xs
-----------------------------------------

-- Adding Global elements to the environment.
programAdd :: SAST.AnnASTElement SemanticAnn
  -> SemanticMonad (Identifier, Located (GEntry SemanticAnn))
programAdd (Function ident params mretType _bd _mods anns) =
  let
    gbl = GFun params (fromMaybe Unit mretType)
    el = Located gbl (location anns)
  in
  insertGlobal ident el
  (EUsedFunName ident)
  >> return (ident , el)
programAdd (GlobalDeclaration glb) =
  let (global_name, sem, ann_glb) =
        case glb of
          Task ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (getSemanticAnn ann)), ann)
          Resource ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (getSemanticAnn ann)), ann)
          Handler ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (getSemanticAnn ann)), ann)
          Emitter ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (getSemanticAnn ann)), ann)
          Channel ident _type_spec _me _mod ann ->
            (ident, fromJust (getGEntry (getSemanticAnn ann)), ann)
          Const ident _type_spec _e _mod ann ->
            (ident, fromJust (getGEntry (getSemanticAnn ann)), ann)
      el = Located sem (location ann_glb) in
  insertGlobal global_name el
  (EUsedGlobalName global_name) >>
  return (global_name , el)
programAdd (TypeDefinition ty anns) =
  let type_name = identifierType ty in
    case getSemanticAnn anns of
      GTy semTy@(GType _) ->
        let el = Located semTy (location anns) in
        insertGlobal
          type_name el
          (EUsedTypeName type_name)
        >> return (type_name , el)
      _ -> throwError (annotateError Internal EInternalNoGTY)

typeTerminaModule :: 
  Parser.AnnotatedProgram Parser.ParserAnn 
  -> SemanticMonad (SAST.AnnotatedProgram SemanticAnn)
typeTerminaModule = mapM checkAndAdd

  where
    checkAndAdd t = programSeman t >>= \t' -> programAdd t' >> return t'

runTypeChecking
  :: Environment
  -> SemanticMonad a
  -> Either SemanticErrors (a, Environment)
runTypeChecking initSt m = case flip ST.runState initSt . runExceptT $ m of
  (Left err, _) -> Left err
  (Right output, st) -> Right (output, st)
