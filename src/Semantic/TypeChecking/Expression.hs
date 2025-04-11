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
import Control.Monad
import Control.Monad.Except
import Semantic.Errors
-- Semantic Monad
import Semantic.Monad
import qualified Data.Map as M

----------------------------------------
-- Libaries and stuff

import qualified Data.List  (find, sortOn)

-- import Control.Monad.State as ST
import Parser.Types
import qualified Data.Set as S
import qualified Data.List as L

checkConstant :: Location -> SAST.TerminaType SemanticAnn -> SAST.Const SemanticAnn -> SemanticMonad ()
checkConstant loc expected_type (I ti (Just type_c)) =
  -- |type_c| is correct
  checkTerminaType loc type_c >>
  -- | Check that the constant is well-typed
  catchExpectedNum loc EInvalidNumericConstantType (numTyOrFail loc type_c) >>
  -- | Check that the explicit type matches the expected type
  sameTyOrError loc expected_type type_c >>
  -- | Check that the constant is in the range of the type
  checkIntConstant loc type_c ti
checkConstant loc expected_type (I ti Nothing) =
  -- | Check that the expected type is a number
  catchExpectedNum loc EUnexpectedNumericConstant (numTyOrFail loc expected_type) >>
  -- | Check that the constant is in the range of the type
  checkIntConstant loc expected_type ti
checkConstant loc expected_type (B {}) =
  sameTyOrError loc expected_type TBool
checkConstant loc expected_type (C {}) =
  sameTyOrError loc expected_type TChar

checkIntConstant :: Location -> SAST.TerminaType SemanticAnn -> TInteger -> SemanticMonad ()
checkIntConstant loc tyI ti@(TInteger i _) =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I ti (Just tyI)))

-- | Function checking that a TerminaType is well-defined.
-- We are assuming that this function is always called AFTER the type was
-- correctly obtained from the type specifier using the function
-- |typeTypeSpecifier|.  Thus, we do not need to check if the type of structs,
-- enums, etc. are previously defined.  Note that we do not change the
-- |TerminaType| in any way, that's why this function return |()|.
checkTerminaType :: 
  Location -> SAST.TerminaType SemanticAnn -> SemanticMonad ()
checkTerminaType loc (TArray ty s) = 
  checkTerminaType loc ty >>
  arrayTyOrFail loc ty >>
  getExprType s >>= 
    \case { 
      TConstSubtype _ -> return ();
      _ -> throwError $ annotateError loc EExpressionNotConstant
    }
checkTerminaType loc (TMsgQueue ty s) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty >>
  getExprType s >>= \case { 
    TConstSubtype _ -> return ();
    _ -> throwError $ annotateError loc EExpressionNotConstant
  }
checkTerminaType loc (TPool ty s) = 
  checkTerminaType loc ty >>
  getExprType s >>= \case { 
    TConstSubtype _ -> return ();
    _ -> throwError $ annotateError loc EExpressionNotConstant
  }
checkTerminaType loc (TOption ty) = 
  checkTerminaType loc ty >>
  optionTyOrFail loc ty
checkTerminaType loc (TReference _ ty) =
  checkTerminaType loc ty >>
  refTyOrFail loc ty
checkTerminaType loc (TBoxSubtype ty) =
  checkTerminaType loc ty >>
  boxTyOrFail loc ty
checkTerminaType loc (TFixedLocation ty) =
  checkTerminaType loc ty >>
  locTyOrFail loc ty
checkTerminaType loc (TAccessPort ty) =
  checkTerminaType loc ty >>
  accessPortTyOrFail loc ty
checkTerminaType loc (TSinkPort ty _) =
  checkTerminaType loc ty >>
  msgTyOrFail loc ty
checkTerminaType loc (TInPort ty _) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty
checkTerminaType loc (TOutPort ty) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty
checkTerminaType loc (TAllocator ty) = 
  checkTerminaType loc ty >>
  allocTyOrFail loc ty
checkTerminaType loc (TAtomicAccess ty) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicAccessInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomicArrayAccess ty s) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicArrayAccessInvalidType (numTyOrFail loc ty) >>
  getExprType s >>= \case { 
    TConstSubtype _ -> return ();
    _ -> throwError $ annotateError loc EExpressionNotConstant
  }
checkTerminaType loc (TAtomic ty) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomicArray ty s) =
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicArrayInvalidType (numTyOrFail loc ty) >>
  getExprType s >>= \case { 
    TConstSubtype _ -> return ();
    _ -> throwError $ annotateError loc EExpressionNotConstant
  }
checkTerminaType loc (TConstSubtype ty) =
  checkTerminaType loc ty >>
  constTyOrFail loc ty
-- The rest of the types are always well defined, since they were
-- constructed using the |typeTypeSpecifier| function.
checkTerminaType _ _ = return ()

checkParameterType :: Location -> SAST.Parameter SemanticAnn -> SemanticMonad ()
checkParameterType loc p =
    let typeSpec = paramType p in
    unless (parameterTy typeSpec) (throwError (annotateError loc (EInvalidParameterType p))) >>
    checkTerminaType loc typeSpec

checkProcedureParameterType :: Location -> SAST.Parameter SemanticAnn -> SemanticMonad ()
checkProcedureParameterType loc p =
    let typeSpec = paramType p in
    unless (procedureParamTy typeSpec) (throwError (annotateError loc (EInvalidProcedureParameterType typeSpec))) >>
    checkTerminaType loc typeSpec

checkActionParameterType :: Location -> SAST.Parameter SemanticAnn -> SemanticMonad ()
checkActionParameterType loc p =
    let typeSpec = paramType p in
    unless (actionParamTy typeSpec) (throwError (annotateError loc (EInvalidActionParameterType typeSpec))) >>
    checkTerminaType loc typeSpec
    
checkReturnType :: Location -> SAST.TerminaType SemanticAnn -> SemanticMonad ()
checkReturnType anns ty =
  checkTerminaType anns ty >>
  unless (copyTy ty) (throwError (annotateError anns (EInvalidReturnType ty)))

checkUniqueNames :: Location -> ([Identifier] -> Error) -> [Identifier] -> SemanticMonad ()
checkUniqueNames ann err is =
  if allUnique is then return ()
  else throwError $ annotateError ann (err (repeated is))
  where

    allUnique :: Ord a => [a] -> Bool
    allUnique xs = length (S.fromList xs) == length xs

    repeated :: Ord a => [a] -> [a]
    repeated xs = S.toList (S.fromList xs) L.\\ xs

-- | This function
checkEmitterDataType :: Location -> Identifier -> SAST.TerminaType SemanticAnn -> SemanticMonad ()
checkEmitterDataType loc "Interrupt" ty =
  unless (sameTy ty TUInt32) (throwError $ annotateError loc (EInvalidInterruptEmitterType ty))
checkEmitterDataType loc "PeriodicTimer" ty =
  unless (sameTy ty (TStruct "TimeVal")) (throwError $ annotateError loc (EInvalidPeriodicTimerEmitterType ty))
checkEmitterDataType loc "SystemInit" ty =
  unless (sameTy ty (TStruct "TimeVal")) (throwError $ annotateError loc (EInvalidSystemInitEmitterType ty))
checkEmitterDataType _ _ _ = throwError $ annotateError Internal EUnboxingEmittterClass

collectExtendedInterfaces :: Location -> Identifier -> SemanticMonad [Identifier]
collectExtendedInterfaces loc ident = do
  catchError (getGlobalTypeDef loc ident)
    -- If the interface does not exist, we throw an error.
    (\_ -> throwError $ annotateError loc (EInterfaceNotFound ident)) >>= \case {
        -- If the global type is an interface, we return the procedures.
        (LocatedElement (Interface _ _ extends _ _) _) -> (do
          -- If the interface is well-typed, this should not fail:
          extendedTree <- concat <$> mapM (collectExtendedInterfaces loc) extends
          return $ extends ++ extendedTree);
        -- If the global type is not an interface, we throw an error.
        _ -> throwError $ annotateError loc (EGlobalNotInterface ident)
    }

collectInterfaceProcedures :: Location -> Identifier -> SemanticMonad (M.Map Identifier (SAST.InterfaceMember SemanticAnn))
collectInterfaceProcedures loc ident = do
  catchError (getGlobalTypeDef loc ident)
    -- If the interface does not exist, we throw an error.
    (\_ -> throwError $ annotateError loc (EInterfaceNotFound ident)) >>= \case {
        -- If the global type is an interface, we return the procedures.
        (LocatedElement (Interface _ _ extends iface_prcs _) _) -> (do
          -- If the interface is well-typed, this should not fail:
          extededProcs <- mapM (collectInterfaceProcedures loc) extends
          procs <- mapM (\procedure@(InterfaceProcedure procId _ _ _) -> do
              return (procId, procedure)
            ) iface_prcs
          return (M.fromList procs `M.union` M.unions extededProcs));
        -- If the global type is not an interface, we throw an error.
        _ -> throwError $ annotateError loc (EGlobalNotInterface ident)
    }


getMemberField :: Location 
  -> SAST.TerminaType SemanticAnn 
  -> Identifier 
  -> SemanticMonad (SAST.TerminaType SemanticAnn, SemanticAnn)
getMemberField loc obj_ty ident =
  case obj_ty of
    TFixedLocation obj_ty' -> getMemberField loc obj_ty' ident
    TStruct dident -> getGlobalTypeDef loc dident >>=
      \case{
        -- Either a struct
        (LocatedElement (Struct _identTy fields _mods) strLoc) ->
            let mfield = Data.List.find ((ident ==) . fieldIdentifier) fields in
              maybe
              (throwError $ annotateError loc (EMemberAccessUnknownField (dident, strLoc) ident))
              (\fld -> return (fieldTerminaType fld, fieldAnnotation fld)) mfield
        ;
        _ -> throwError $ annotateError Internal EUnboxingStructType
      }
    TGlobal _ dident -> getGlobalTypeDef loc dident >>=
      \case {
        -- Or a class
        LocatedElement (Class _clsKind _identTy cls _implements _mods) clsLoc ->
          -- Find |ident| field in the class.
          case findClassField ident cls of
            Nothing -> throwError $ annotateError loc (EMemberAccessUnknownField (dident, clsLoc) ident)
            -- type |t| and the type inside |a| should be the same, no?
            Just fld -> return fld
        ;
        -- Other types do not have members.
        _ -> throwError $ annotateError Internal EUnboxingClassType
      }
    ty -> throwError $ annotateError loc (EMemberAccessInvalidType ty)

typeObject ::
  -- | Scope of variables. It returns its access kind (mutable or immutable) and its type
  (ParserAnn -> Identifier -> SemanticMonad (AccessKind, SAST.TerminaType SemanticAnn))
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
  ft <- getMemberField ann obj_ty ident
  case ft of
    (fty@(TAccessPort (TInterface _ _)), SemanticAnn (FTy (AccessPortField members)) _) ->
      return $ SAST.MemberAccess typed_obj ident $ buildExpAnnAccessPortObj ann members fty
    (fty, _) -> return $ SAST.MemberAccess typed_obj ident $ buildExpAnnObj ann obj_ak fty
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
    TReference ak rTy -> do
      ft <- getMemberField ann rTy ident
      case ft of
        (fty@(TAccessPort (TInterface _ _)), SemanticAnn (FTy (AccessPortField members)) _) ->
          return $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnAccessPortObj ann members fty
        (fty, _) -> return $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnObj ann ak fty
    ty -> throwError $ annotateError ann $ EDereferenceInvalidType ty

typeMemberFunctionCall ::
  ParserAnn
  -> SAST.TerminaType SemanticAnn -- ^ type of the object
  -> Identifier -- ^ type of the member function to be called
  -> [Expression ParserAnn] -- ^ arguments
  -> SemanticMonad (([SAST.TerminaType SemanticAnn], [SAST.Expression SemanticAnn]), SAST.TerminaType SemanticAnn)
typeMemberFunctionCall ann obj_ty ident args =
  -- Calling a self method or viewer. We must not allow calling a procedure.
  case obj_ty of
    TGlobal _clsKind dident -> getGlobalTypeDef ann dident >>=
      \case{
        -- This case corresponds to a call to an inner method or viewer from the self object.
        LocatedElement (Class _ _identTy cls _provides _mods) _ ->
          case findClassProcedure ident cls of
            Just _ -> throwError $ annotateError ann EInvalidProcedureCallInsideMemberFunction
            Nothing ->
              case findClassViewerOrMethod ident cls of
                Just (ps, _, anns) -> do
                  let (psLen , asLen ) = (length ps, length args)
                  -- Check that the number of parameters are OK
                  when (psLen < asLen) (throwError $ annotateError ann (EMemberFunctionCallExtraArgs (ident, ps, getLocation anns) (fromIntegral asLen)))
                  when (psLen > asLen) (throwError $ annotateError ann (EMemberFunctionCallMissingArgs (ident, ps, getLocation anns) (fromIntegral asLen)))
                  typed_args <- zipWithM (\(p, idx) e ->
                    catchMismatch ann (EMemberFunctionCallArgTypeMismatch (ident, p, getLocation anns) idx)
                      (typeExpression (Just p) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
                  fty <- maybe (throwError $ annotateError Internal EUnboxingMemberFunctionType) return (getTypeSemAnn anns)
                  return ((ps, typed_args), fty)
                Nothing -> throwError $ annotateError ann (EMemberAccessNotFunction ident)
          ;
        _ -> throwError $ annotateError Internal EUnboxingClassType
      }
    TAccessPort (TInterface _ dident) -> getGlobalTypeDef ann dident >>=
      \case{
        LocatedElement (Interface _ _identTy extends members _mods) _ -> (do
          extendedProcedures <- concat <$> mapM (fmap M.elems . collectInterfaceProcedures ann) extends
          case findInterfaceProcedure ident (members ++ extendedProcedures) of
            Nothing -> throwError $ annotateError ann (EUnknownProcedure ident)
            Just (ps, SemanticAnn _ loc) -> do
              let (psLen , asLen) = (length ps, length args)
              when (psLen < asLen) (throwError $ annotateError ann (EProcedureCallExtraArgs (ident, ps, loc) (fromIntegral asLen)))
              when (psLen > asLen) (throwError $ annotateError ann (EProcedureCallMissingArgs (ident, ps, loc) (fromIntegral asLen)))
              typed_args <- zipWithM (\(p, idx) e ->
                catchMismatch ann (EProcedureCallArgTypeMismatch (ident, p, loc) idx)
                  (typeExpression (Just p) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
              return ((ps, typed_args), TUnit)
        );
        _ -> throwError $ annotateError Internal EUnboxingInterface
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

typeModifier :: Location 
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> Modifier ParserAnn 
  -> SemanticMonad (SAST.Modifier SemanticAnn)
typeModifier _loc _typeObj (Modifier ident Nothing) =
  return $ SAST.Modifier ident Nothing
typeModifier loc typeObj (Modifier ident (Just constant)) = do
  typed_const <- typeConstant loc typeObj constant
  return $ SAST.Modifier ident (Just typed_const)

typeConstant :: Location 
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> Const ParserAnn
  -> SemanticMonad (SAST.Const SemanticAnn)
typeConstant _loc _typeObj (I tInt Nothing) = return $ SAST.I tInt Nothing
typeConstant loc typeObj (I tInt (Just ts)) = do
  ty <- typeTypeSpecifier loc typeObj ts
  return $ SAST.I tInt (Just ty)
typeConstant _loc _typeObj (B tBool) = return $ SAST.B tBool
typeConstant _loc _typeObj (C tChar) = return $ SAST.C tChar

-- | Function that translates a |TypeSpecifier| into a |TerminaType|.
typeTypeSpecifier :: Location 
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> PAST.TypeSpecifier ParserAnn 
  -> SemanticMonad (SAST.TerminaType SemanticAnn)
typeTypeSpecifier loc _typeObj (TSDefinedType ident []) = do
  -- Check that the type was defined
  (LocatedElement glbTypeDef _) <- getGlobalTypeDef loc ident
  case glbTypeDef of 
    Struct s _ _ -> return $ TStruct s
    Enum e _ _ -> return $ TEnum e
    Class clsKind c _ _ _ -> return $ TGlobal clsKind c 
    Interface RegularInterface i _ _ _ -> return $ TInterface RegularInterface i
    Interface SystemInterface i _ _ _ -> return $ TInterface SystemInterface i
typeTypeSpecifier loc typeObj ts@(TSDefinedType "Allocator" [typeParam]) = 
  case typeParam of
    TypeParamIdentifier ident -> TAllocator <$> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> TAllocator <$> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
typeTypeSpecifier loc typeObj ts@(TSDefinedType "AtomicAccess" [typeParam]) =
  case typeParam of
    TypeParamIdentifier ident -> TAtomicAccess <$> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> TAtomicAccess <$> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
typeTypeSpecifier loc typeObj ts@(TSDefinedType "AtomicArrayAccess" [typeParam, sizeParam]) = do
  tyTypeParam <- case typeParam of
    TypeParamIdentifier ident -> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  tySizeParam <- case sizeParam of
    TypeParamIdentifier ident ->  typeExpression (Just (TConstSubtype TUSize)) typeObj (AccessObject (Variable ident loc))
    TypeParamSize s -> typeExpression (Just (TConstSubtype TUSize)) typeObj s
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  return $ TAtomicArrayAccess tyTypeParam tySizeParam
typeTypeSpecifier loc typeObj ts@(TSDefinedType "Atomic" [typeParam]) = 
  case typeParam of
    TypeParamIdentifier ident -> TAtomic <$> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> TAtomic <$> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
typeTypeSpecifier loc typeObj ts@(TSDefinedType "AtomicArray" [typeParam, sizeParam]) = do
  tyTypeParam <- case typeParam of
    TypeParamIdentifier ident -> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  tySizeParam <- case sizeParam of
    TypeParamIdentifier ident ->  typeExpression (Just (TConstSubtype TUSize)) typeObj (AccessObject (Variable ident loc))
    TypeParamSize s -> typeExpression (Just (TConstSubtype TUSize)) typeObj s
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  return $ TAtomicArray tyTypeParam tySizeParam
typeTypeSpecifier loc typeObj ts@(TSDefinedType "Option" [typeParam]) = 
  case typeParam of
    TypeParamIdentifier ident -> TOption <$> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> TOption <$> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
typeTypeSpecifier loc typeObj ts@(TSDefinedType "MsgQueue" [typeParam, sizeParam]) = do
  tyTypeParam <- case typeParam of
    TypeParamIdentifier ident -> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  tySizeParam <- case sizeParam of
    TypeParamIdentifier ident ->  typeExpression (Just (TConstSubtype TUSize)) typeObj (AccessObject (Variable ident loc))
    TypeParamSize s -> typeExpression (Just (TConstSubtype TUSize)) typeObj s
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  return $ TMsgQueue tyTypeParam tySizeParam
typeTypeSpecifier loc typeObj ts@(TSDefinedType "Pool" [typeParam, sizeParam]) = do
  tyTypeParam <- case typeParam of
    TypeParamIdentifier ident -> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  tySizeParam <- case sizeParam of
    TypeParamIdentifier ident ->  typeExpression (Just (TConstSubtype TUSize)) typeObj (AccessObject (Variable ident loc))
    TypeParamSize s -> typeExpression (Just (TConstSubtype TUSize)) typeObj s
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  return $ TPool tyTypeParam tySizeParam
typeTypeSpecifier loc typeObj (TSArray ts s) = do
  ty <- typeTypeSpecifier loc typeObj ts
  arraySize <- typeExpression (Just (TConstSubtype TUSize)) typeObj s
  return $ TArray ty arraySize
typeTypeSpecifier loc typeObj (TSReference ak ts) = 
  TReference ak <$> typeTypeSpecifier loc typeObj ts
typeTypeSpecifier loc typeObj (TSBoxSubtype ts) = 
  TBoxSubtype <$> typeTypeSpecifier loc typeObj ts
typeTypeSpecifier loc typeObj (TSLocation ts) = 
  TFixedLocation <$> typeTypeSpecifier loc typeObj ts
typeTypeSpecifier loc typeObj (TSAccessPort ty) =
  TAccessPort <$> typeTypeSpecifier loc typeObj ty
typeTypeSpecifier loc typeObj (TSSinkPort ty action) =
  TSinkPort <$> typeTypeSpecifier loc typeObj ty <*> pure action
typeTypeSpecifier loc typeObj (TSInPort ty action) = 
  TInPort <$> typeTypeSpecifier loc typeObj ty <*> pure action
typeTypeSpecifier loc typeObj (TSOutPort ty) = 
  TOutPort <$> typeTypeSpecifier loc typeObj ty
typeTypeSpecifier loc typeObj (TSConstSubtype ty) =
  TConstSubtype <$> typeTypeSpecifier loc typeObj ty
-- This is explicit just in case
typeTypeSpecifier _ _ TSUInt8  = return TUInt8
typeTypeSpecifier _ _ TSUInt16 = return TUInt16
typeTypeSpecifier _ _ TSUInt32 = return TUInt32
typeTypeSpecifier _ _ TSUInt64 = return TUInt64
typeTypeSpecifier _ _ TSInt8   = return TInt8
typeTypeSpecifier _ _ TSInt16  = return TInt16
typeTypeSpecifier _ _ TSInt32  = return TInt32
typeTypeSpecifier _ _ TSInt64  = return TInt64
typeTypeSpecifier _ _ TSUSize  = return TUSize
typeTypeSpecifier _ _ TSChar   = return TChar
typeTypeSpecifier _ _ TSBool   = return TBool
typeTypeSpecifier _ _ TSUnit   = return TUnit
typeTypeSpecifier loc _ ts = throwError $ annotateError loc (EInvalidTypeSpecifier ts)

typeAssignmentExpression ::
  SAST.TerminaType SemanticAnn ->
  (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn)) ->
  Expression ParserAnn ->
  SemanticMonad (SAST.Expression SemanticAnn)
----------------------------------------
-- | Struct Initializer
typeAssignmentExpression expected_type@(TAtomic ty) typeObj (StructInitializer fs mts pann) = do
  -- | Check field type
  case mts of
    (Just ts) -> do
      init_ty <- typeTypeSpecifier pann typeObj ts
      catchMismatch pann
        (EStructInitializerTypeMismatch expected_type)
        (sameTyOrError pann expected_type init_ty)
    Nothing -> return ()
  -- | The type is ok. Now we have to check the field
  SAST.StructInitializer
        <$> typeFieldAssignments pann (expected_type, Builtin) typeObj [FieldDefinition "value" ty (buildExpAnn Internal ty)] fs
        <*> pure (buildExpAnn pann expected_type)
typeAssignmentExpression expected_type@(TAtomicArray ty size) typeObj (StructInitializer fs mts pann) = do
  -- | Check field type
  case mts of
    (Just ts) -> do
      init_ty <- typeTypeSpecifier pann typeObj ts
      catchMismatch pann
        (EStructInitializerTypeMismatch expected_type)
        (sameTyOrError pann expected_type init_ty)
    Nothing -> return ()
  -- | The type is ok. Now we have to check the field
  SAST.StructInitializer
        <$> typeFieldAssignments pann (expected_type, Builtin) typeObj [FieldDefinition "values" (TArray ty size) (buildExpAnn Internal (TArray ty size))] fs
        <*> pure (buildExpAnn pann expected_type)
typeAssignmentExpression expected_type@(TStruct id_ty) typeObj (StructInitializer fs mts pann) = do
  -- | Check field type
  case mts of
    (Just ts) -> do
      init_ty <- typeTypeSpecifier pann typeObj ts
      catchMismatch pann
        (EStructInitializerTypeMismatch expected_type)
        (sameTyOrError pann expected_type init_ty)
      getGlobalTypeDef pann id_ty
    Nothing -> getGlobalTypeDef pann id_ty
  >>= \case{
    LocatedElement (Struct _ ty_fs _mods) strLoc  ->
      SAST.StructInitializer
        <$> typeFieldAssignments pann (expected_type, strLoc) typeObj ty_fs fs
        <*> pure (buildExpAnn pann (TStruct id_ty));
    _ -> throwError $ annotateError Internal EUnboxingStructType;
  }
typeAssignmentExpression expected_type@(TGlobal _ id_ty) typeObj (StructInitializer fs mts pann) = do
  -- | Check field type
  case mts of
    (Just ts) -> do
      init_ty <- typeTypeSpecifier pann typeObj ts
      catchMismatch pann
        (EStructInitializerTypeMismatch expected_type)
        (sameTyOrError pann expected_type init_ty)
      getGlobalTypeDef pann id_ty
    Nothing -> getGlobalTypeDef pann id_ty
  >>= \case{
    LocatedElement (Class clsKind _ident members _provides _mods) clsLoc ->
      let fields = [fld | ClassField fld@(FieldDefinition {}) <- members] in
        SAST.StructInitializer
        <$> typeFieldAssignments pann (expected_type, clsLoc) typeObj fields fs
        <*> pure (buildExpAnn pann (TGlobal clsKind id_ty));
    _ -> throwError $ annotateError Internal EUnboxingClassType;
  }
typeAssignmentExpression expected_type@(TEnum id_expected) typeObj (EnumVariantInitializer id_ty variant args pann) = do
  unless (id_expected == id_ty) (throwError $ annotateError pann (EEnumInitializerExpectedTypeMismatch expected_type (TEnum id_ty)))
  -- | Enum Variant
  getEnumTy pann id_ty
  >>= \case {
    LocatedElement (Enum enumId ty_vs _mods) loc ->
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
    TArray ts _ -> do
      -- | We do not need to catch any error, since it will be correctly handler
      -- by the recursive call to |typeAssignmentExpression|
      typed_init <- typeAssignmentExpression ts typeObj iexp
      typed_init_size <- typeExpression (Just (TConstSubtype TUSize)) typeObj size
      -- We will check the size of the array when we are performing the const propagation.
      return $ SAST.ArrayInitializer typed_init typed_init_size (buildExpAnn pann (TArray ts typed_init_size))
    ts -> throwError $ annotateError pann (EArrayInitializerNotArray ts)
typeAssignmentExpression expectedType typeObj (ArrayExprListInitializer exprs pann) = do
  case expectedType of
    TArray ts _s -> do
      typed_exprs <- mapM (\e ->
        catchError (typeAssignmentExpression ts typeObj e) (\err -> case getError err of
          EMismatch _ ty -> throwError $ annotateError (getAnnotation e) (EArrayExprListInitializerExprTypeMismatch ts ty)
          EAssignmentExprMismatch _ ty -> throwError $ annotateError (getAnnotation e) (EArrayExprListInitializerExprTypeMismatch ts ty)
          _ -> throwError err)) exprs
      -- We will check the size of the array when we are performing the const propagation.
      -- The size of the expression list initializer will be the number of its elements
      let typed_init_size = SAST.Constant 
            (SAST.I (TInteger (fromIntegral $ length typed_exprs) DecRepr) 
              (Just (TConstSubtype TUSize))) (buildExpAnn pann (TConstSubtype TUSize)) 
      return $ SAST.ArrayExprListInitializer typed_exprs (buildExpAnn pann (TArray ts typed_init_size))
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
typeAssignmentExpression expectedType _typeObj (StringInitializer value pann) = do
-- | TArray Initialization
  case expectedType of
    TArray TChar _ -> do
      -- We will check the size of the array when we are performing the const propagation.
      -- The size of the expression list initializer will be the number of its elements
      let typed_init_size = 
            SAST.Constant (SAST.I (TInteger (fromIntegral $ length value) DecRepr) (Just (TConstSubtype TUSize)))
                (buildExpAnn pann (TConstSubtype TUSize))
      return $ SAST.StringInitializer value (buildExpAnn pann (TArray TChar typed_init_size))
    ty -> throwError $ annotateError pann (EStringInitializerNotArrayOfChars ty)
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
  Maybe (SAST.TerminaType SemanticAnn) ->
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
      (Just (TConstSubtype ts), TConstSubtype ts') -> do
        -- If the type must be an expected type, then check it.
        sameTyOrError (getAnnotation obj) ts ts'
        return $ SAST.AccessObject typed_obj
      (Just (TConstSubtype ts), ts') -> do
        -- If the type must be an expected type, then check it.
        sameTyOrError (getAnnotation obj) ts ts'
        -- We need to return an error because the type is not a const type.
        throwError $ annotateError (getAnnotation obj) EExpressionNotConstant
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
typeExpression (Just expectedType) typeObj (Constant c pann) = do
  typed_c <- typeConstant pann typeObj c
  -- | Call the function that checks that the constant is of the expected type.
  checkConstant pann expectedType typed_c
  return $ SAST.Constant typed_c (buildExpAnn pann expectedType)
-- | Integer literals without an expected type but with a known type.
typeExpression Nothing typeObj (Constant c@(I _ (Just ts)) pann) = do
  ty <- typeTypeSpecifier pann typeObj ts
  typed_c <- typeConstant pann typeObj c
  return $ SAST.Constant typed_c (buildExpAnn pann ty)
-- | Integer literals without an expected type and without a known type.
-- This is an error, since we cannot infer the type of the constant.
typeExpression Nothing _ (Constant (I tInt Nothing) pann) = do
  throwError $ annotateError pann $ EConstantWithoutKnownType (SAST.I tInt Nothing)
-- | Boolean literals without an expected type.
typeExpression Nothing typeObj (Constant c@(B {}) pann) = do
  typed_c <- typeConstant pann typeObj c
  return $ SAST.Constant typed_c (buildExpAnn pann TBool)
-- | Character literals without an expected type.
typeExpression Nothing typeObj (Constant c@(C {}) pann) = do
  typed_c <- typeConstant pann typeObj c
  return $ SAST.Constant typed_c (buildExpAnn pann TChar)
typeExpression expectedType typeObj (Casting e nts pann) = do
  nty <- typeTypeSpecifier pann typeObj nts
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
      (SAST.TerminaType SemanticAnn -> Bool)
      -- | Left hand side error constructor
      -> (SAST.TerminaType SemanticAnn -> Error)
      -- | Right hand side error constructor
      -> (SAST.TerminaType SemanticAnn -> Error)
      -- | Left hand side expression
      -> Expression ParserAnn
      -- | Right hand side expression
      -> Expression ParserAnn
      -- | Typed expressions
      -> SemanticMonad (SAST.TerminaType SemanticAnn, SAST.Expression SemanticAnn, SAST.Expression SemanticAnn)
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
  maybe (return ()) (flip (sameTyOrError ann) fty) expectedType
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
      maybe (return ()) (flip (sameTyOrError ann) fty) expectedType
      return $ SAST.DerefMemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)
    ty -> throwError $ annotateError ann $ EDereferenceInvalidType ty
typeExpression expectedType typeObj (IsEnumVariantExpression obj id_ty variant_id pann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjType obj_typed
  LocatedElement lhs_ty _ <- case obj_ty of
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
typeExpression _ _ (StringInitializer _ pann) = throwError $ annotateError pann EStringInitializerInvalidUse

typeFieldAssignment
  :: Location -> (SAST.TerminaType SemanticAnn, Location)
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> SAST.FieldDefinition SemanticAnn
  -> FieldAssignment ParserAnn
  -> SemanticMonad (SAST.FieldAssignment SemanticAnn)
typeFieldAssignment loc tyDef typeObj (FieldDefinition fid fty _) (FieldValueAssignment faid faexp pann) =
  if fid == faid
  then
    flip (SAST.FieldValueAssignment faid) (buildStmtAnn pann) <$> typeAssignmentExpression fty typeObj faexp
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [faid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty _) (FieldAddressAssignment faid addr pann) =
  if fid == faid
  then
    case fty of
      TFixedLocation _ -> return $ SAST.FieldAddressAssignment faid addr (buildExpAnn pann fty)
      ty -> throwError $ annotateError loc (EFieldNotFixedLocation fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [faid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty _) (FieldPortConnection InboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry pann sid >>=
    \gentry ->
    case fty of
      TSinkPort ty action  ->
        case gentry of
          LocatedElement  (GGlob ets@(TGlobal EmitterClass clsId)) _ -> do
            checkEmitterDataType loc clsId ty
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildSinkPortConnAnn pann ets action)
          _ -> throwError $ annotateError loc $ ESinkPortConnectionInvalidGlobal sid
      TInPort ty action  ->
        case gentry of
          LocatedElement (GGlob cts@(TMsgQueue ty' _)) _ -> do
            catchMismatch pann (EInboundPortConnectionMsgQueueTypeMismatch sid ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildInPortConnAnn pann cts action)
          _ -> throwError $ annotateError loc $ EInboundPortConnectionInvalidObject sid
      ty -> throwError $ annotateError loc (EFieldNotSinkOrInboundPort fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [pid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty _) (FieldPortConnection OutboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry pann sid >>=
    \gentry ->
    case fty of
      TOutPort ty ->
        case gentry of
          LocatedElement (GGlob cts@(TMsgQueue ty' _)) _ -> do
            catchMismatch pann (EOutboundPortConnectionMsgQueueTypeMismatch sid ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection OutboundPortConnection pid sid (buildOutPortConnAnn pann cts)
          _ -> throwError $ annotateError loc $ EOutboundPortConnectionInvalidGlobal sid
      ty -> throwError $ annotateError loc (EFieldNotOutboundPort fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [pid])
typeFieldAssignment loc tyDef _ (FieldDefinition fid fty fann) (FieldPortConnection AccessPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry pann sid >>=
    \gentry ->
    case fty of
      TAccessPort (TAllocator ty) ->
        case gentry of
          LocatedElement (GGlob (TPool ty' s)) _ -> do
            catchMismatch pann (EAllocatorPortConnectionPoolTypeMismatch sid ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildPoolConnAnn pann ty s)
          _ -> throwError $ annotateError loc $ EAllocatorPortConnectionInvalidGlobal sid
      TAccessPort (TAtomicAccess ty) ->
        case gentry of
          LocatedElement (GGlob (TAtomic ty')) _ -> do
            catchMismatch pann (EAtomicConnectionTypeMismatch ty) (sameTyOrError loc ty ty')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicConnAnn pann ty)
          _ -> throwError $ annotateError loc $ EAtomicAccessPortConnectionInvalidGlobal sid
      TAccessPort (TAtomicArrayAccess ty s) ->
        case gentry of
          LocatedElement (GGlob (TAtomicArray ty' _)) _ -> do
            catchMismatch pann (EAtomicArrayConnectionTypeMismatch ty) (sameTyOrError loc ty ty')
            -- We will check the size of the atomic array when we are performing the const propagation.
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicArrayConnAnn pann ty s)
          _ -> throwError $ annotateError loc $ EAtomicArrayAccessPortConnectionInvalidGlobal sid
      TAccessPort ifaceTy@(TInterface _ iface) -> do
        -- | Get the interface definition
        getGlobalTypeDef loc iface >>=
          \case {
            LocatedElement (Interface _ _ extends members _) _ -> (do
              -- Collect the procedures of the interface
              extendedMembers <- concat <$> mapM (fmap M.elems . collectInterfaceProcedures loc) extends
              let procs = [ProcedureSeman procid (map paramType params) [] | (InterfaceProcedure procid params _ _) <- members ++ extendedMembers]
              -- Check that the resource provides the interface
              case gentry of
                LocatedElement (GGlob rts@(TGlobal ResourceClass clsId)) _ ->
                  getGlobalTypeDef loc clsId >>=
                  \case {
                      LocatedElement (Class _ _ _ provides _) _ -> (do
                        -- Collect the interfaces provided by the resource and their extended interfaces
                        extendedProvides <- concat <$> mapM (collectExtendedInterfaces loc) provides;
                        case Data.List.find (iface ==) (provides ++ extendedProvides) of
                          Just _ ->
                            case fann of
                              (SemanticAnn (FTy (AccessPortField usedProcs)) _) ->
                                let filteredProcs = filter (\(ProcedureSeman procid _ _) -> M.member procid usedProcs) procs in
                                return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAccessPortConnAnn pann ifaceTy rts filteredProcs)
                              _ -> error "Invalid annotation for field"
                          _ -> throwError $ annotateError loc $ EAccessPortConnectionInterfaceNotProvided sid iface
                      );
                      _ -> throwError $ annotateError Internal EUnboxingInterface
                  }
                _ -> throwError $ annotateError loc $ EAccessPortConnectionInvalidGlobal sid
            );
            _ -> throwError $ annotateError Internal EUnboxingInterface
          }
      ty -> throwError $ annotateError loc (EFieldNotAccessPort fid ty)
  else throwError $ annotateError loc (EFieldValueAssignmentUnknownFields tyDef [pid])

typeFieldAssignments
  :: Location -> (SAST.TerminaType SemanticAnn, Location)
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> [SAST.FieldDefinition SemanticAnn]
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
      if fieldIdentifier d == getFid a then
        typeFieldAssignment faLoc tyDef typeObj d a >>= checkSortedFields ds as . (:acc)
      else
        throwError $ annotateError faLoc (EFieldValueAssignmentMissingFields tyDef [fieldIdentifier d])
