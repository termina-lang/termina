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
import qualified Control.Monad.State as ST
import Semantic.Environment
import Utils.Monad 
import Semantic.Utils

checkObjectNotMoved :: Location -> SAST.Object a -> SemanticMonad (SAST.Object a)
checkObjectNotMoved loc obj = do
  let objectHash = getMovedHash obj Nothing
  movedObjects <- ST.gets moved
  case M.lookup objectHash movedObjects of
    Nothing -> return obj
    Just movedLocation -> throwError $ annotateError loc (EObjectPreviouslyMoved movedLocation)

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
checkConstant loc expected_type Null =
  sameTyOrError loc expected_type TUnit

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
checkTerminaType loc (TResult tyOk tyError) = 
  checkTerminaType loc tyOk >>
  checkTerminaType loc tyError >>
  resultTyOrFail loc tyOk >>
  resultTyOrFail loc tyError
checkTerminaType loc (TStatus ty) = 
  checkTerminaType loc ty >>
  statusTyOrFail loc ty
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
  
checkViewerParameterType :: Location -> SAST.Parameter SemanticAnn -> SemanticMonad ()
checkViewerParameterType loc p =
    let typeSpec = paramType p in
    unless (viewerParamTy typeSpec) (throwError (annotateError loc (EInvalidViewerParameterType typeSpec))) >>
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
checkEmitterDataType loc "SystemExcept" ty =
  unless (sameTy ty (TEnum "Exception")) (throwError $ annotateError loc (EInvalidSystemExceptEmitterType ty))
checkEmitterDataType _ _ _ = throwError $ annotateError Internal EInvalidEmitterClass

checkEmitterActionReturnType :: Location -> Identifier -> Identifier -> SAST.TerminaType SemanticAnn -> SemanticMonad ()
checkEmitterActionReturnType loc "Interrupt" action ty =
  unless (sameTy ty (TStatus TInt32)) (throwError $ annotateError loc (EInvalidInterruptActionReturnType action ty))
checkEmitterActionReturnType loc "PeriodicTimer" action ty =
  unless (sameTy ty (TStatus TInt32)) (throwError $ annotateError loc (EInvalidPeriodicTimerActionReturnType action ty))
checkEmitterActionReturnType loc "SystemInit" action ty =
  unless (sameTy ty (TStatus TInt32)) (throwError $ annotateError loc (EInvalidSystemInitActionReturnType action ty))
checkEmitterActionReturnType loc "SystemExcept" action ty =
  unless (sameTy ty TUnit) (throwError $ annotateError loc (EInvalidSystemExceptActionReturnType action ty))
checkEmitterActionReturnType _ _ _ _ = throwError $ annotateError Internal EInvalidEmitterClass

collectExtendedInterfaces :: Location -> Identifier -> SemanticMonad [Identifier]
collectExtendedInterfaces loc ident = do
  catchError (getGlobalTypeDef loc ident)
    -- If the interface does not exist, we throw an error.
    (\_ -> throwError $ annotateError loc (EInterfaceNotFound ident)) >>= \case {
        -- If the global type is an interface, we return the procedures.
        (LocatedElement (Interface _ _ extends _ _) _) -> (do
          -- If the interface is well-typed, this should not fail:
          extendedTree <- concat <$> traverse (collectExtendedInterfaces loc) extends
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
          procs <- mapM (\procedure@(InterfaceProcedure _ procId _ _ _) -> do
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
        _ -> throwError $ annotateError Internal EExpectedStructType
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
        _ -> throwError $ annotateError Internal EExpectedClassType
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
  checkObjectNotMoved ann $ SAST.Variable ident (buildExpAnnObj ann ak ty)
typeObject getVarTy (ArrayIndexExpression obj idx ann) = do
  typed_obj <- typeObject getVarTy obj
  (obj_ak, obj_ty) <- getObjType typed_obj
  case obj_ty of
    TArray ty_elems _vexp -> do
        idx_typed  <- catchMismatch (getAnnotation idx) EArrayIndexNotUSize (typeExpression (Just TUSize) typeRHSObject idx)
        checkObjectNotMoved ann $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann obj_ak ty_elems
    TReference ref_ak (TArray ty_elems _vexp) -> do
        idx_typed  <- catchMismatch (getAnnotation idx) EArrayIndexNotUSize (typeExpression (Just TUSize) typeRHSObject idx)
        checkObjectNotMoved ann $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann ref_ak ty_elems
    TFixedLocation (TArray ty_elems _vexp) -> do
        idx_typed  <- catchMismatch (getAnnotation idx) EArrayIndexNotUSize (typeExpression (Just TUSize) typeRHSObject idx)
        checkObjectNotMoved ann $ SAST.ArrayIndexExpression typed_obj idx_typed $ buildExpAnnObj ann obj_ak ty_elems
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
  typed_obj' <- typeObject getLHSVarTy obj
  (obj_ak', obj_ty') <- getObjType typed_obj'
  let (typed_obj, obj_ak, obj_ty) =
        maybe (typed_obj', obj_ak', obj_ty') (unBox typed_obj', Mutable, ) (isBox obj_ty')
  ft <- getMemberField ann obj_ty ident
  case ft of
    (fty@(TAccessPort (TInterface _ _)), SemanticAnn (FTy (AccessPortField members)) _) ->
      checkObjectNotMoved ann $ SAST.MemberAccess typed_obj ident $ buildExpAnnAccessPortObj ann obj_ak members fty
    (fty, _) -> checkObjectNotMoved ann $ SAST.MemberAccess typed_obj ident $ buildExpAnnObj ann obj_ak fty
typeObject getVarTy (Dereference obj ann) = do
  typed_obj <- typeObject getVarTy obj
  (_, obj_ty) <- getObjType typed_obj
  case obj_ty of
    TReference ak ty -> checkObjectNotMoved ann $ SAST.Dereference typed_obj $ buildExpAnnObj ann ak ty
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
          checkObjectNotMoved ann $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnAccessPortObj ann ak members fty
        (fty, _) -> checkObjectNotMoved ann $ SAST.DereferenceMemberAccess typed_obj ident $ buildExpAnnObj ann ak fty
    ty -> throwError $ annotateError ann $ EDereferenceInvalidType ty

typeMemberFunctionCall ::
  ParserAnn
  -> AccessKind
  -> SAST.TerminaType SemanticAnn -- ^ type of the object
  -> Identifier -- ^ type of the member function to be called
  -> [Expression ParserAnn] -- ^ arguments
  -> SemanticMonad (([SAST.Parameter SemanticAnn], [SAST.Expression SemanticAnn]), SAST.TerminaType SemanticAnn)
typeMemberFunctionCall ann ak obj_ty ident args =
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
                  typed_args <- localScope $ zipWithM (\(p, idx) e ->
                    catchMismatch ann (EMemberFunctionCallArgTypeMismatch (ident, p, getLocation anns) idx)
                      (
                        case paramType p of 
                          (TConstSubtype pty) -> do
                            -- | See FunctionCall comment
                            typedArgument <- typeExpression (Just (TConstSubtype pty)) typeRHSObject e
                            ST.modify $ \s -> s { global = M.insert (paramIdentifier p) (LocatedElement (GConstExpr pty typedArgument) (getAnnotation e)) (global s) }
                            return typedArgument
                          ty -> typeExpression (Just ty) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
                  fty <- maybe (throwError $ annotateError Internal EInvalidMemberFunctionTypeAnnotation) return (getTypeSemAnn anns)
                  return ((ps, typed_args), fty)
                Nothing -> throwError $ annotateError ann (EMemberAccessNotFunction ident)
          ;
        _ -> throwError $ annotateError Internal EExpectedClassType
      }
    TAccessPort (TInterface _ dident) -> do
        getGlobalTypeDef ann dident >>=
          \case {
            LocatedElement (Interface _ _identTy extends members _mods) _ -> (do
              extendedProcedures <- concat <$> traverse (fmap M.elems . collectInterfaceProcedures ann) extends
              case findInterfaceProcedure ident (members ++ extendedProcedures) of
                Nothing -> throwError $ annotateError ann (EUnknownProcedure ident)
                Just (ak', ps, SemanticAnn _ loc) -> do
                  -- | Check that the access kind is correct.
                  -- If the self reference is immutable, we cannot call a mutable procedure.
                  when (ak == Immutable && ak' /= Immutable) (throwError $ annotateError ann EInvalidAccessToProcedureFromImmutableSelfReference)
                  let (psLen , asLen) = (length ps, length args)
                  when (psLen < asLen) (throwError $ annotateError ann (EProcedureCallExtraArgs (ident, ps, loc) (fromIntegral asLen)))
                  when (psLen > asLen) (throwError $ annotateError ann (EProcedureCallMissingArgs (ident, ps, loc) (fromIntegral asLen)))
                  typed_args <- localScope $ zipWithM (\(p, idx) e ->
                    catchMismatch ann (EProcedureCallArgTypeMismatch (ident, p, loc) idx)
                      (
                            case paramType p of 
                              (TConstSubtype pty) -> do
                                -- | See FunctionCall comment
                                typedArgument <- typeExpression (Just (TConstSubtype pty)) typeRHSObject e
                                ST.modify $ \s -> s { global = M.insert (paramIdentifier p) (LocatedElement (GConstExpr pty typedArgument) (getAnnotation e)) (global s) }
                                return typedArgument
                              ty -> typeExpression (Just ty) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
                  return ((ps, typed_args), TUnit)
            );
            _ -> throwError $ annotateError Internal EExpectedInterfaceType
      }
    TAccessPort (TAllocator ty_pool) -> do
      when (ak == Immutable) (throwError $ annotateError ann EInvalidAccessToProcedureFromImmutableSelfReference)
      case ident of
        "alloc" ->
          case args of
            [opt] -> do
              typed_arg <- catchMismatch ann (EProcedureCallArgTypeMismatch ("alloc", Parameter "opt" (TReference Mutable (TOption (TBoxSubtype ty_pool))), Builtin) 0)
                (typeExpression (Just (TReference Mutable (TOption (TBoxSubtype ty_pool)))) typeRHSObject opt)
              return (([Parameter "opt" (TReference Mutable (TOption (TBoxSubtype ty_pool)))], [typed_arg]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("alloc", [Parameter "opt" (TReference Mutable (TOption (TBoxSubtype ty_pool)))], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("alloc", [Parameter "opt" (TReference Mutable (TOption (TBoxSubtype ty_pool)))], Builtin) (fromIntegral (length args)))
        "free" ->
          case args of
            [elemnt] -> do
              typed_arg <- catchMismatch ann (EProcedureCallArgTypeMismatch ("free", Parameter "element" (TBoxSubtype ty_pool), Builtin) 0)
                (typeExpression (Just (TBoxSubtype ty_pool)) typeRHSObject elemnt)
              return (([Parameter "element" (TBoxSubtype ty_pool)], [typed_arg]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("free", [Parameter "element" (TBoxSubtype ty_pool)], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("free", [Parameter "element" (TBoxSubtype ty_pool)], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    TAccessPort (TAtomicAccess ty_atomic) -> do
      case ident of
        "load" ->
          case args of
            [retval] -> do
              typed_arg <- catchMismatch ann (EProcedureCallArgTypeMismatch ("load", Parameter "p" (TReference Mutable ty_atomic), Builtin) 0)
                (typeExpression (Just (TReference Mutable ty_atomic)) typeRHSObject retval)
              return (([Parameter "p" (TReference Mutable ty_atomic)], [typed_arg]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("load", [Parameter "p" (TReference Mutable ty_atomic)], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("load", [Parameter "p" (TReference Mutable ty_atomic)], Builtin) (fromIntegral (length args)))
        "store" -> do
          when (ak == Immutable) (throwError $ annotateError ann EInvalidAccessToProcedureFromImmutableSelfReference)
          case args of
            [value] -> do
              typed_value <- catchMismatch ann (EProcedureCallArgTypeMismatch ("store", Parameter "v" ty_atomic, Builtin) 0)
                (typeExpression (Just ty_atomic) typeRHSObject value)
              return (([Parameter "v" ty_atomic], [typed_value]), TUnit)
            [] -> throwError $ annotateError ann (EProcedureCallMissingArgs ("store", [Parameter "v" ty_atomic], Builtin) 0)
            _ -> throwError $ annotateError ann (EProcedureCallExtraArgs ("store", [Parameter "v" ty_atomic], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    TAccessPort (TAtomicArrayAccess ty_atomic _size) ->
      case ident of
        "load_index" ->
          case args of
            [index, retval] -> do
              typed_idx <- catchMismatch ann (EProcedureCallArgTypeMismatch ("load_index", Parameter "idx" TUSize, Builtin) 0)
                (typeExpression (Just TUSize) typeRHSObject index)
              typed_ref <- catchMismatch ann (EProcedureCallArgTypeMismatch ("load_index", Parameter "p" (TReference Mutable ty_atomic), Builtin) 1)
                (typeExpression (Just (TReference Mutable ty_atomic)) typeRHSObject retval)
              return (([Parameter "idx" TUSize, Parameter "p" (TReference Mutable ty_atomic)], [typed_idx, typed_ref]), TUnit)
            _ -> if length args < 2 then
              throwError $ annotateError ann (EProcedureCallMissingArgs ("load_index", [Parameter "idx" TUSize, Parameter "p" (TReference Mutable ty_atomic)], Builtin) (fromIntegral (length args)))
              else throwError $ annotateError ann (EProcedureCallExtraArgs ("load_index", [Parameter "idx" TUSize, Parameter "p" (TReference Mutable ty_atomic)], Builtin) (fromIntegral (length args)))
        "store_index" -> do
          when (ak == Immutable) (throwError $ annotateError ann EInvalidAccessToProcedureFromImmutableSelfReference)
          case args of
            [index, retval] -> do
              typed_idx <- catchMismatch ann (EProcedureCallArgTypeMismatch ("store_index", Parameter "idx" TUSize, Builtin) 0)
                (typeExpression (Just TUSize) typeRHSObject index)
              typed_value <- catchMismatch ann (EProcedureCallArgTypeMismatch ("store_index", Parameter "v" ty_atomic, Builtin) 1)
                (typeExpression (Just ty_atomic) typeRHSObject retval)
              return (([Parameter "idx" TUSize, Parameter "v" ty_atomic], [typed_idx, typed_value]), TUnit)
            _ -> if length args < 2 then
              throwError $ annotateError ann (EProcedureCallMissingArgs ("store_index", [Parameter "idx" TUSize, Parameter "v" ty_atomic], Builtin) (fromIntegral (length args)))
              else throwError $ annotateError ann (EProcedureCallExtraArgs ("store_index", [Parameter "idx" TUSize, Parameter "v" ty_atomic], Builtin) (fromIntegral (length args)))
        _ -> throwError $ annotateError ann (EUnknownProcedure ident)
    TOutPort ty -> do
      when (ak == Immutable) (throwError $ annotateError ann EInvalidAccessToOutPortFromImmutableSelfReference)
      case ident of
        -- send(T)
        "send" ->
          case args of
            [elemnt] -> do
              -- Type the first argument element
              typed_element <- catchMismatch ann (EOutboundPortArgTypeMismatch ty) (typeExpression (Just ty) typeRHSObject elemnt)
              return (([Parameter "element" ty] , [typed_element]), TUnit)
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
typeConstant _loc _typeObj Null = return SAST.Null

-- | Function that translates a |TypeSpecifier| into a |TerminaType|.
typeTypeSpecifier :: Location 
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> PAST.TypeSpecifier ParserAnn 
  -> SemanticMonad (SAST.TerminaType SemanticAnn)
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
typeTypeSpecifier loc typeObj ts@(TSDefinedType "Result" [typeParamOk, typeParamError]) = do
  tyOk <- case typeParamOk of
    TypeParamIdentifier ident -> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  tyError <- case typeParamError of
    TypeParamIdentifier ident -> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> typeTypeSpecifier loc typeObj ts'
    _ -> throwError $ annotateError loc (EInvalidTypeSpecifier ts)
  return $ TResult tyOk tyError  
typeTypeSpecifier loc _ ts@(TSDefinedType "Result" _) =
  throwError $ annotateError loc (EInvalidResultTypeSpecifier ts)
typeTypeSpecifier loc typeObj ts@(TSDefinedType "Status" [typeParam]) = 
  case typeParam of
    TypeParamIdentifier ident -> TStatus <$> typeTypeSpecifier loc typeObj (TSDefinedType ident [])
    TypeParamTypeSpec ts' -> TStatus <$> typeTypeSpecifier loc typeObj ts'
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
  arraySize <- catchMismatch loc EArrayIndexNotUSize 
      (typeExpression (Just (TConstSubtype TUSize)) typeObj s)
  return $ TArray ty arraySize
typeTypeSpecifier loc _typeObj (TSDefinedType ident []) = do
  -- Check that the type was defined
  (LocatedElement glbTypeDef loc') <- getGlobalTypeDef loc ident
  -- | Check that the location of the type is in the dependencies set
  -- of the current module. This is important because the type
  -- can be defined in another module and we need to check that
  -- the module is properly imported.
  case loc' of
    Position qualifiedName _ _ -> do
      -- | Check that the type is defined in the current module
      -- | or in a module that is imported.
      visibleMods <- ST.gets visible
      if S.member qualifiedName visibleMods then
        return ()
      else
        throwError $ annotateError loc (ETypeNotInScope ident qualifiedName)
    _ -> return ()

  case glbTypeDef of 
    Struct s _ _ -> return $ TStruct s
    Enum e _ _ -> return $ TEnum e
    Class clsKind c _ _ _ -> return $ TGlobal clsKind c 
    Interface RegularInterface i _ _ _ -> return $ TInterface RegularInterface i
    Interface SystemInterface i _ _ _ -> return $ TInterface SystemInterface i
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
    _ -> throwError $ annotateError Internal EExpectedStructType;
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
    _ -> throwError $ annotateError Internal EExpectedClassType;
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
                catchMismatch pann (EEnumVariantParamTypeMismatch (enumId, loc) (variant, position, ty)) (typeAssignmentExpression ty typeObj e)) (zip ps [0 :: Integer ..]) args
          else if psLen < asLen
          then throwError $ annotateError pann (EEnumVariantExtraParams (enumId, loc) (variant, ps) (fromIntegral asLen))
          else throwError $ annotateError pann (EEnumVariantMissingParams (enumId, loc) (variant, ps) (fromIntegral asLen))
     ;
    _ -> throwError $ annotateError Internal EExpectedEnumType
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
typeAssignmentExpression expectedType typeObj (MonadicVariantInitializer vexp anns) =
  case expectedType of
    TOption ts -> do
      case vexp of
        None -> return $ SAST.MonadicVariantInitializer None (buildExpAnn anns (TOption ts))
        Some e -> do
          typed_e <- catchMismatch (getAnnotation e) (EMonadicVariantParameterTypeMismatch ts) $ typeAssignmentExpression ts typeObj e
          return $ SAST.MonadicVariantInitializer (Some typed_e) (buildExpAnn anns (TOption ts))
        Ok _ -> throwError $ annotateError anns (EInvalidVariantForOption "Ok")
        Error _ -> throwError $ annotateError anns (EInvalidVariantForOption "Error")
        Success -> throwError $ annotateError anns (EInvalidVariantForOption "Success")
        Failure _ -> throwError $ annotateError anns (EInvalidVariantForOption "Failure")
    TResult tsOk tsError -> do
      case vexp of
        Ok e -> do
          typed_e <- catchMismatch (getAnnotation e) (EMonadicVariantParameterTypeMismatch tsOk) $ typeAssignmentExpression tsOk typeObj e
          return $ SAST.MonadicVariantInitializer (Ok typed_e) (buildExpAnn anns (TResult tsOk tsError))
        Error e -> do
          typed_e <- catchMismatch (getAnnotation e) (EMonadicVariantParameterTypeMismatch tsError) $ typeAssignmentExpression tsError typeObj e
          return $ SAST.MonadicVariantInitializer (Error typed_e) (buildExpAnn anns (TResult tsOk tsError))
        Success -> throwError $ annotateError anns (EInvalidVariantForResult "Success")
        Failure _ -> throwError $ annotateError anns (EInvalidVariantForResult "Failure")
        None -> throwError $ annotateError anns (EInvalidVariantForResult "None")
        Some _ -> throwError $ annotateError anns (EInvalidVariantForResult "Some")
    TStatus ts -> do
      case vexp of
        Success -> return $ SAST.MonadicVariantInitializer Success (buildExpAnn anns (TStatus ts))
        Failure e -> do
          typed_e <- catchMismatch (getAnnotation e) (EMonadicVariantParameterTypeMismatch ts) $ typeAssignmentExpression ts typeObj e
          return $ SAST.MonadicVariantInitializer (Failure typed_e) (buildExpAnn anns (TStatus ts))
        None -> throwError $ annotateError anns (EInvalidVariantForStatus "None")
        Some _ -> throwError $ annotateError anns (EInvalidVariantForStatus "Some")
        Ok _ -> throwError $ annotateError anns (EInvalidVariantForStatus "Ok")
        Error _ -> throwError $ annotateError anns (EInvalidVariantForStatus "Error")
    _ -> throwError $ annotateError anns EMonadicVariantInitializerInvalidUse
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
typeAssignmentExpression expectedType@(TBoxSubtype _) typeObj expr = do
  catchMismatch (getAnnotation expr) (EAssignmentExprMismatch expectedType) (typeExpression (Just expectedType) typeObj expr)
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
  case obj of 
    Variable ident loc -> do
      -- | This is the case of a single variable access.
      -- | We need to check if the variable name refers to a constexpression or not
      glb <- ST.gets global
      case M.lookup ident glb of
        Just (LocatedElement (GConstExpr _ty expr) _) -> do
          exprType <- getExprType expr
          case (expectedType, exprType) of
            (Just (TConstSubtype ts), TConstSubtype ts') -> do
              -- If the type must be an expected type, then check it.
              sameTyOrError loc ts ts'
              return expr
            (Just (TConstSubtype ts), ts') -> do
              -- If the type must be an expected type, then check it.
              sameTyOrError loc ts ts'
              -- We need to return an error because the type is not a const type.
              throwError $ annotateError loc EExpressionNotConstant
            (Just ts, ts') -> do
              -- If the type must be an expected type, then check it.
              sameTyOrError loc ts ts'
              return expr
            -- If we are requesting an object without an expected type, then we must
            -- be casting the result. Thus, we must return an unboxed object.
            (Nothing, _) ->
              return expr
        _ -> typeObjExpression
    _ -> typeObjExpression

  where

    typeObjExpression :: SemanticMonad (SAST.Expression SemanticAnn)
    typeObjExpression = do
      -- | Type the object
      typed_obj <- typeObj obj
      -- | Get the type of the object
      (_, obj_type) <- getObjType typed_obj
      case (expectedType, obj_type) of
        (Just (TBoxSubtype ts), TBoxSubtype ts') -> do
          -- If the type must match an expected type, then check it.
          sameTyOrError (getAnnotation obj) ts ts'
          return $ SAST.AccessObject typed_obj
        (Just ts, TBoxSubtype ts') -> do
          -- If the type must match an expected type, then check it.
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
typeExpression (Just (TConstSubtype expectedType)) typeObj (Constant c pann) = do
  typed_c <- typeConstant pann typeObj c
  -- | Call the function that checks that the constant is of the expected type.
  checkConstant pann expectedType typed_c
  return $ SAST.Constant typed_c (buildExpAnn pann (TConstSubtype expectedType))
typeExpression (Just expectedType) typeObj (Constant c pann) = do
  typed_c <- typeConstant pann typeObj c
  -- | Call the function that checks that the constant is of the expected type.
  checkConstant pann expectedType typed_c
  return $ SAST.Constant typed_c (buildExpAnn pann (TConstSubtype expectedType))
-- | Integer literals without an expected type but with a known type.
typeExpression Nothing typeObj (Constant c@(I _ (Just ts)) pann) = do
  typed_c <- typeConstant pann typeObj c
  typedTS <- typeTypeSpecifier pann typeObj ts
  case typedTS of
    TConstSubtype ty -> do
      checkConstant pann ty typed_c
      return $ SAST.Constant typed_c (buildExpAnn pann (TConstSubtype ty))
    ty -> do
      checkConstant pann ty typed_c
      return $ SAST.Constant typed_c (buildExpAnn pann (TConstSubtype ty))
-- | Integer literals without an expected type and without a known type.
-- This is an error, since we cannot infer the type of the constant.
typeExpression Nothing _ (Constant (I tInt Nothing) pann) = do
  throwError $ annotateError pann $ EConstantWithoutKnownType (SAST.I tInt Nothing)
-- | Boolean literals without an expected type.
typeExpression Nothing typeObj (Constant c@(B {}) pann) = do
  typed_c <- typeConstant pann typeObj c
  return $ SAST.Constant typed_c (buildExpAnn pann (TConstSubtype TBool))
-- | Character literals without an expected type.
typeExpression Nothing typeObj (Constant c@(C {}) pann) = do
  typed_c <- typeConstant pann typeObj c
  return $ SAST.Constant typed_c (buildExpAnn pann (TConstSubtype TChar))
typeExpression Nothing typeObj (Constant c@Null pann) = do
  typed_c <- typeConstant pann typeObj c
  return $ SAST.Constant typed_c (buildExpAnn pann TUnit)
typeExpression expectedType typeObj (Casting e nts pann) = do
  nty <- typeTypeSpecifier pann typeObj nts
  maybe (return ()) (flip (sameTyOrError pann) nty) expectedType
  -- | Casting Expressions.
  typed_exp <- typeExpression Nothing typeObj e
  type_exp <- getExprType typed_exp
  if casteableTys type_exp nty
  then 
    case (nty, type_exp) of 
      (TConstSubtype _, TConstSubtype _) -> return (SAST.Casting typed_exp nty (buildExpAnn pann nty))
      (_, TConstSubtype _) -> return (SAST.Casting typed_exp nty (buildExpAnn pann (TConstSubtype nty)))
      (_, _) -> return (SAST.Casting typed_exp nty (buildExpAnn pann nty))
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
      case tyle_ty of
        TConstSubtype tyle_ty' -> do
          tyre <- catchMismatch pann (EBinOpTypeMismatch op tyle_ty') (typeExpression (Just tyle_ty') typeObj rnume)
          tyre_ty <- getExprType tyre
          case tyre_ty of
            TConstSubtype _ -> return (tyle_ty, tyle, tyre) 
            _ -> return (tyle_ty', tyle, tyre)
        _ -> do
          tyre <- catchMismatch pann (EBinOpTypeMismatch op tyle_ty) (typeExpression (Just tyle_ty) typeObj rnume)
          return (tyle_ty, tyle, tyre)

    -- | This function checks the that the lhs and the rhs are both
    -- equal to the expected type (if any) and that the expected type is
    -- a numeric type. This function is used to check the
    -- binary expressions multiplication, division, addition and subtraction.
    sameNumType :: SemanticMonad (SAST.Expression SemanticAnn)
    sameNumType =
      case expectedType of
        ty@(Just (TConstSubtype ty')) -> do
          unless (numTy ty') (throwError $ annotateError pann (EBinOpExpectedTypeNotNum op ty'))
          tyle <- catchMismatch (getAnnotation le) (EBinOpExpectedTypeLeft op ty')
            (typeExpression ty typeObj le)
          tyre <- catchMismatch (getAnnotation re) (EBinOpExpectedTypeRight op ty')
            (typeExpression ty typeObj re)
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype ty'))
        ty@(Just ty') -> do
          unless (numTy ty') (throwError $ annotateError pann (EBinOpExpectedTypeNotNum op ty'))
          tyle <- catchMismatch (getAnnotation le) (EBinOpExpectedTypeLeft op ty')
            (typeExpression ty typeObj le)
          tyre <- catchMismatch (getAnnotation re) (EBinOpExpectedTypeRight op ty')
            (typeExpression ty typeObj re)
          tyle_ty <- getExprType tyle
          tyre_ty <- getExprType tyre
          case (tyle_ty, tyre_ty) of
            (TConstSubtype _, TConstSubtype _) -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype ty'))
            (_, _) -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          (ty, tyle, tyre) <- sameTypeExpressions numTy (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty)

    -- | This function checks that the lhs is equal to the expected type (if any)
    -- and that that type is numeric. The rhs must be a positive (i.e. unsigned) type.
    -- This function is used to check the binary expressions bitwise left shift and
    -- bitwise right shift.
    leftNumRightPosType :: SemanticMonad (SAST.Expression SemanticAnn)
    leftNumRightPosType = do
      tyre <- typeExpression Nothing typeObj re
      tyre_ty <- getExprType tyre
      unless (posTy tyre_ty) (throwError $ annotateError pann (EBinOpRightTypeNotPos op tyre_ty))
      case expectedType of
        ty@(Just ty'@(TConstSubtype _)) -> do
          unless (numTy ty') (throwError $ annotateError pann (EBinOpExpectedTypeNotNum op ty'))
          tyle <- catchMismatch (getAnnotation le) (EBinOpExpectedTypeLeft op ty')
            (typeExpression ty typeObj le)
          case tyre_ty of
            TConstSubtype _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
            _ -> throwError $ annotateError (getAnnotation re) EExpressionNotConstant
        ty@(Just ty') -> do
          unless (numTy ty') (throwError $ annotateError pann (EBinOpExpectedTypeNotNum op ty'))
          tyle <- catchMismatch (getAnnotation le) (EBinOpExpectedTypeLeft op ty')
            (typeExpression ty typeObj le)
          tyle_ty <- getExprType tyle
          case (tyle_ty, tyre_ty) of
            (TConstSubtype _, TConstSubtype _) -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype ty'))
            (_, _) -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann ty')
        Nothing -> do
          tyle <- typeExpression Nothing typeObj le
          tyle_ty <- getExprType tyle
          unless (numTy tyle_ty) (throwError $ annotateError pann (EBinOpLeftTypeNotNum op tyle_ty))
          return $ SAST.BinOp op tyle tyre (buildExpAnn pann tyle_ty)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is equatable. This function is used to check the binary expressions == and !=.
    sameEquatableTyBool :: SemanticMonad (SAST.Expression SemanticAnn)
    sameEquatableTyBool =
      case expectedType of
        (Just TBool) -> do
          (ty, tyle, tyre) <-
            sameTypeExpressions eqTy
              (EBinOpLeftTypeNotEq op) (EBinOpRightTypeNotEq op) le re
          case ty of
            TConstSubtype _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype TBool))
            _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          (ty, tyle, tyre) <-
            sameTypeExpressions eqTy
              (EBinOpLeftTypeNotEq op) (EBinOpRightTypeNotEq op) le re
          case ty of
            TConstSubtype _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype TBool))
            _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)

    -- | This function checks that the lhs and the rhs are both of the same type and that the
    -- type is numeric. This function is used to check the binary expressions <, <=, > and >=.
    sameNumTyBool :: SemanticMonad (SAST.Expression SemanticAnn)
    sameNumTyBool =
      case expectedType of
        (Just TBool) -> do
          (ty, tyle, tyre) <-
            sameTypeExpressions numTy
              (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          case ty of 
            TConstSubtype _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype TBool))
            _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)
        Just ty -> throwError $ annotateError pann (EBinOpExpectedTypeNotBool op ty)
        Nothing -> do
          (ty, tyle, tyre) <-
            sameTypeExpressions numTy
              (EBinOpLeftTypeNotNum op) (EBinOpRightTypeNotNum op) le re
          case ty of
            TConstSubtype _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann (TConstSubtype TBool))
            _ -> return $ SAST.BinOp op tyle tyre (buildExpAnn pann TBool)

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
            Just (TReference ak (TArray ts rawSize)) -> do
              unless (sameTy ty ts) (throwError $ annotateError pann $ EMismatch ts ty)
              expectedSize <- evalArraySizeExpr rawSize
              return (SAST.ArraySliceExpression refKind typed_obj typed_lower typed_upper (buildExpAnn pann (TReference ak (TArray ts expectedSize))))
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
          maybe (return ()) (flip (sameTyOrError pann) (TReference refKind ty)) expectedType
          return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (TReference refKind ty)))
        _ -> do
          -- | Check if the we are allowed to create that kind of reference from the object
          checkReferenceAccessKind obj_ak
          maybe (return ()) (flip (sameTyOrError pann) (TReference refKind obj_type)) expectedType
          return (SAST.ReferenceExpression refKind typed_obj (buildExpAnn pann (TReference refKind obj_type)))

  where 

    evalArraySizeExpr :: SAST.Expression SemanticAnn -> SemanticMonad (SAST.Expression SemanticAnn)
    evalArraySizeExpr expr@(SAST.AccessObject (SAST.Variable ident' _)) = do
      glb <- ST.gets global
      case M.lookup ident' glb of
        Just (LocatedElement (GConstExpr _ty cexpr) _) -> do
          cExprType <- getExprType cexpr
          case cExprType of
            TConstSubtype TUSize -> return cexpr
            ty -> throwError $ annotateError Internal (EInvalidConstExprType ty)
        _ -> return expr
    evalArraySizeExpr expr = return expr

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
  typed_args <- localScope $ zipWithM (\(p, idx) e -> catchMismatch ann (EFunctionCallArgTypeMismatch (ident, p, funcLocation) idx)
      (
        case paramType p of 
          (TConstSubtype pty) -> do
            -- | We need to type the expression and ADD it to the global context as a const expression
            -- We have to do this because the const parameter may have been used to define an array size
            -- of another parameter. We may have to expand the size of the array if we are passing a
            -- slice of an array as parameter whose bounds are not known at compile time. We are using
            -- the global const substitution mechanism instread of implementing a new one for this
            -- case so that we can reuse the code.
            typedArgument <- typeExpression (Just (TConstSubtype pty)) typeRHSObject e
            ST.modify $ \s -> s { global = M.insert (paramIdentifier p) (LocatedElement (GConstExpr pty typedArgument) (getAnnotation e)) (global s) }
            return typedArgument
          ty -> typeExpression (Just ty) typeRHSObject e)) (zip ps [0 :: Integer ..]) args
  maybe (return ()) (flip (sameTyOrError ann) retty) expectedType
  return $ SAST.FunctionCall ident typed_args expAnn

----------------------------------------
typeExpression expectedType typeObj (MemberFunctionCall obj ident args ann) = do
  obj_typed <- typeObj obj
  (ak, obj_ty) <- getObjType obj_typed
  ((ps, typed_args), fty) <- typeMemberFunctionCall ann ak obj_ty ident args
  maybe (return ()) (flip (sameTyOrError ann) fty) expectedType
  return $ SAST.MemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)
typeExpression expectedType typeObj (DerefMemberFunctionCall obj ident args ann) = do
  obj_typed <- typeObj obj
  (_, obj_ty) <- getObjType obj_typed
  case obj_ty of
    TReference ak rTy -> do
      -- NOTE: We have reused the code from MemberFunctionCall, but we must take into
      -- account that, for the time being, when we are accessing a member function through
      -- a reference, the object (self) can only be of a user-defined class type. There
      -- cannot be references to ports. 
      ((ps, typed_args), fty) <- typeMemberFunctionCall ann ak rTy ident args
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
          maybe (return ()) (flip (sameTyOrError pann) TBool) expectedType
          return $ SAST.IsEnumVariantExpression obj_typed id_ty variant_id (buildExpAnn pann TBool)
        Nothing -> throwError $ annotateError pann (EEnumVariantNotFound lhs_enum variant_id)
    _ -> throwError $ annotateError Internal EExpectedEnumType
typeExpression expectedType typeObj (IsMonadicVariantExpression obj variant_id pann) = do
  case variant_id of
    NoneLabel -> typeOptionVariant
    SomeLabel -> typeOptionVariant
    SuccessLabel -> typeStatusVariant
    FailureLabel -> typeStatusVariant
    OkLabel -> typeResultVariant
    ErrorLabel -> typeResultVariant
  
  where 

    typeOptionVariant :: SemanticMonad (SAST.Expression SemanticAnn)
    typeOptionVariant = do
      obj_typed <- typeObj obj
      (_, obj_ty) <- getObjType obj_typed
      case obj_ty of
        (TOption {}) -> do
          maybe (return ()) (flip (sameTyOrError pann) TBool) expectedType
          return $ SAST.IsMonadicVariantExpression obj_typed variant_id (buildExpAnn pann TBool)
        _ -> throwError $ annotateError pann (EIsOptionVariantInvalidType obj_ty)

    typeStatusVariant :: SemanticMonad (SAST.Expression SemanticAnn)
    typeStatusVariant = do
      obj_typed <- typeObj obj
      (_, obj_ty) <- getObjType obj_typed
      case obj_ty of
        (TStatus {}) -> do
          maybe (return ()) (flip (sameTyOrError pann) TBool) expectedType
          return $ SAST.IsMonadicVariantExpression obj_typed variant_id (buildExpAnn pann TBool)
        _ -> throwError $ annotateError pann (EIsStatusVariantInvalidType obj_ty)

    typeResultVariant :: SemanticMonad (SAST.Expression SemanticAnn)
    typeResultVariant = do
      obj_typed <- typeObj obj
      (_, obj_ty) <- getObjType obj_typed
      case obj_ty of
        (TResult {}) -> do
          maybe (return ()) (flip (sameTyOrError pann) TBool) expectedType
          return $ SAST.IsMonadicVariantExpression obj_typed variant_id (buildExpAnn pann TBool)
        _ -> throwError $ annotateError pann (EIsResultVariantInvalidType obj_ty)

typeExpression _ _ (StructInitializer _ _ pann) = throwError $ annotateError pann EStructInitializerInvalidUse
typeExpression _ _ (ArrayInitializer _ _ pann) = throwError $ annotateError pann EArrayInitializerInvalidUse
typeExpression _ _ (ArrayExprListInitializer _ pann) = throwError $ annotateError pann EArrayExprListInitializerInvalidUse
typeExpression _ _ (MonadicVariantInitializer _ pann) = throwError $ annotateError pann EMonadicVariantInitializerInvalidUse
typeExpression _ _ (EnumVariantInitializer _ _ _ pann) = throwError $ annotateError pann EEnumVariantInitializerInvalidUse
typeExpression _ _ (StringInitializer _ pann) = throwError $ annotateError pann EStringInitializerInvalidUse

typeFieldAssignment
  :: (SAST.TerminaType SemanticAnn, Location)
  -> (Object ParserAnn -> SemanticMonad (SAST.Object SemanticAnn))
  -> SAST.FieldDefinition SemanticAnn
  -> FieldAssignment ParserAnn
  -> SemanticMonad (SAST.FieldAssignment SemanticAnn)
typeFieldAssignment tyDef typeObj (FieldDefinition fid fty _) (FieldValueAssignment faid faexp pann) =
  if fid == faid
  then
    flip (SAST.FieldValueAssignment faid) (buildExpAnn pann fty) <$> typeAssignmentExpression fty typeObj faexp
  else throwError $ annotateError pann (EFieldValueAssignmentUnknownFields tyDef [faid])
typeFieldAssignment tyDef _ (FieldDefinition fid fty _) (FieldAddressAssignment faid addr pann) =
  if fid == faid
  then
    case fty of
      TFixedLocation _ -> return $ SAST.FieldAddressAssignment faid addr (buildExpAnn pann fty)
      ty -> throwError $ annotateError pann (EFieldNotFixedLocation fid ty)
  else throwError $ annotateError pann (EFieldValueAssignmentUnknownFields tyDef [faid])
typeFieldAssignment tyDef _ (FieldDefinition fid fty _) (FieldPortConnection InboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry pann sid >>=
    \gentry ->
    case fty of
      TSinkPort ty action -> do
        rty <- case tyDef of
          (TGlobal _ clsId, _) -> getActionReturnType clsId action
          _ -> throwError $ annotateError Internal EExpectedClassType
        case gentry of
          LocatedElement  (GGlob ets@(TGlobal EmitterClass clsId)) _ -> do
            checkEmitterDataType pann clsId ty
            checkEmitterActionReturnType pann clsId action rty
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildSinkPortConnAnn pann ets action)
          _ -> throwError $ annotateError pann $ ESinkPortConnectionInvalidGlobal sid
      TInPort ty action  -> do
        rty <- case tyDef of
          (TGlobal _ clsId, _) -> getActionReturnType clsId action
          _ -> throwError $ annotateError Internal EExpectedClassType
        case gentry of
          LocatedElement (GGlob cts@(TMsgQueue ty' _)) _ -> do
            catchMismatch pann (EInboundPortConnectionMsgQueueTypeMismatch sid ty) (sameTyOrError pann ty ty')
            unless (sameTy rty (TStatus TInt32)) (throwError $ annotateError pann (EInvalidMsgQueueActionReturnType action rty))
            return $ SAST.FieldPortConnection InboundPortConnection pid sid (buildInPortConnAnn pann cts action)
          _ -> throwError $ annotateError pann $ EInboundPortConnectionInvalidObject sid
      ty -> throwError $ annotateError pann (EFieldNotSinkOrInboundPort fid ty)
  else throwError $ annotateError pann (EFieldValueAssignmentUnknownFields tyDef [pid])

  where

    getActionReturnType :: Identifier -> Identifier -> SemanticMonad (SAST.TerminaType SemanticAnn)
    getActionReturnType clsId action = do
      getGlobalTypeDef pann clsId >>=
        \case
          LocatedElement (Class _ _ members _ _) _ -> 
            case findClassAction action members of
              Just (_, rty, _) -> return rty
              Nothing -> throwError $ annotateError Internal EExpectedClassType
          _ -> throwError $ annotateError Internal EExpectedClassType

typeFieldAssignment tyDef _ (FieldDefinition fid fty _) (FieldPortConnection OutboundPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry pann sid >>=
    \gentry ->
    case fty of
      TOutPort ty ->
        case gentry of
          LocatedElement (GGlob cts@(TMsgQueue ty' _)) _ -> do
            catchMismatch pann (EOutboundPortConnectionMsgQueueTypeMismatch sid ty) (sameTyOrError pann ty ty')
            return $ SAST.FieldPortConnection OutboundPortConnection pid sid (buildOutPortConnAnn pann cts)
          _ -> throwError $ annotateError pann $ EOutboundPortConnectionInvalidGlobal sid
      ty -> throwError $ annotateError pann (EFieldNotOutboundPort fid ty)
  else throwError $ annotateError pann (EFieldValueAssignmentUnknownFields tyDef [pid])
typeFieldAssignment tyDef _ (FieldDefinition fid fty fann) (FieldPortConnection AccessPortConnection pid sid pann) =
  if fid == pid
  then
    getGlobalEntry pann sid >>=
    \gentry ->
    case fty of
      TAccessPort (TAllocator ty) ->
        case gentry of
          LocatedElement (GGlob (TPool ty' s)) _ -> do
            catchMismatch pann (EAllocatorPortConnectionPoolTypeMismatch sid ty) (sameTyOrError pann ty ty')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildPoolConnAnn pann ty s)
          _ -> throwError $ annotateError pann $ EAllocatorPortConnectionInvalidGlobal sid
      TAccessPort (TAtomicAccess ty) ->
        case gentry of
          LocatedElement (GGlob (TAtomic ty')) _ -> do
            catchMismatch pann (EAtomicConnectionTypeMismatch ty) (sameTyOrError pann ty ty')
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicConnAnn pann ty)
          _ -> throwError $ annotateError pann $ EAtomicAccessPortConnectionInvalidGlobal sid
      TAccessPort (TAtomicArrayAccess ty portSize) ->
        case gentry of
          LocatedElement (GGlob (TAtomicArray ty' glbSize)) _ -> do
            catchMismatch pann (EAtomicArrayConnectionTypeMismatch ty) (sameTyOrError pann ty ty')
            -- We will check the size of the atomic array when we are performing the const propagation.
            return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAtomicArrayConnAnn pann ty portSize glbSize)
          _ -> throwError $ annotateError pann $ EAtomicArrayAccessPortConnectionInvalidGlobal sid
      TAccessPort ifaceTy@(TInterface _ iface) -> do
        -- | Get the interface definition
        getGlobalTypeDef pann iface >>=
          \case {
            LocatedElement (Interface _ _ extends members _) _ -> (do
              -- Collect the procedures of the interface
              extendedMembers <- concat <$> traverse (fmap M.elems . collectInterfaceProcedures pann) extends
              let procs = [ProcedureSeman procid params ms | (InterfaceProcedure _ak procid params ms _) <- members ++ extendedMembers]
              -- Check that the resource provides the interface
              case gentry of
                LocatedElement (GGlob rts@(TGlobal ResourceClass clsId)) _ ->
                  getGlobalTypeDef pann clsId >>=
                  \case {
                      LocatedElement (Class _ _ _ provides _) _ -> (do
                        -- Collect the interfaces provided by the resource and their extended interfaces
                        extendedProvides <- concat <$> traverse (collectExtendedInterfaces pann) provides;
                        case Data.List.find (iface ==) (provides ++ extendedProvides) of
                          Just _ ->
                            case fann of
                              (SemanticAnn (FTy (AccessPortField usedProcs)) _) ->
                                let filteredProcs = filter (\(ProcedureSeman procid _ _) -> M.member procid usedProcs) procs in
                                return $ SAST.FieldPortConnection AccessPortConnection pid sid (buildAccessPortConnAnn pann ifaceTy rts filteredProcs)
                              _ -> error "Invalid annotation for field"
                          _ -> throwError $ annotateError pann $ EAccessPortConnectionInterfaceNotProvided sid iface
                      );
                      _ -> throwError $ annotateError Internal EExpectedInterfaceType
                  }
                _ -> throwError $ annotateError pann $ EAccessPortConnectionInvalidGlobal sid
            );
            _ -> throwError $ annotateError Internal EExpectedInterfaceType
          }
      ty -> throwError $ annotateError pann (EFieldNotAccessPort fid ty)
  else throwError $ annotateError pann (EFieldValueAssignmentUnknownFields tyDef [pid])

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
        typeFieldAssignment tyDef typeObj d a >>= checkSortedFields ds as . (:acc)
      else
        throwError $ annotateError faLoc (EFieldValueAssignmentMissingFields tyDef [fieldIdentifier d])
