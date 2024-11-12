module Semantic.TypeChecking.Check where
import Utils.Annotations
import Parser.AST as PAST
import qualified Semantic.AST as SAST
import Semantic.Monad
import Control.Monad
import Control.Monad.Except
import Semantic.Errors.Errors
import Core.Utils
import qualified Core.AST as CAST
import qualified Data.List
import Semantic.Types
import Parser.Types

checkConstant :: Location -> TerminaType -> SAST.Const -> SemanticMonad ()
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

checkIntConstant :: Location -> TerminaType -> TInteger -> SemanticMonad ()
checkIntConstant loc tyI ti@(TInteger i _) =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I ti (Just tyI)))

checkSize :: Location -> Size -> SemanticMonad ()
checkSize loc (CAST.K s) = checkIntConstant loc TUSize s
checkSize loc (CAST.V ident) = getConst loc ident >>= (\(ty, _) -> sameTyOrError loc TUSize ty) >> return ()

-- | Function checking that a TerminaType is well-defined.
-- We are assuming that this function is always called AFTER the type was
-- correctly obtained from the type specifier using the function
-- |typeTypeSpecifier|.  Thus, we do not need to check if the type of structs,
-- enums, etc. are previously defined.  Note that we do not change the
-- |TerminaType| in any way, that's why this function return |()|.
checkTerminaType :: Location -> TerminaType -> SemanticMonad ()
checkTerminaType loc (TArray ty s) =
  checkTerminaType loc ty >>
  arrayTyOrFail loc ty >>
  checkSize loc s
checkTerminaType loc (TMsgQueue ty s) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty >>
  checkSize loc s
checkTerminaType loc (TPool ty s) = checkTerminaType loc ty >> checkSize loc s
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
  checkSize loc s
checkTerminaType loc (TAtomic ty) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomicArray ty s) =
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicArrayInvalidType (numTyOrFail loc ty) >>
  checkSize loc s
-- The rest of the types are always well defined, since they were
-- constructed using the |typeTypeSpecifier| function.
checkTerminaType _ _ = return ()

checkParameterType :: Location -> SAST.Parameter -> SemanticMonad ()
checkParameterType loc p =
    let typeSpec = paramType p in
    unless (parameterTy typeSpec) (throwError (annotateError loc (EInvalidParameterType p))) >>
    checkTerminaType loc typeSpec

checkProcedureParameterType :: Location -> SAST.Parameter -> SemanticMonad ()
checkProcedureParameterType loc p =
    let typeSpec = paramType p in
    unless (procedureParamTy typeSpec) (throwError (annotateError loc (EInvalidProcedureParameterType typeSpec))) >>
    checkTerminaType loc typeSpec

checkActionParameterType :: Location -> SAST.Parameter -> SemanticMonad ()
checkActionParameterType loc p =
    let typeSpec = paramType p in
    unless (actionParamTy typeSpec) (throwError (annotateError loc (EInvalidActionParameterType typeSpec))) >>
    checkTerminaType loc typeSpec
    
checkReturnType :: Location -> TerminaType -> SemanticMonad ()
checkReturnType anns ty =
  checkTerminaType anns ty >>
  unless (copyTy ty) (throwError (annotateError anns (EInvalidReturnType ty)))

checkUniqueNames :: Location -> ([Identifier] -> Error) -> [Identifier] -> SemanticMonad ()
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

-- | This function type checks the members of a class depending on its kind.
checkClassKind :: Location -> Identifier -> ClassKind
  -> ([SAST.ClassMember SemanticAnn],
      [ClassMember ParserAnn],
      [ClassMember ParserAnn])
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
    _ -> throwError (annotateError Internal EMalformedClassTyping)
  -- Check that the resource class does not define any in and out ports
  mapM_ (
    \case {
      ClassField (FieldDefinition fs_id fs_ty) annCF ->
        case fs_ty of
          TInPort _ _ -> throwError $ annotateError (location annCF) (EResourceClassInPort (clsId, anns) fs_id)
          TOutPort _ -> throwError $ annotateError (location annCF) (EResourceClassOutPort (clsId, anns) fs_id)
          _ -> return ()
      ;
      _ -> return ();
    }) fs
  -- Check that all the procedures are provided
  providedProcedures <- concat <$> foldM (\acc ifaceId ->
    catchError (getGlobalTypeDef anns ifaceId)
      (\_ -> throwError $ annotateError anns (EInterfaceNotFound ifaceId)) >>= \case {
      (LocatedElement (Interface _ iface_prcs _) _) -> return $ map (, ifaceId) iface_prcs : acc;
      _ -> throwError $ annotateError anns (EGlobalNotInterface ifaceId)
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

    checkSortedProcedures :: [(SAST.InterfaceMember SemanticAnn, Identifier)] -> [ClassMember ParserAnn] -> SemanticMonad ()
    checkSortedProcedures [] [] = return ()
    checkSortedProcedures [] ((ClassProcedure prcId _ _ ann):_) = throwError $ annotateError ann (EProcedureNotFromProvidedInterfaces (clsId, anns) prcId)
    checkSortedProcedures ((InterfaceProcedure procId _ _, ifaceId) : _) [] = throwError $ annotateError anns (EMissingProcedure ifaceId procId)
    checkSortedProcedures ((InterfaceProcedure prcId ps pann, ifaceId) : ds) ((ClassProcedure prcId' ps' _ ann):as) =
      unless (prcId == prcId') (throwError $ annotateError ann (EProcedureNotFromProvidedInterfaces (clsId, anns) prcId')) >> do
      let psLen = length ps
          psLen' = length ps'
      when (psLen < psLen') (throwError $ annotateError ann (EProcedureExtraParams (ifaceId, prcId, map paramType ps, location pann) (fromIntegral psLen')))
      when (psLen > psLen') (throwError $ annotateError ann (EProcedureMissingParams (ifaceId, prcId, map paramType ps, location pann) (fromIntegral psLen')))
      zipWithM_ (\p@(Parameter _ ty) (Parameter _ ts) -> do
        ty' <- typeTypeSpecifier (location pann) ts
        unless (sameTy ty ty') (throwError $ annotateError ann (EProcedureParamTypeMismatch (ifaceId, prcId, paramType p, location pann) ty'))) ps ps'
      checkSortedProcedures ds as
    checkSortedProcedures _ _ = throwError (annotateError Internal EMalformedClassTyping)

checkClassKind anns clsId TaskClass (_fs, prcs, acts) provides = do
  -- A task must not provide any interface
  unless (null provides) (throwError $ annotateError anns (ETaskClassProvides clsId))
  -- A task must not implement any procedures
  case prcs of
    [] -> return ()
    (ClassProcedure procId _ _ ann):_  ->
        throwError $ annotateError ann (ETaskClassProcedure (clsId, anns) procId)
    _ -> throwError (annotateError Internal EMalformedClassTyping)
  -- A task must implement at least one action
  when (null acts) (throwError $ annotateError anns (ETaskClassNoActions clsId))
checkClassKind anns clsId HandlerClass (fs, prcs, acts) provides = do
  -- A handler must not provide any interface
  unless (null provides) (throwError $ annotateError anns (EHandlerClassProvides clsId))
  -- A handler must not implement any procedures
  case prcs of
    [] -> return ()
    (ClassProcedure procId _ _ ann):_  ->
        throwError $ annotateError ann (EHandlerClassProcedure (clsId, anns) procId)
    _ -> throwError (annotateError Internal EMalformedClassTyping)
  -- A handler must implement only one action
  case acts of
    [] -> throwError $ annotateError anns (EHandlerClassNoAction clsId)
    [ClassAction _actionId _ _ _ _ann] -> return ()
    ClassAction _ _ _ _ prevActAnn : ClassAction _ _ _ _ otherActAnn : _  ->
        throwError $ annotateError otherActAnn (EHandlerClassMultipleActions clsId prevActAnn)
    _ -> throwError (annotateError Internal EMalformedClassTyping)
  -- A handler must have one single sink port and cannot define any in ports
  checkHandlerPorts Nothing fs

  where

    checkHandlerPorts :: Maybe Location -> [SAST.ClassMember SemanticAnn] -> SemanticMonad ()
    checkHandlerPorts Nothing [] = throwError $ annotateError anns (EHandlerClassNoSinkPort clsId)
    checkHandlerPorts (Just _) [] = return ()
    checkHandlerPorts prev ((ClassField (FieldDefinition fs_id fs_ty) annCF): xfs) =
      case fs_ty of
        TSinkPort _ _ ->
          case prev of
            Nothing -> checkHandlerPorts (Just (location annCF)) xfs
            Just prevPort -> throwError $ annotateError (location annCF) (EHandlerClassMultipleSinkPorts clsId prevPort)
        TInPort _ _ -> throwError $ annotateError (location annCF) (EHandlerClassInPort (clsId, anns) fs_id)
        _ -> checkHandlerPorts prev xfs
    checkHandlerPorts _ _ = throwError $ annotateError Internal EMalformedClassTyping

checkClassKind _anns _clsId _kind _members _provides = return ()

checkEmitterDataType :: Location -> Identifier -> TerminaType -> SemanticMonad ()
checkEmitterDataType loc "Interrupt" ty =
  unless (sameTy ty TUInt32) (throwError $ annotateError loc (EInvalidInterruptEmitterType ty))
checkEmitterDataType loc "PeriodicTimer" ty =
  unless (sameTy ty (TStruct "TimeVal")) (throwError $ annotateError loc (EInvalidPeriodicTimerEmitterType ty))
checkEmitterDataType loc "SystemInit" ty =
  unless (sameTy ty (TStruct "TimeVal")) (throwError $ annotateError loc (EInvalidSystemInitEmitterType ty))
checkEmitterDataType _ _ _ = throwError $ annotateError Internal EUnboxingEmittterClass
