module Semantic.TypeChecking.Check where
import Utils.Annotations
import Parser.AST as PAST
import qualified Semantic.AST as SAST
import Semantic.Monad
import Control.Monad
import Control.Monad.Except
import Semantic.Errors
import Core.Utils
import qualified Data.List as L
import Semantic.Types
import qualified Data.Set as S
import qualified Data.Map as M


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
checkTerminaType :: Location -> SAST.TerminaType SemanticAnn -> SemanticMonad ()
checkTerminaType loc (TArray ty _s) =
  checkTerminaType loc ty >>
  arrayTyOrFail loc ty
  -- TODO: check size constant 
  -- checkSize loc s
checkTerminaType loc (TMsgQueue ty _s) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty 
  -- TODO: check size constant 
  -- checkSize loc s
checkTerminaType loc (TPool ty _s) = checkTerminaType loc ty
  -- TODO: check size constant 
  -- >> checkSize loc s
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
  catchExpectedNum loc EAtomicArrayAccessInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomic ty) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomicArray ty s) =
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicArrayInvalidType (numTyOrFail loc ty)
-- TODO: check size constant 
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
