module Semantic.TypeChecking.TypeDefinition where

-- Termina Ast and Utils
import Utils.Annotations
import Parser.AST as PAST
import Core.Utils
import Semantic.Utils

-- Top Sort
import qualified Data.Map as M

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

import Extras.TopSort

----------------------------------------
-- Libaries and stuff

import qualified Data.List  (map)
import Data.Maybe

import Parser.Types
import Utils.Monad
import Semantic.TypeChecking.Statement
import Semantic.TypeChecking.Check
import Semantic.TypeChecking.Expression
import qualified Data.Set as S

typeProcedureParameter :: Location -> Parameter -> SemanticMonad SAST.Parameter
typeProcedureParameter loc (Parameter ident ts) = do
  ty <- typeTypeSpecifier loc ts
  let procParam = SAST.Parameter ident ty
  checkProcedureParameterType loc procParam
  return procParam

typeParameter :: Location -> Parameter -> SemanticMonad SAST.Parameter
typeParameter loc (Parameter ident ts) = do
  ty <- typeTypeSpecifier loc ts
  let param = SAST.Parameter ident ty
  checkParameterType loc param
  return param

typeActionParameter :: Location -> Parameter -> SemanticMonad SAST.Parameter
typeActionParameter loc (Parameter ident ts) = do
  ty <- typeTypeSpecifier loc ts
  let param = SAST.Parameter ident ty
  checkActionParameterType loc param
  return param

typeClassFieldDefinition ::
  S.Set Identifier
  -> ClassMember ParserAnn
  -> SemanticMonad (SAST.ClassMember SemanticAnn)
typeClassFieldDefinition fldDependencies (ClassField (FieldDefinition fs_id fs_ts annCF)) = do
  fs_ty <- typeTypeSpecifier annCF fs_ts
  checkTerminaType annCF fs_ty
  classFieldTyorFail annCF fs_ty
  let fieldDef = SAST.FieldDefinition fs_id fs_ty
  case fs_ty of
    TAccessPort (TInterface _ iface) -> do
      procedures <- collectInterfaceProcedures annCF iface
      let usedProcedures = M.filterWithKey (\k _ -> S.member k fldDependencies) procedures
      return $ SAST.ClassField (fieldDef (buildAccessPortFieldAnn annCF usedProcedures))
    _ -> return $ SAST.ClassField (fieldDef (buildFieldAnn annCF))
typeClassFieldDefinition _ _ = error "Internal Error: not a field"

-- Type definition
-- Here I am traversing lists serveral times, I prefer to be clear than
-- proficient for the time being.
typeTypeDefinition :: Location -> TypeDef ParserAnn -> SemanticMonad (SAST.TypeDef SemanticAnn)
-- Check Type definitions
typeTypeDefinition ann (Struct ident fs_ts mds_ts) = do
  -- Check every type is well-defined:
  -- Check that the struct is not empty
  when (Prelude.null fs_ts) (throwError $ annotateError ann (EStructDefEmpty ident))
  -- Check that every field is well-defined
  fs_ty <- mapM typeFieldDefinition fs_ts
  -- Check field names are unique
  checkUniqueNames ann EStructDefNotUniqueField (Data.List.map fieldIdentifier fs_ty)
  -- Type the modifiers
  mds_ty <- mapM (typeModifier ann) mds_ts
  -- If everything is fine, return same struct
  return (SAST.Struct ident fs_ty mds_ty)
typeTypeDefinition ann (Enum ident evs_ts mds_ts) = do
  -- Check that the enum is not empty
  when (Prelude.null evs_ts) (throwError $ annotateError ann (EEnumDefEmpty ident))
  -- Check the enum variants are well-defined
  evs_ty <- mapM (typeEnumVariant ann) evs_ts
  -- Check names are unique
  checkUniqueNames ann EEnumDefNotUniqueVariant (Data.List.map variantIdentifier evs_ty)
  -- Type the modifiers
  mds_ty <- mapM (typeModifier ann) mds_ts
  -- If everything is fine, return the same definition.
  return (Enum ident evs_ty mds_ty)
typeTypeDefinition ann (Interface RegularInterface ident extends members mds_ts) = do
  -- Check that the interface is not empty
  when (null members) (throwError $ annotateError ann (EInterfaceEmpty ident))
  -- Check that there are no repeated extended interfaces
  checkNoDuplicatedExtendedInterfaces extends
  -- Check procedure names are unique
  checkUniqueNames ann
    EInterfaceNotUniqueProcedure
      (Data.List.map (\case InterfaceProcedure ifaceId _ _ _ -> ifaceId) members)
  -- Check that every procedure is well-defined
  procedures <- mapM typeInterfaceProcedure members
  -- Check that the name of the procedures is not repeated in the extended interfaces
  checkNoDuplicatedExtendedProcedures extends
  -- Type the modifiers
  mds_ty <- mapM (typeModifier ann) mds_ts
  -- If everything is fine, return the same definition.
  return (Interface RegularInterface ident extends procedures mds_ty)

  where

    typeInterfaceProcedure :: InterfaceMember Location -> SemanticMonad (SAST.InterfaceMember SemanticAnn)
    typeInterfaceProcedure (InterfaceProcedure procId ps_ts mds_ts' annIP) = do
      ps_ty <- mapM (typeProcedureParameter annIP) ps_ts
      mds_ty' <- mapM (typeModifier ann) mds_ts'
      return $ InterfaceProcedure procId ps_ty mds_ty' (buildExpAnn annIP TUnit)

    -- | Checks that the procedures incorporated from the extended interfaces
    -- are not duplicated
    checkNoDuplicatedExtendedProcedures ::
      [Identifier] -- Accumulator
      -> SemanticMonad ()
    checkNoDuplicatedExtendedProcedures [] = return ()
    checkNoDuplicatedExtendedProcedures ifaces = do
      -- | Collect the procedures of the extended interfaces. This function
      -- returns a map from the extended interfaces to their procedures.
      -- The function has already checked that there are no duplicated
      -- procedures in the extended interfaces.
      prevProcedures <- checkNoDuplicatedExtendedProcedures' M.empty ifaces
      -- | Now we have to check that the procedures of the current interface
      -- do not collide with the procedures of the extended interfaces.
      forM_ members $ \(InterfaceProcedure procId _ _ procAnn) -> do
        forM_ (M.keys prevProcedures) $ \prevIface -> do
          let prevProcedueresMap = prevProcedures M.! prevIface
          case M.lookup procId prevProcedueresMap of
            Nothing -> return ()
            Just _ -> throwError $ annotateError procAnn (EInterfaceProcedurePreviouslyExtended procId prevIface)

      where

        checkNoDuplicatedExtendedProcedures' ::
          M.Map Identifier (M.Map Identifier (SAST.InterfaceMember SemanticAnn))
          -> [Identifier]
          -> SemanticMonad (M.Map Identifier (M.Map Identifier (SAST.InterfaceMember SemanticAnn)))
        checkNoDuplicatedExtendedProcedures' acc [] = return acc
        checkNoDuplicatedExtendedProcedures' acc (p:ps) = do
          -- Recursively check the rest of the interfaces
          accxs <- checkNoDuplicatedExtendedProcedures' acc ps
          -- Collect the procedures of the current extended interface, including
          -- the procedures of the extended interfaces of the current interface.
          extendedProcsMap <- collectInterfaceProcedures ann p
          forM_ (M.keys extendedProcsMap) $ \extendedProc -> do
            forM_ (M.keys accxs) $ \prevIface -> do
              let prevProcsMap = accxs M.! prevIface
              case M.lookup extendedProc prevProcsMap of
                Nothing -> return ()
                Just _ -> throwError $ annotateError ann (EInterfaceDuplicatedExtendedProcedure p prevIface extendedProc)
          return $ M.insert p extendedProcsMap accxs

    checkNoDuplicatedExtendedInterfaces ::
      [Identifier] -- List of interfaces to check
      -> SemanticMonad ()
    checkNoDuplicatedExtendedInterfaces [] = return ()
    checkNoDuplicatedExtendedInterfaces ifaces =
      void $ checkNoDuplicatedExtendedInterfaces' M.empty ifaces

      where

        checkNoDuplicatedExtendedInterfaces' ::
          M.Map Identifier (Maybe Identifier)
          -> [Identifier] -- List of interfaces to check
          -> SemanticMonad (M.Map Identifier (Maybe Identifier))
        checkNoDuplicatedExtendedInterfaces' acc [] = return acc
        checkNoDuplicatedExtendedInterfaces' acc (x:xs) = do
          accxs <- checkNoDuplicatedExtendedInterfaces' acc xs
          case M.lookup x accxs of
            Just (Just prevIface) -> throwError $ annotateError ann (EInterfacePreviouslyExtended x prevIface)
            Just Nothing -> throwError $ annotateError ann (EInterfaceDuplicatedExtendedIface x)
            Nothing -> do
              extendedIfaces <- collectExtendedInterfaces ann x
              foldM (\acc' extendedIface -> do
                case M.lookup extendedIface acc' of
                  Just (Just prevIface) -> throwError $ annotateError ann (EInterfacePreviouslyExtended extendedIface prevIface)
                  Just Nothing -> throwError $ annotateError ann (EInterfacePreviouslyExtended x extendedIface)
                  Nothing -> return $ M.insert extendedIface (Just x) acc') (M.insert x Nothing accxs) extendedIfaces

typeTypeDefinition _ (Interface SystemInterface ident _ _ _ ) = throwError $ annotateError Internal (ESystemInterfaceDefinition ident)

typeTypeDefinition ann (Class kind ident members provides mds_ts) =
  foldM
    (\(fs, prcs, mths, vws, acts) cl ->
        case cl of
          -- ClassFields
          fld@(ClassField {})  ->
              return (fld : fs, prcs, mths, vws, acts) {-- do
              fs_ty <- typeTypeSpecifier annCF fs_ts
              checkTerminaType annCF fs_ty
              classFieldTyorFail annCF fs_ty
              let fieldDef = SAST.FieldDefinition fs_id fs_ty
              let checkFs = SAST.ClassField fieldDef (buildExpAnn annCF fs_ty)
              return (checkFs : fs, prcs, mths, vws, acts) --}
          -- Procedures
          prc@(ClassProcedure {}) ->
              return (fs, prc : prcs, mths, vws, acts)
          -- Methods
          mth@(ClassMethod {}) ->
              return (fs, prcs, mth : mths, vws, acts)
          -- Viewers
          view@(ClassViewer {}) ->
              return (fs, prcs, mths, view : vws, acts)
          action@(ClassAction {}) ->
              return (fs, prcs, mths, vws , action : acts)
        )
    ([],[],[],[],[]) members
  >>= \(fls   -- Fields
       , prcs -- Procedures.
       , mths -- Methods
       , vws  -- Viewers
       , acts -- Actions
       -- introduce a semi-well formed type.
       ) ->
  do
  -- Now we can go function by function checking everything is well typed.
  ----------------------------------------
  -- Loop between methods, procedures and viewers.
  -- Assumption: dependencies are computes through `method g () {... self->f()
  -- ...}`, then `g > f`.
    let elements = prcs ++ mths ++ vws ++ acts
    -- Dependencies emplying the assumption.
    let dependenciesMap =
          foldr selfDepClass M.empty elements
    let dependencies = fmap (
          M.foldrWithKey (\k v acc -> SelfDep k (getAnnotation v) : acc)
          []) dependenciesMap
    let fldDependenciesMap = foldr fieldDepClass M.empty elements

    ty_fls <- mapM (
      \case { 
        fld@(ClassField (FieldDefinition fid _ _)) -> 
          (case M.lookup fid fldDependenciesMap of
            Just s -> typeClassFieldDefinition s fld
            Nothing -> typeClassFieldDefinition S.empty fld);
        _ -> error "Internal Error: not a field";
      }) fls
    checkClassKind ann ident kind (ty_fls, prcs, acts) provides

    -- Map from ClassNames to their definition (usefull after sorting by name and dep)
    let nameClassMap = M.fromList (map (\e -> (className e, e)) elements)
    mds_ty <- mapM (typeModifier ann) mds_ts
    -- Sort and see if there is a loop
    topSortOrder <- case topSort dependencies of
            -- Tell the user a loop is in the room
            Left (ELoop loop) -> throwError (annotateError ann (EClassLoop ((\(SelfDep member loc) -> (member, loc)) <$> loop)))
            Left (ENotFound (SelfDep dep _) parentId) -> do
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
                  (Data.List.map kClassMember (ty_fls ++ prevMembers))
                  provides mds_ty in
        localScope $ do
          insertGlobalTy ann clsType
          -- Now analyze new member.
          case newMember of
            -- Filtered Cases
            ClassField {} -> throwError (annotateError Internal EMalformedClassTyping)
            -- Interesting case
            ClassProcedure mIdent ps_ts blk mann -> do
              -- We have checked the validity of the parameters when sorting the class members.
              ps_ty <- mapM (typeProcedureParameter mann) ps_ts
              typed_bret <- addLocalImmutObjs mann (("self", TReference Mutable (TGlobal kind ident)) : fmap (\p -> (paramIdentifier p, paramType p)) ps_ty) (typeBlock Nothing blk)
              let newPrc = SAST.ClassProcedure mIdent ps_ty typed_bret (buildExpAnn mann TUnit)
              return (newPrc : prevMembers)
            ClassMethod mIdent mts mbody mann -> do
              mty <- maybe (return Nothing) (typeTypeSpecifier mann >=>
                  (\ty -> checkReturnType mann ty >> return (Just ty))) mts
              typed_bret <- addLocalImmutObjs mann [("self", TReference Private (TGlobal kind ident))] (typeBlock mty mbody)
              let newMth = SAST.ClassMethod mIdent mty  typed_bret (buildExpAnn mann (fromMaybe TUnit mty))
              return (newMth : prevMembers)
            ClassViewer mIdent ps_ts mts mbody mann -> do
              ps_ty <- mapM (typeParameter mann) ps_ts
              mty <- maybe (return Nothing) (typeTypeSpecifier mann >=>
                  (\ty -> checkReturnType mann ty >> return (Just ty))) mts
              typed_bret <- addLocalImmutObjs mann (("self", TReference Immutable (TGlobal kind ident)) : fmap (\p -> (paramIdentifier p, paramType p)) ps_ty) (typeBlock mty mbody)
              let newVw = SAST.ClassViewer mIdent ps_ty mty typed_bret (buildExpAnn mann (fromMaybe TUnit mty))
              return (newVw : prevMembers)
            ClassAction mIdent p_ts ts mbody mann -> do
              p_ty <- typeActionParameter mann p_ts
              ty <- typeTypeSpecifier mann ts
              checkReturnType mann ty
              typed_bret <- addLocalImmutObjs mann (("self", TReference Private (TGlobal kind ident)) : [(paramIdentifier p_ty, paramType p_ty)]) (typeBlock (Just ty) mbody)
              let newAct = SAST.ClassAction mIdent p_ty ty typed_bret (buildExpAnn mann ty)
              return (newAct : prevMembers)
        ) [] topSortOrder
    -- | Return the class with the methods, procedures, viewers and actions
    -- checked. The methods, procedures, viewers and actions are reverse-sorted
    -- to math the order of the topological sort (they are inserted in reverse
    -- order).
    return (SAST.Class kind ident (ty_fls ++ reverse fnChecked) provides mds_ty)

----------------------------------------
-- Field definition helpers.
typeFieldDefinition :: FieldDefinition ParserAnn -> SemanticMonad (SAST.FieldDefinition SemanticAnn)
typeFieldDefinition (FieldDefinition ident ts loc) = do
  -- First we check its type is well-defined
  ty <- typeTypeSpecifier loc ts
  checkTerminaType loc ty
  -- and that it is simply (see simple types).
  structFieldTyOrFail loc ty
  return $ SAST.FieldDefinition ident ty (buildFieldAnn loc)

-- Enum Variant definition helpers.
typeEnumVariant :: Location -> EnumVariant -> SemanticMonad SAST.EnumVariant
typeEnumVariant loc (EnumVariant ident ps_ts) = do
  ptys <- mapM (
    \ts -> do
        ty <- typeTypeSpecifier loc ts
        enumParamTyOrFail loc ty
        return ty) ps_ts
  return $ SAST.EnumVariant ident ptys
