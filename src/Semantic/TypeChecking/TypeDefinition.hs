module Semantic.TypeChecking.TypeDefinition where

-- Termina Ast and Utils
import Utils.Annotations
import Parser.AST as PAST
import Core.Utils
import Semantic.Utils

-- Top Sort
import qualified Data.Map.Strict as M

-- Termina Semantic AST
import qualified Semantic.AST as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
import Control.Monad
import Control.Monad.Except
import Semantic.Errors.Errors
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
import Semantic.TypeChecking.Expression (typeModifier)

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
  fs_ty <- mapM (typeFieldDefinition ann) fs_ts
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
typeTypeDefinition ann (Interface ident cls mds_ts) = do
  -- Check that the interface is not empty
  when (null cls) (throwError $ annotateError ann (EInterfaceEmpty ident))
  -- Check procedure names are unique
  checkUniqueNames ann EInterfaceNotUniqueProcedure (Data.List.map (\case InterfaceProcedure ifaceId _ _ -> ifaceId) cls)
  -- Check that every procedure is well-defined
  procedures <- mapM typeInterfaceProcedure cls
  -- Type the modifiers
  mds_ty <- mapM (typeModifier ann) mds_ts
  -- If everything is fine, return the same definition.
  return (Interface ident procedures mds_ty)

  where

    typeInterfaceProcedure :: InterfaceMember Location -> SemanticMonad (SAST.InterfaceMember SemanticAnn)
    typeInterfaceProcedure (InterfaceProcedure procId ps_ts annIP) = do
      ps_ty <- mapM (typeProcedureParameter annIP) ps_ts
      return $ InterfaceProcedure procId ps_ty (buildExpAnn annIP TUnit)

typeTypeDefinition ann (Class kind ident members provides mds_ts) =
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Classes
  -- check that it defines at least one method.
  -- TODO: Check class well-formedness depending on its kind
  -- TODO: Check the class procedures belong to a provided interface
  foldM
    (\(fs, prcs, mths, vws, acts) cl ->
        case cl of
          -- ClassFields
          ClassField (FieldDefinition fs_id fs_ts) annCF -> do
              fs_ty <- typeTypeSpecifier annCF fs_ts
              checkTerminaType annCF fs_ty
              classFieldTyorFail annCF fs_ty
              let fieldDef = SAST.FieldDefinition fs_id fs_ty
              let checkFs = SAST.ClassField fieldDef (buildExpAnn annCF fs_ty)
              return (checkFs : fs, prcs, mths, vws, acts)
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
    mds_ty <- mapM (typeModifier ann) mds_ts
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
    return (SAST.Class kind ident (fls ++ reverse fnChecked) provides mds_ty)

----------------------------------------
-- Field definition helpers.
typeFieldDefinition :: Location -> FieldDefinition -> SemanticMonad SAST.FieldDefinition
typeFieldDefinition loc (FieldDefinition ident ts) = do
  -- First we check its type is well-defined
  ty <- typeTypeSpecifier loc ts
  checkTerminaType loc ty
  -- and that it is simply (see simple types).
  structFieldTyOrFail loc ty
  return $ SAST.FieldDefinition ident ty

-- Enum Variant definition helpers.
typeEnumVariant :: Location -> EnumVariant -> SemanticMonad SAST.EnumVariant
typeEnumVariant loc (EnumVariant ident ps_ts) = do
  ptys <- mapM (
    \ts -> do
        ty <- typeTypeSpecifier loc ts
        enumParamTyOrFail loc ty
        return ty) ps_ts
  return $ SAST.EnumVariant ident ptys
