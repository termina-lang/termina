{-# LANGUAGE FlexibleContexts #-}

module ControlFlow.Architecture.Utils where

import ControlFlow.BasicBlocks.AST
import ControlFlow.Architecture.Types
import qualified Data.Map as M
import Modules.Modules
import qualified Data.Set as S
import Data.List (group, sort, foldl')
import Control.Monad.Except
import ControlFlow.Architecture.Errors.Errors
import Semantic.Types
import Utils.Annotations

getEmmiterIdentifier :: TPEmitter a -> Identifier
getEmmiterIdentifier (TPInterruptEmittter ident _) = ident
getEmmiterIdentifier (TPPeriodicTimerEmitter ident _ _) = ident
getEmmiterIdentifier (TPSystemInitEmitter ident _) = ident

getInputPorts :: [ClassMember a] -> M.Map Identifier (TerminaType, Identifier)
getInputPorts = foldl' (\acc member -> 
  case member of
    ClassField (FieldDefinition fid fty) _ -> 
      case fty of
        TInPort dataTy action -> M.insert fid (dataTy, action) acc
        _ -> acc
    _ -> acc
  ) M.empty

getSinkPorts :: [ClassMember a] -> M.Map Identifier (TerminaType, Identifier)
getSinkPorts = foldl' (\acc member -> 
  case member of
    ClassField (FieldDefinition fid fty) _ -> 
      case fty of
        TSinkPort dataTy action -> M.insert fid (dataTy, action) acc
        _ -> acc
    _ -> acc
  ) M.empty

getOutputPorts :: [ClassMember a] -> M.Map Identifier TerminaType
getOutputPorts = foldl' (\acc member -> 
  case member of
    ClassField (FieldDefinition fid fty) _ -> 
      case fty of
        TOutPort dataTy -> M.insert fid dataTy acc
        _ -> acc
    _ -> acc
  ) M.empty

getPortType :: Identifier -> [ClassMember a] -> TerminaType
getPortType ident [] = error $ "Internal error: no port with identifier " ++ ident
getPortType ident (member : members) =
  case member of
    ClassField (FieldDefinition fid fty) _ | fid == ident -> fty
    _ -> getPortType ident members

getClassMembers :: TypeDef a -> [ClassMember a]
getClassMembers (Class _ _ members _ _) = members
getClassMembers _ = error "Internal error: getClassMembers called with the non-class type definition"

getConnectedEmitters :: TerminaProgArch a -> [TPEmitter a]
getConnectedEmitters tp = 
  filter (\emitter -> M.member (getEmmiterIdentifier emitter) (emitterTargets tp)) $ M.elems (emitters tp)

getResDependencies :: TerminaProgArch a -> M.Map Identifier (S.Set Identifier)
getResDependencies progArchitecture = 
  foldr (\(resource, resDeps) acc ->
      -- | This means that the resource depends on other resources
      -- We must obtain the list of tasks and handlers that depend on the resource
      case M.lookup resource acc of
        Nothing ->
          -- | This means that the resource only depends on other resources
          acc
        Just agents ->
          -- | This means that the resource depends on other resources and on tasks or handlers
          -- We must update the list of tasks and handlers that depend on the resource
          -- with the list of tasks and handlers that depend on the resources that the resource depends on
          foldr (\agent acc' ->
            resDependencies agent (S.toList resDeps) acc') acc (S.toList agents)
    ) reverseDependencies (M.toList resDependenciesMap)

  where

    -- | This function returns a map of resource dependencies. The key is the
    -- resource identifier and the value is the set of resources that the
    -- resource depends on.
    resDependenciesMap :: M.Map Identifier (S.Set Identifier)
    resDependenciesMap = foldr (\(TPResource identifier _ aps _ _) acc ->
        if null aps then acc else
          M.insert identifier (S.fromList (fst <$> M.elems aps)) acc
      ) M.empty (M.elems (resources progArchitecture))

    reverseTaskDependencies :: M.Map Identifier (S.Set Identifier)
    reverseTaskDependencies = foldr (\(TPTask identifier _ _ _ _ apConns _ _ _) acc ->
        foldr (\(target, _) acc' ->
          case M.lookup target acc' of
            Nothing -> M.insert target (S.singleton identifier) acc'
            Just ids -> M.insert target (S.insert identifier ids) acc'
          ) acc (M.elems apConns)
      ) M.empty (M.elems (tasks progArchitecture))

    reverseDependencies :: M.Map Identifier (S.Set Identifier)
    reverseDependencies = foldr (\(TPHandler identifier _ _ _ apConns _ _ _) acc ->
        foldr (\(target, _) acc' ->
          case M.lookup target acc' of
            Nothing -> M.insert target (S.singleton identifier) acc'
            Just ids -> M.insert target (S.insert identifier ids) acc'
          ) acc (M.elems apConns)
      ) reverseTaskDependencies (M.elems (handlers progArchitecture))

    -- | Now we must update the reverse dependencies map with the resource dependencies
    -- This is done by iterating over the resource dependencies map and updating the
    -- reverse dependencies map with the resource dependencies
    resDependencies ::
      -- | Task or handler identifier
      Identifier
      -- | List of resources that indirectly depend on the task or handler
      -> [Identifier]
      -- | Accumulator
      -> M.Map Identifier (S.Set Identifier)
      -> M.Map Identifier (S.Set Identifier)
    resDependencies _ [] acc = acc
    resDependencies agent (res : ress) acc =
        let acc' = case M.lookup res acc of
                  Nothing -> M.insert res (S.singleton agent) acc
                  Just agents -> M.insert res (S.insert agent agents) acc in
        case M.lookup res resDependenciesMap of
          Nothing -> resDependencies agent ress acc'
          Just ids ->
            -- | ids is the list of tasks and handlers that the resource depends on.
            -- We must recursively update the dependencies of these resources so that
            -- they also depend on the agent
            resDependencies agent ress $ resDependencies agent (S.toList ids) acc'

getGlobDeclModules :: TerminaProgArch a -> [QualifiedName]
getGlobDeclModules progArchitecture = 
  map head . group . sort $ taskModules ++ handlerModules ++ resourceModules ++ poolModules ++ atomicModules ++ atomicArrayModules
  where
    taskModules = map taskModule $ M.elems (tasks progArchitecture)
    handlerModules = map handlerModule $ M.elems (handlers progArchitecture)
    resourceModules = map resourceModule $ M.elems (resources progArchitecture)
    poolModules = map poolModule $ M.elems (pools progArchitecture)
    atomicModules = map atomicModule $ M.elems (atomics progArchitecture)
    atomicArrayModules = map atomicArrayModule $ M.elems (atomicArrays progArchitecture)

    -- | This function returns the module name of a pool
    poolModule :: TPPool a -> QualifiedName
    poolModule (TPPool _ _ _ modName _) = modName

    -- | This function returns the module name of an atomic
    atomicModule :: TPAtomic a -> QualifiedName
    atomicModule (TPAtomic _ _ modName _) = modName

    -- | This function returns the module name of an atomic array
    atomicArrayModule :: TPAtomicArray a -> QualifiedName
    atomicArrayModule (TPAtomicArray _ _ _ modName _) = modName

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError ArchitectureError m) => Object SemanticAnn -> m TerminaType
getObjType (Variable _ (LocatedElement (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (ArrayIndexExpression _ _ (LocatedElement (ETy (ObjectType _ ts)) _))    = return ts
getObjType (MemberAccess _ _ (LocatedElement (ETy (ObjectType _ ts)) _))            = return ts
getObjType (Dereference _ (LocatedElement (ETy (ObjectType _ ts)) _))               = return ts
getObjType (Unbox _ (LocatedElement (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (DereferenceMemberAccess _ _ (LocatedElement (ETy (ObjectType _ ts)) _)) = return ts
getObjType _ = throwError $ annotateError Internal EUnboxingObject

-- | This function returns the type of an expression. The type is extracted from the
-- expression's semantic annotation. The function assumes that the expression is well-typed
-- and that the semantic annotation is correct. If the expression is not well-typed, the
-- function will throw an error.
getExprType :: (MonadError ArchitectureError m) => Expression SemanticAnn -> m TerminaType
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (OptionVariantInitializer _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp _ _ _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression _ _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting _ _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (FunctionCall _ _ (LocatedElement (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall _ _ _ (LocatedElement (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall _ _ _ (LocatedElement (ETy (AppType _ ts)) _)) = return ts
getExprType (StructInitializer _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer _ _ _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer _ _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer _ (LocatedElement (ETy (SimpleType ts)) _)) = return ts
getExprType _ = throwError $ annotateError Internal EUnboxingExpression

-- | This function returns the name of a port. The function assumes that the object is
-- a port and that the object is well-typed. If the object is not a port or if the object
-- is not well-typed, the function will throw an error.    
getPortName :: (MonadError ArchitectureError m) => Object SemanticAnn -> m Identifier
getPortName obj = do
    obj_type <- getObjType obj
    case obj_type of 
        TAccessPort _ -> 
            case obj of
                (MemberAccess _ portName _) -> return portName
                (DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ annotateError Internal EUnboxingPort
        TOutPort _ -> 
            case obj of
                (MemberAccess _ portName _) -> return portName
                (DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ annotateError Internal EUnboxingPort
        _ -> throwError $ annotateError Internal EUnboxingPort

getObjOptionBoxName :: (MonadError ArchitectureError m) => Object SemanticAnn -> m Identifier
getObjOptionBoxName obj@(Variable name _) = do
  ty <- getObjType obj
  case ty of
    TOption (TBoxSubtype _) -> return name
    _ -> throwError $ annotateError Internal EUnboxingOptionBox
getObjOptionBoxName (Dereference expr _) = getObjOptionBoxName expr
getObjOptionBoxName _ = throwError $ annotateError Internal EUnboxingOptionBox

getObjBoxName :: (MonadError ArchitectureError m) => Object SemanticAnn -> m Identifier
getObjBoxName obj@(Variable name _) = do
  ty <- getObjType obj
  case ty of
    TBoxSubtype _ -> return name
    _ -> throwError $ annotateError Internal EUnboxingBox
getObjBoxName _ = throwError $ annotateError Internal EUnboxingBox
  
getExprOptionBoxName :: (MonadError ArchitectureError m) => Expression SemanticAnn -> m Identifier
getExprOptionBoxName (AccessObject obj) = getObjOptionBoxName obj
getExprOptionBoxName (ReferenceExpression _ak obj _ann) = getObjOptionBoxName obj
getExprOptionBoxName _ = throwError $ annotateError Internal EUnboxingOptionBox

getExprBoxName :: (MonadError ArchitectureError m) => Expression SemanticAnn -> m Identifier
getExprBoxName (AccessObject obj) = getObjBoxName obj
getExprBoxName _ = throwError $ annotateError Internal EUnboxingBox

getInBox :: InOptionBox a -> InBox a
getInBox (InOptionBoxAlloc ident ann) = InBoxAlloc ident ann
getInBox (InOptionBoxProcedureCall ident idx _ann) = InBoxProcedureCall ident idx