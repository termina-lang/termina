module DataFlow.Architecture.Utils where

import ControlFlow.AST
import DataFlow.Architecture.Types
import qualified Data.Map as M
import Modules.Modules
import qualified Data.Set as S
import Data.List (group, sort)

getEmmiterIdentifier :: TPEmitter a -> Identifier
getEmmiterIdentifier (TPInterruptEmittter ident _) = ident
getEmmiterIdentifier (TPPeriodicTimerEmitter ident _ _) = ident
getEmmiterIdentifier (TPSystemInitEmitter ident _) = ident

getTriggeredAction :: Identifier -> [ClassMember a] -> Identifier
getTriggeredAction ident [] = error $ "Internal error: no port with identifier " ++ ident
getTriggeredAction ident (member : members) =
  case member of
    ClassField (FieldDefinition fid fty) _ | fid == ident -> getTriggeredAction' fty
    _ -> getTriggeredAction ident members

  where

    getTriggeredAction' :: TerminaType -> Identifier
    getTriggeredAction' (SinkPort _ act) = act
    getTriggeredAction' (InPort _ act) = act
    getTriggeredAction' _ = error $ "Internal error: port " ++ ident ++ " is not a sink port or an in port"

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
