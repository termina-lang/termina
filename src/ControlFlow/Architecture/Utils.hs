{-# LANGUAGE FlexibleContexts #-}

module ControlFlow.Architecture.Utils where

import ControlFlow.BasicBlocks.AST
import ControlFlow.Architecture.Types
import qualified Data.Map.Strict as M
import Data.List (group, sort, foldl')
import Control.Monad.Except
import ControlFlow.Architecture.Errors
import Semantic.Types
import Utils.Annotations
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Extras.Graph (findDisjointPaths, reverseGraph)
import Control.Monad.State

getEmitterIdentifier :: TPEmitter a -> Identifier
getEmitterIdentifier (TPInterruptEmittter ident _) = ident
getEmitterIdentifier (TPPeriodicTimerEmitter ident _ _) = ident
getEmitterIdentifier (TPSystemInitEmitter ident _) = ident
getEmitterIdentifier (TPSystemExceptEmitter ident _) = ident

getMemberFunctions :: [ClassMember a] -> M.Map Identifier (TPFunction a)
getMemberFunctions = foldl' (\acc member ->
  case member of
    ClassMethod _ak identifier mRetTy blk ann  ->
      M.insert identifier (TPFunction identifier [] mRetTy blk ann)   acc
    ClassProcedure _ak identifier params blk ann ->
      M.insert identifier (TPFunction identifier params Nothing blk ann) acc
    ClassAction _ak identifier Nothing rty blk ann ->
      M.insert identifier (TPFunction identifier [] (Just rty) blk ann) acc
    ClassAction _ak identifier (Just param) rty blk ann ->
      M.insert identifier (TPFunction identifier [param] (Just rty) blk ann) acc
    ClassViewer identifier params mRetTy blk ann ->
      M.insert identifier (TPFunction identifier params mRetTy blk ann) acc
    _ -> acc
  ) M.empty

getInputPorts :: [ClassMember a] -> M.Map Identifier (TerminaType a, Identifier)
getInputPorts = foldl' (\acc member ->
  case member of
    ClassField (FieldDefinition fid fty _) ->
      case fty of
        TInPort dataTy action -> M.insert fid (dataTy, action) acc
        _ -> acc
    _ -> acc
  ) M.empty

getSinkPorts :: [ClassMember a] -> M.Map Identifier (TerminaType a, Identifier)
getSinkPorts = foldl' (\acc member ->
  case member of
    ClassField (FieldDefinition fid fty _) ->
      case fty of
        TSinkPort dataTy action -> M.insert fid (dataTy, action) acc
        _ -> acc
    _ -> acc
  ) M.empty

getOutputPorts :: [ClassMember a] -> M.Map Identifier (TerminaType a)
getOutputPorts = foldl' (\acc member ->
  case member of
    ClassField (FieldDefinition fid fty _) ->
      case fty of
        TOutPort dataTy -> M.insert fid dataTy acc
        _ -> acc
    _ -> acc
  ) M.empty

getPortType :: Identifier -> [ClassMember a] -> TerminaType a
getPortType ident [] = error $ "Internal error: no port with identifier " ++ ident
getPortType ident (member : members) =
  case member of
    ClassField (FieldDefinition fid fty _) | fid == ident -> fty
    _ -> getPortType ident members

getClassMembers :: TypeDef a -> [ClassMember a]
getClassMembers (Class _ _ members _ _) = members
getClassMembers _ = error "Internal error: getClassMembers called with the non-class type definition"

getConnectedEmitters :: TerminaProgArch a -> [TPEmitter a]
getConnectedEmitters tp =
  filter (\emitter -> M.member (getEmitterIdentifier emitter) (emitterTargets tp)) $ M.elems (emitters tp)

genResourceUsageGraph :: TerminaProgArch a -> M.Map Identifier [Identifier]
genResourceUsageGraph progArchitecture = S.toList <$> resDependenciesMap

  where

    -- | This function returns a map of resource dependencies. The key is the
    -- resource identifier and the value is the set of resources that the
    -- resource depends on.
    resDependenciesMap :: M.Map Identifier (S.Set Identifier)
    resDependenciesMap =
      let initialMap = M.fromList $
            map (, S.empty) (M.keys (pools progArchitecture)) ++
            map (, S.empty) (M.keys (atomics progArchitecture)) ++
            map (, S.empty) (M.keys (atomicArrays progArchitecture))
          resoucesGraph = foldr (\(TPResource identifier _ aps _ _) acc ->
              M.insert identifier (S.fromList (fst <$> M.elems aps)) acc
            ) initialMap (M.elems (resources progArchitecture))
          tasksGraph = foldr (\(TPTask identifier _ _ _ _ apConns _ _ _) acc ->
              M.insert identifier (S.fromList (fst <$> M.elems apConns)) acc
            ) resoucesGraph (M.elems (tasks progArchitecture))
          handlersGraph = foldr (\(TPHandler identifier _ _ _ apConns _ _ _) acc ->
              M.insert identifier (S.fromList (fst <$> M.elems apConns)) acc
            ) tasksGraph (M.elems (handlers progArchitecture))

      in
        handlersGraph


-- | Obtains the required locking mechanisms for the resources in the program architecture.
-- This function analyzes resource dependencies and determines the appropriate locking strategy
-- based on the system architecture and access patterns.
--
-- The locking strategy is determined by:
-- 1. Whether the resource is atomic (no locking needed)
-- 2. The types of components accessing the resource (tasks, handlers, or the handler attached to the initial event)
-- 3. The priority levels of tasks accessing the resource

newtype ResLockingSt = ResLockingSt
  { 
    resLockingMap :: M.Map Identifier ResourceLock
  }

-- Monad to compute.
-- Error TopSortError and State TopSt
type ResLockingMonad = State ResLockingSt

genResourceLockings :: TerminaProgArch a -> M.Map Identifier ResourceLock
genResourceLockings programArchitecture =
  evalState (genResourceLockingsInternal programArchitecture >> gets resLockingMap) (ResLockingSt M.empty)

genResourceLockingsInternal :: TerminaProgArch a -> ResLockingMonad ()
genResourceLockingsInternal programArchitecture =
  let resIds = M.keys $ resources programArchitecture
      poolIds = M.keys $ pools programArchitecture
      atomicIds = M.keys (atomics programArchitecture) ++ M.keys (atomicArrays programArchitecture) in
  mapM_ processResource (resIds ++ poolIds ++ atomicIds)

  where

    reversedGraph = reverseGraph $ genResourceUsageGraph programArchitecture

    processResource ::
        Identifier -- ^ Resource identifier
        -> ResLockingMonad ResourceLock
    processResource resId = do
      resLockMap <- gets resLockingMap
      case M.lookup resId resLockMap of
        Just resLock -> return resLock
        Nothing ->
          if isAtomic resId then
              -- Atomic resources don't need locking
              let resLock = ResourceLockNone in
              modify (\st ->
                st { resLockingMap = M.insert resId resLock (resLockingMap st) }) >> return resLock
          else do
            -- Non-atomic resources need appropriate locking based on their dependencies
            case findDisjointPaths reversedGraph resId of
              Left err -> error $ "error findDisjointPaths: " ++ show reversedGraph ++ " " ++ show resId ++ " " ++ show err
              Right [] -> error $ "Internal error: no paths found for resource " ++ resId
              Right [_] -> 
                -- Atomic resources don't need locking
                let resLock = ResourceLockNone in
                modify (\st ->
                  st { resLockingMap = M.insert resId resLock (resLockingMap st) }) >> return resLock
              Right paths -> do
                let dest = head <$> paths
                -- We must filter out the initialization handler, as it does not require locking
                let filteredDest = filter (not . isInitHandler) dest
                 -- Determine the locking mechanism based on the access patterns
                resLock <- getResLocking filteredDest
                modify (\st ->
                  st { resLockingMap = M.insert resId resLock (resLockingMap st) }) >> return resLock

    -- | Checks if a resource is atomic (either a single atomic variable or an atomic array)
    isAtomic :: Identifier -> Bool
    isAtomic identifier = do
        case M.lookup identifier (atomics programArchitecture) of
            Just _ -> True
            Nothing -> isJust $ M.lookup identifier (atomicArrays programArchitecture)

    -- | Checks if a handler is the system initialization handler
    isInitHandler :: Identifier -> Bool
    isInitHandler ident =
        case M.lookup "system_init" (emitterTargets programArchitecture) of
            Just (ident', _, _) -> ident == ident'
            Nothing -> False

    -- | Obtains the locking mechanism that must be used for a resource
    getResLocking :: [Identifier] -> ResLockingMonad ResourceLock
    getResLocking [] = error "Internal error: empty resource list in getResLocking"
    getResLocking [_] = return ResourceLockNone
    getResLocking (ident: ids) = do
        case M.lookup ident (handlers programArchitecture) of
            Just _ -> return ResourceLockIrq
            Nothing -> case M.lookup ident (tasks programArchitecture) of
                Just tsk -> getResLocking' (getPriority tsk) ids
                Nothing -> 
                  error $ "Internal error: resource not found in usage map: " ++ ident


    getResLocking' :: TInteger -> [Identifier] -> ResLockingMonad ResourceLock
    -- | If we have reach the end of the list, it means that there are at least two different tasks that
    -- access the resource. We are going to force the use of the priority ceiling algorithm. In the
    -- (hopefully near) future, we will support algorithm selection via the configuration file.
    getResLocking' ceilPrio [] = return $ ResourceLockMutex ceilPrio
    getResLocking' ceilPrio (ident : ids) =
        case M.lookup ident (handlers programArchitecture) of
            Just _ -> return ResourceLockIrq
            Nothing -> case M.lookup ident (tasks programArchitecture) of
                Just tsk ->
                    getResLocking' (min ceilPrio (getPriority tsk)) ids
                Nothing -> 
                    error $ "Internal error: resource not found in usage map: " ++ ident


getGlobDeclModules :: TerminaProgArch a -> [QualifiedName]
getGlobDeclModules progArchitecture =
  map head . group . sort $ taskModules
    ++ handlerModules
    ++ catMaybes resourceModules
    ++ poolModules
    ++ atomicModules
    ++ atomicArrayModules
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
getObjType :: (MonadError ArchitectureError m) => Object SemanticAnn -> m (TerminaType SemanticAnn)
getObjType (Variable _ (SemanticAnn (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (Variable {}) = throwError $ annotateError Internal EInvalidObjectTypeAnnotation
getObjType (ArrayIndexExpression _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))    = return ts
getObjType (ArrayIndexExpression {}) = throwError $ annotateError Internal EInvalidObjectTypeAnnotation
getObjType (MemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))            = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ _ ts)) _))     = return ts
getObjType (MemberAccess {}) = throwError $ annotateError Internal EInvalidObjectTypeAnnotation
getObjType (Dereference _ (SemanticAnn (ETy (ObjectType _ ts)) _))               = return ts
getObjType (Dereference {}) = throwError $ annotateError Internal EInvalidObjectTypeAnnotation
getObjType (Unbox _ (SemanticAnn (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (Unbox {}) = throwError $ annotateError Internal EInvalidObjectTypeAnnotation
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _)) = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ _ ts)) _)) = return ts
getObjType (DereferenceMemberAccess {}) = throwError $ annotateError Internal EInvalidObjectTypeAnnotation

-- | This function returns the type of an expression. The type is extracted from the
-- expression's semantic annotation. The function assumes that the expression is well-typed
-- and that the semantic annotation is correct. If the expression is not well-typed, the
-- function will throw an error.
getExprType :: (MonadError ArchitectureError m) => Expression SemanticAnn -> m (TerminaType SemanticAnn)
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (Constant {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (MonadicVariantInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (MonadicVariantInitializer {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (BinOp _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (ReferenceExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (Casting _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (FunctionCall _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (FunctionCall {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (MemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (DerefMemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (StructInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (StructInitializer {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (EnumVariantInitializer _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (ArrayInitializer _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (ArrayExprListInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (StringInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (StringInitializer {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (ArraySliceExpression _ _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArraySliceExpression {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (IsEnumVariantExpression _ _ _  (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (IsEnumVariantExpression {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation
getExprType (IsMonadicVariantExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (IsMonadicVariantExpression {}) = throwError $ annotateError Internal EInvalidExprTypeAnnotation

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
                _ -> throwError $ annotateError Internal EInvalidPortAccessExpression
        TOutPort _ ->
            case obj of
                (MemberAccess _ portName _) -> return portName
                (DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ annotateError Internal EInvalidPortAccessExpression
        _ -> throwError $ annotateError Internal EExpectedPort

getObjOptionBoxName :: (MonadError ArchitectureError m) => Object SemanticAnn -> m Identifier
getObjOptionBoxName obj@(Variable name _) = do
  ty <- getObjType obj
  case ty of
    TOption (TBoxSubtype _) -> return name
    _ -> throwError $ annotateError Internal EExpectedOptionBoxType
getObjOptionBoxName (Dereference expr _) = getObjOptionBoxName expr
getObjOptionBoxName _ = throwError $ annotateError Internal EExpectedOptionBoxType

getObjBoxName :: (MonadError ArchitectureError m) => Object SemanticAnn -> m Identifier
getObjBoxName obj@(Variable name _) = do
  ty <- getObjType obj
  case ty of
    TBoxSubtype _ -> return name
    _ -> throwError $ annotateError Internal EExpectedBoxSubtype
getObjBoxName _ = throwError $ annotateError Internal EExpectedBoxSubtype

getExprOptionBoxName :: (MonadError ArchitectureError m) => Expression SemanticAnn -> m Identifier
getExprOptionBoxName (AccessObject obj) = getObjOptionBoxName obj
getExprOptionBoxName (ReferenceExpression _ak obj _ann) = getObjOptionBoxName obj
getExprOptionBoxName _ = throwError $ annotateError Internal EExpectedOptionBoxType

getExprBoxName :: (MonadError ArchitectureError m) => Expression SemanticAnn -> m Identifier
getExprBoxName (AccessObject obj) = getObjBoxName obj
getExprBoxName _ = throwError $ annotateError Internal EExpectedBoxSubtype

getInBox :: InOptionBox a -> InBox a
getInBox (InOptionBoxAlloc ident ann) = InBoxAlloc ident ann
getInBox (InOptionBoxProcedureCall ident idx _ann) = InBoxProcedureCall ident idx

getInterruptEmittersToTasks :: TerminaProgArch a -> [TPEmitter a]
getInterruptEmittersToTasks progArchitecture = foldl (\acc emitter ->
    case emitter of
        TPInterruptEmittter identifier _ ->
            case M.lookup identifier (emitterTargets progArchitecture) of
                Just (entity, _port, _) ->
                    case M.lookup entity (tasks progArchitecture) of
                        Just _ -> emitter : acc
                        Nothing -> acc
                Nothing -> acc
        _ -> acc
    ) [] (getConnectedEmitters progArchitecture)

getPeriodicTimersToTasks :: TerminaProgArch a -> [TPEmitter a]
getPeriodicTimersToTasks progArchitecture = foldl (\acc emitter ->
    case emitter of
        TPPeriodicTimerEmitter identifier _ _ ->
            case M.lookup identifier (emitterTargets progArchitecture) of
                Just (entity, _port, _) ->
                    case M.lookup entity (tasks progArchitecture) of
                        Just _ -> emitter : acc
                        Nothing -> acc
                Nothing -> acc
        _ -> acc
    ) [] (getConnectedEmitters progArchitecture)

-- | Returns the value of the "priority" modifier, if present in the list of modifiers.
-- If not, it returns 255, which is the default value for the priority (the lowest).
getPriority :: TPTask a -> TInteger
getPriority = getPriority' . taskModifiers

  where

    getPriority' :: [Modifier a] -> TInteger
    getPriority' [] = TInteger 255 DecRepr
    getPriority' ((Modifier "priority" (Just (I priority _))) : _) = priority
    getPriority' (_ : modifiers) = getPriority' modifiers


-- | Returns the value of the "stack_size" modifier, if present in the list of modifiers.
-- If not, it returns 4096, which is the default value for the stack size (RTEMS_MINIUMUM_STACK_SIZE)
getStackSize :: TPTask a -> TInteger
getStackSize = getStackSize' . taskModifiers

  where

    getStackSize' :: [Modifier a] -> TInteger
    getStackSize' [] = TInteger 4096 DecRepr
    getStackSize' ((Modifier "stack_size" (Just (I stackSize _))) : _) = stackSize
    getStackSize' (_ : modifiers) = getStackSize' modifiers

isUnprotected :: ProcedureSeman a -> Bool
isUnprotected (ProcedureSeman _ _ modifiers)= isUnprotected' modifiers

  where

    isUnprotected' :: [Modifier a] -> Bool
    isUnprotected' [] = False
    isUnprotected' ((Modifier "unprotected" Nothing) : _) = True
    isUnprotected' (_ : xs) = isUnprotected' xs