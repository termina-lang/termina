module DataFlow.Architecture (
    runGenArchitecture, emptyTerminaProgArch, getResDependencies, getGlobDeclModules
) where

import DataFlow.Architecture.Types
import Control.Monad.Except
import DataFlow.Architecture.Errors.Errors
import qualified Control.Monad.State.Strict as ST
import ControlFlow.AST
import Data.Maybe
import DataFlow.Architecture.Utils
import Semantic.Types
import Utils.Annotations
import qualified Data.Map as M
import qualified Data.Set as S
import Modules.Modules
import Data.List (group, sort)

type ArchitectureMonad = ExceptT ProgramError (ST.State (TerminaProgArch SemanticAnn))

genArchTypeDef :: TypeDef SemanticAnn -> ArchitectureMonad ()
genArchTypeDef tydef@(Class TaskClass ident _ _ _) =
  ST.modify $ \tp ->
    tp {
      taskClasses = M.insert ident tydef (taskClasses tp)
    }
genArchTypeDef tydef@(Class HandlerClass ident _ _ _) =
  ST.modify $ \tp ->
    tp {
      handlerClasses = M.insert ident tydef (handlerClasses tp)
    }
genArchTypeDef tydef@(Class ResourceClass ident _ _ _) =
  ST.modify $ \tp ->
    tp {
      resourceClasses = M.insert ident tydef (resourceClasses tp)
    }
genArchTypeDef _ = return ()

genArchGlobal :: QualifiedName -> Global SemanticAnn -> ArchitectureMonad ()
genArchGlobal _ (Const {}) = return ()
genArchGlobal modName (Emitter ident emitterCls _ _ ann) = do
  case emitterCls of
    (DefinedType "Interrupt") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPInterruptEmittter ident ann) (emitters tp)
      }
    (DefinedType "PeriodicTimer") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPPeriodicTimerEmitter ident modName ann) (emitters tp)
       }
    (DefinedType "SystemInit") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPSystemInitEmitter ident ann) (emitters tp)
      }
    _ -> throwError $ annotateError (location ann) (UnsupportedEmitterClass ident)
genArchGlobal modName (Task ident (DefinedType tcls) (Just (StructInitializer assignments _ _)) modifiers tann) = do
  members <- ST.get >>= \tp -> return $ getClassMembers (fromJust (M.lookup tcls (taskClasses tp)))
  (inpConns, sinkConns, outpConns, apConns) <- foldM (\(inp, sink, outp, accp) assignment ->
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case getPortType pname members of
          InPort ts action -> do
            connectedTargets <- channelTargets <$> ST.get
            -- | Check if the target channel is already connected to an in port
            case M.lookup target connectedTargets of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    channelTargets = M.insert target (ident, pname, action, cann) connectedTargets
                  }
                return (M.insert pname (ts, target, cann) inp, sink, outp, accp)
              Just (_, _, _, prevcann) ->
                throwError $ annotateError (location cann) (DuplicatedChannelConnection target (location prevcann))
          SinkPort ts action -> do
            connectedEmitters <- emitterTargets <$> ST.get
            -- | Check if the target emmiter is already connected to a sink port
            case M.lookup target connectedEmitters of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    emitterTargets = M.insert target (ident, pname, action, cann) connectedEmitters
                  }
                return (inp, M.insert pname (ts, target, cann) sink, outp, accp)
              Just (_, _, _, prevcann) ->
                throwError $ annotateError (location cann) (DuplicatedEmitterConnection target (location prevcann))
          _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann ->
        case getPortType pname members of
          OutPort ts -> return (inp, sink, M.insert pname (ts, target, cann) outp, accp)
          _ -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann  ->
        return (inp, sink, outp, M.insert pname (target, cann) accp)
      _ -> return (inp, sink, outp, accp)
    ) (M.empty, M.empty, M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      tasks = M.insert ident (TPTask ident tcls inpConns sinkConns outpConns apConns modifiers modName tann) (tasks tp)
    }
-- | Task declaration without struct initializer or a proper type specifier
-- This should not happen, since a task must define at least one inbound port
genArchGlobal _ (Task {}) = error "Internal error: invalid task declaration"
genArchGlobal modName (Resource ident (DefinedType rcls) initializer _ rann) =
  case initializer of
    Nothing -> ST.modify $ \tp ->
      tp {
        resources = M.insert ident (TPResource ident rcls M.empty modName rann) (resources tp)
      }
    Just (StructInitializer assignments _ _) -> do
      apConns <- foldM (\accps assignment -> do
        case assignment of
          FieldPortConnection AccessPortConnection pname target cann  ->
            return $ M.insert pname (target, cann) accps
          _ -> return accps
        ) M.empty assignments
      ST.modify $ \tp ->
        tp {
          resources = M.insert ident (TPResource ident rcls apConns modName rann) (resources tp)
        }
    -- | Resource initializer is not a struct initializer
    -- This should not happen, since the type checker must not allow initializing a
    -- resource with anything other than a struct initializer 
    Just _ -> error "Internal error: resource initializer is not a struct initializer"
genArchGlobal modName (Resource ident (Atomic aty) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      atomics = M.insert ident (TPAtomic ident aty modName rann)  (atomics tp)
    }
genArchGlobal modName (Resource ident (AtomicArray aty size) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      atomicArrays = M.insert ident (TPAtomicArray ident aty size modName rann) (atomicArrays tp)
    }
genArchGlobal modName (Resource ident (Pool aty size) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      pools = M.insert ident (TPPool ident aty size modName rann) (pools tp)
    }
genArchGlobal _ (Resource {}) = error "Internal error: invalid resource declaration"
genArchGlobal modName (Handler ident (DefinedType hcls) (Just (StructInitializer assignments _ _)) modifiers hann) = do
  members <- ST.get >>= \tp ->
    case M.lookup hcls (handlerClasses tp) of
      Nothing -> error $ "Handler class: " ++ hcls ++ " not found"
      Just cls -> return $ getClassMembers cls
  (sinkConn, outpConns, apConns) <- foldM (\(sink, outp, accp) assignment -> do
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case getPortType pname members of
          SinkPort ts action -> do
            connectedEmitters <- emitterTargets <$> ST.get
            -- | Check if the target emmiter is already connected to a sink port
            case M.lookup target connectedEmitters of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    emitterTargets = M.insert target (ident, pname, action, cann) connectedEmitters
                  }
                return (Just (pname, ts, target, cann), outp, accp)
              Just (_, _, _, prevcann) ->
                throwError $ annotateError (location cann) (DuplicatedEmitterConnection target (location prevcann))
          _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann ->
        case getPortType pname members of
          OutPort ts -> return (sink, M.insert pname (ts, target, cann) outp, accp)
          _ -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann  ->
        return (sink, outp, M.insert pname (target, cann) accp)
      _ -> return (sink, outp, accp)
    ) (Nothing, M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      handlers = M.insert ident (TPHandler ident hcls (fromJust sinkConn) outpConns apConns modifiers modName hann) (handlers tp)
    }
-- | Handler declaration without struct initializer or a proper type specifier
-- This should not happen, since a handler must define one sink port
genArchGlobal _ (Handler {}) = error "Internal error: invalid handler declaration"
genArchGlobal modName (Channel ident (MsgQueue mty size) _ _ cann) =
  ST.modify $ \tp ->
    tp {
      channels = M.insert ident (TPMsgQueue ident mty size modName cann) (channels tp)
    }
genArchGlobal _ (Channel {}) = error "Internal error: invalid channel declaration"

genArchElement :: QualifiedName -> AnnASTElement SemanticAnn -> ArchitectureMonad ()
genArchElement _ (Function {}) = return ()
genArchElement modName (GlobalDeclaration glb) = genArchGlobal modName glb
genArchElement _ (TypeDefinition typeDef _) = genArchTypeDef typeDef

emptyTerminaProgArch :: TerminaProgArch SemanticAnn
emptyTerminaProgArch = TerminaProgArch {
  emitters = M.fromList [
    ("system_init", TPSystemInitEmitter "system_init" (Located (GTy (GGlob (SEmitter (DefinedType "SystemInit")))) Internal))
  ],
  emitterTargets = M.empty,
  taskClasses = M.empty,
  tasks = M.empty,
  handlerClasses = M.empty,
  handlers = M.empty,
  resourceClasses = M.empty,
  resources = M.empty,
  pools = M.empty,
  atomics = M.empty,
  atomicArrays = M.empty,
  channels = M.empty,
  channelTargets = M.empty
}

runGenArchitecture ::
  TerminaProgArch SemanticAnn
  -> QualifiedName -> AnnotatedProgram SemanticAnn
  -> Either ProgramError (TerminaProgArch SemanticAnn)
runGenArchitecture tp modName elements =
  case flip ST.runState tp . runExceptT $ mapM_ (genArchElement modName) elements of
    (Left err, _) -> Left err
    (Right _, st) -> Right st

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
        case M.lookup res resDependenciesMap of
          Nothing ->
            -- | This means that the resource does not depend on any other resource
            case M.lookup res acc of
              Nothing -> M.insert res (S.singleton agent) acc
              Just agents -> M.insert res (S.insert agent agents) acc
          Just ids ->
            let acc' = case M.lookup res acc of
                  Nothing -> M.insert res (S.singleton agent) acc
                  Just agents -> M.insert res (S.insert agent agents) acc in
            -- | ids is the list of tasks and handlers that the resource depends on.
            -- We must recursively update the dependencies of these resources so that
            -- they also depend on the agent
            resDependencies agent ress $ resDependencies agent (S.toList ids) acc' --}

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

