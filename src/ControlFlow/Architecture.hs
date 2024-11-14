module ControlFlow.Architecture (
    runGenArchitecture, emptyTerminaProgArch, getResDependencies, getGlobDeclModules
) where

import ControlFlow.Architecture.Types
import Control.Monad
import Control.Monad.Except
import ControlFlow.Architecture.Errors.Errors
import qualified Control.Monad.State.Strict as ST
import ControlFlow.BasicBlocks.AST
import Data.Maybe
import ControlFlow.Architecture.Utils
import Semantic.Types
import Utils.Annotations
import qualified Data.Map as M
import Modules.Modules
import ControlFlow.Architecture.BoxInOut
import ControlFlow.Architecture.Forwarding
import Configuration.Configuration

type ArchitectureMonad = ExceptT ArchitectureError (ST.State (TerminaProgArch SemanticAnn))

addResourceSource :: 
  Identifier -- ^ Name of the resource
  -> Identifier -- ^ Name of the element that is connected to the resource
  -> Identifier -- ^ Name of the port that is used to connect to the resource
  -> SemanticAnn
  -> ArchitectureMonad ()
addResourceSource target source port ann = do
  connectedResources <- ST.gets resourceSources
  case M.lookup target connectedResources of
    Nothing -> do
      ST.modify $ \tp ->
        tp {
          resourceSources = M.insert target [(source, port, ann)] connectedResources
        }
    Just s ->
      ST.modify $ \tp ->
        tp {
          resourceSources = M.insert target ((source, port, ann) : s) connectedResources
        }

addChannelTarget :: 
  Identifier -- ^ Name of the channel
  -> Identifier -- ^ Name of the element that is connected to the channel
  -> Identifier -- ^ Name of the port that is used to connect to the channel
  -> SemanticAnn
  -> ArchitectureMonad ()
addChannelTarget channel ident pname cann = do
  ST.modify $ \tp ->
    tp {
      channelTargets = M.insert channel (ident, pname, cann) (channelTargets tp)
    }

addChannelSource :: 
  Identifier -- ^ Name of the element that is connected to the channel
  -> Identifier -- ^ Name of the port that is used to connect to the channel
  -> Identifier -- ^ Name of the channel
  -> SemanticAnn
  -> ArchitectureMonad ()
addChannelSource ident pname channel ann = do
  channelSrcs <- ST.gets channelSources
  case M.lookup ident channelSrcs of
    Nothing -> do
      ST.modify $ \tp ->
        tp {
          channelSources = M.insert channel [(ident, pname, ann)] channelSrcs
        }
    Just s ->
      ST.modify $ \tp ->
        tp {
          channelSources = M.insert channel ((ident, pname, ann) : s) channelSrcs
        }

genArchTypeDef :: TypeDef SemanticAnn -> ArchitectureMonad ()
genArchTypeDef tydef@(Class TaskClass ident _ _ _) = do
  let members = getClassMembers tydef
      inPs = getInputPorts members
      sinkPs = getSinkPorts members
      outputPs = getOutputPorts members
  outBoxMap <-
    case runInOutClass tydef of 
      Left err -> throwError err
      Right boxMap -> return boxMap
  forwardingMap <-
    case runGetForwardingMap members of
      Left err -> throwError err
      Right m -> return m
  let tskCls = TPClass ident TaskClass tydef inPs sinkPs outputPs outBoxMap forwardingMap
  ST.modify $ \tp ->
    tp {
      taskClasses = M.insert ident tskCls (taskClasses tp)
    }
genArchTypeDef tydef@(Class HandlerClass ident _ _ _) = do
  let members = getClassMembers tydef
      inPs = M.empty
      sinkPs = getSinkPorts members
      outputPs = getOutputPorts members
  outBoxMap <-
    case runInOutClass tydef of 
      Left err -> throwError err
      Right boxMap -> return boxMap
  forwardingMap <-
    case runGetForwardingMap members of
      Left err -> throwError err
      Right m -> return m
  let hdlCls = TPClass ident HandlerClass tydef inPs sinkPs outputPs outBoxMap forwardingMap
  ST.modify $ \tp ->
    tp {
      handlerClasses = M.insert ident hdlCls (handlerClasses tp)
    }
genArchTypeDef tydef@(Class ResourceClass ident _ _ _) = do
  let inPs = M.empty
      sinkPs = M.empty
      outputPs = M.empty
  outBoxMap <-
    case runInOutClass tydef of 
      Left err -> throwError err
      Right boxMap -> return boxMap
  -- | Resources do not have forwarding actions
  let resCls = TPClass ident ResourceClass tydef inPs sinkPs outputPs outBoxMap M.empty
  ST.modify $ \tp ->
    tp {
      resourceClasses = M.insert ident resCls (resourceClasses tp)
    }
genArchTypeDef _ = return ()

genArchGlobal :: QualifiedName -> Global SemanticAnn -> ArchitectureMonad ()
genArchGlobal _ (Const {}) = return ()
genArchGlobal modName (Emitter ident emitterCls _ _ ann) = do
  case emitterCls of
    (TGlobal EmitterClass "Interrupt") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPInterruptEmittter ident ann) (emitters tp)
      }
    (TGlobal EmitterClass "PeriodicTimer") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPPeriodicTimerEmitter ident modName ann) (emitters tp)
       }
    (TGlobal EmitterClass "SystemInit") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPSystemInitEmitter ident ann) (emitters tp)
      }
    -- | Any other emitter class is not supported (this should not happen)
    _ -> throwError $ annotateError Internal EUnsupportedEmitterClass
genArchGlobal modName (Task ident (TGlobal TaskClass tcls) (Just (StructInitializer assignments _)) modifiers tann) = do
  members <- ST.get >>= \tp -> return $ getClassMembers (classTypeDef $ fromJust (M.lookup tcls (taskClasses tp)))
  (inpConns, sinkConns, outpConns, apConns) <- foldM (\(inp, sink, outp, accp) assignment ->
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case getPortType pname members of
          TInPort {} -> do
            connectedTargets <- channelTargets <$> ST.get
            -- | Check if the target channel is already connected to an in port
            case M.lookup target connectedTargets of
              Nothing -> do
                addChannelTarget target ident pname cann
                return (M.insert pname (target, cann) inp, sink, outp, accp)
              Just (_, _, prevcann) ->
                throwError $ annotateError (location cann) (EDuplicatedChannelConnection target (location prevcann))
          TSinkPort {} -> do
            connectedEmitters <- emitterTargets <$> ST.get
            -- | Check if the target emmiter is already connected to a sink port
            case M.lookup target connectedEmitters of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    emitterTargets = M.insert target (ident, pname, cann) connectedEmitters
                  }
                return (inp, M.insert pname (target, cann) sink, outp, accp)
              Just (_, _, prevcann) ->
                throwError $ annotateError (location cann) (EDuplicatedEmitterConnection target (location prevcann))
          _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann ->
        case getPortType pname members of
          TOutPort {} -> do
            addChannelSource ident pname target cann
            return (inp, sink, M.insert pname (target, cann) outp, accp)
          _ -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann -> do
        addResourceSource target ident pname cann
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
genArchGlobal modName (Resource ident (TGlobal ResourceClass rcls) initializer _ rann) =
  case initializer of
    Nothing -> ST.modify $ \tp ->
      tp {
        resources = M.insert ident (TPResource ident rcls M.empty modName rann) (resources tp)
      }
    Just (StructInitializer assignments _) -> do
      apConns <- foldM (\accps assignment -> do
        case assignment of
          FieldPortConnection AccessPortConnection pname target cann  -> do
            addResourceSource target ident pname cann
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
genArchGlobal modName (Resource ident (TAtomic aty) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      atomics = M.insert ident (TPAtomic ident aty modName rann)  (atomics tp)
    }
genArchGlobal modName (Resource ident (TAtomicArray aty size) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      atomicArrays = M.insert ident (TPAtomicArray ident aty size modName rann) (atomicArrays tp)
    }
genArchGlobal modName (Resource ident (TPool aty size) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      pools = M.insert ident (TPPool ident aty size modName rann) (pools tp)
    }
genArchGlobal _ (Resource {}) = error "Internal error: invalid resource declaration"
genArchGlobal modName (Handler ident (TGlobal HandlerClass hcls) (Just (StructInitializer assignments _)) modifiers hann) = do
  members <- ST.get >>= \tp ->
    case M.lookup hcls (handlerClasses tp) of
      Nothing -> error $ "Handler class: " ++ hcls ++ " not found"
      Just cls -> return $ getClassMembers (classTypeDef cls)
  (sinkConn, outpConns, apConns) <- foldM (\(sink, outp, accp) assignment -> do
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case getPortType pname members of
          TSinkPort {} -> do
            connectedEmitters <- emitterTargets <$> ST.get
            -- | Check if the target emmiter is already connected to a sink port
            case M.lookup target connectedEmitters of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    emitterTargets = M.insert target (ident, pname, cann) connectedEmitters
                  }
                return (Just (pname, target, cann), outp, accp)
              Just (_, _, prevcann) ->
                throwError $ annotateError (location cann) (EDuplicatedEmitterConnection target (location prevcann))
          _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann ->
        case getPortType pname members of
          TOutPort {} -> do
            addChannelSource ident pname target cann
            return (sink, M.insert pname (target, cann) outp, accp)
          _ -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann  -> do
        addResourceSource target ident pname cann
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
genArchGlobal modName (Channel ident (TMsgQueue mty size) _ _ cann) =
  ST.modify $ \tp ->
    tp {
      channels = M.insert ident (TPMsgQueue ident mty size modName cann) (channels tp)
    }
genArchGlobal _ (Channel {}) = error "Internal error: invalid channel declaration"

genArchElement :: QualifiedName -> AnnASTElement SemanticAnn -> ArchitectureMonad ()
genArchElement _ (Function {}) = return ()
genArchElement modName (GlobalDeclaration glb) = genArchGlobal modName glb
genArchElement _ (TypeDefinition typeDef _) = genArchTypeDef typeDef

emptyTerminaProgArch :: TerminaConfig -> TerminaProgArch SemanticAnn
emptyTerminaProgArch config = TerminaProgArch {
  emitters = if enableSystemInit config then M.fromList [
    ("system_init", TPSystemInitEmitter "system_init" (LocatedElement (GTy (TGlobal EmitterClass "SystemInit")) Internal))
  ] else M.empty,
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
  channelSources = M.empty,
  channelTargets = M.empty,
  resourceSources = M.empty
}

runGenArchitecture ::
  TerminaProgArch SemanticAnn
  -> QualifiedName -> AnnotatedProgram SemanticAnn
  -> Either ArchitectureError (TerminaProgArch SemanticAnn)
runGenArchitecture tp modName elements =
  case flip ST.runState tp . runExceptT $ mapM_ (genArchElement modName) elements of
    (Left err, _) -> Left err
    (Right _, st) -> Right st

