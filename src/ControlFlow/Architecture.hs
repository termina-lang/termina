module ControlFlow.Architecture (
    runGenArchitecture, emptyTerminaProgArch, getGlobDeclModules,
    pruneUnconnectedSystemElements
) where

import ControlFlow.Architecture.Types
import Control.Monad
import Control.Monad.Except
import ControlFlow.Architecture.Errors
import qualified Control.Monad.State.Strict as ST
import Lowering.AST
import Data.Maybe
import ControlFlow.Architecture.Utils
import Semantic.Types
import Utils.Annotations
import qualified Data.Map.Strict as M
import ControlFlow.Architecture.BoxInOut

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
  case M.lookup channel channelSrcs of
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

genArchTypeDef :: SemanticAnn -> TypeDef SemanticAnn -> ArchitectureMonad ()
genArchTypeDef ann tydef@(Class TaskClass ident _ _ _) = do
  let members = getClassMembers tydef
  outBoxMap <-
    case runInOutClass tydef of 
      Left err -> throwError err
      Right boxMap -> return boxMap
  let tskCls = TPClass {
    classIdentifier = ident,
    classKind = TaskClass,
    classProvides = [],
    classActions = getActions members,
    classMethods = M.empty,
    classProcedures = M.empty,
    classViewers = getViewers members,
    classCases = M.empty,
    accessPorts = getAccessPorts members,
    inputPorts = getInputPorts members,
    sinkPorts = getSinkPorts members,
    outputPorts = getOutputPorts members,
    classBoxIOMaps = outBoxMap,
    classAnns = ann
  }
  ST.modify $ \tp ->
    tp {
      taskClasses = M.insert ident tskCls (taskClasses tp)
    }
genArchTypeDef ann tydef@(Class HandlerClass ident _ _ _) = do
  let members = getClassMembers tydef
  outBoxMap <-
    case runInOutClass tydef of 
      Left err -> throwError err
      Right boxMap -> return boxMap
  let hdlCls = TPClass {
    classIdentifier = ident,
    classKind = HandlerClass,
    classProvides = [],
    classActions = getActions members,
    classMethods = M.empty,
    classProcedures = M.empty,
    classViewers = getViewers members,
    classCases = M.empty,
    accessPorts = getAccessPorts members,
    inputPorts = M.empty,
    sinkPorts = getSinkPorts members,
    outputPorts = getOutputPorts members,
    classBoxIOMaps = outBoxMap,
    classAnns = ann
  }
  ST.modify $ \tp ->
    tp {
      handlerClasses = M.insert ident hdlCls (handlerClasses tp)
    }
genArchTypeDef ann tydef@(Class ResourceClass ident _ provides _) = do
  let members = getClassMembers tydef
  outBoxMap <-
    case runInOutClass tydef of 
      Left err -> throwError err
      Right boxMap -> return boxMap
  -- | Resources do not have forwarding actions
  let resCls = TPClass {
    classIdentifier = ident,
    classKind = ResourceClass,
    classProvides = provides,
    classActions = M.empty,
    classMethods = getMethods members,
    classProcedures = getProcedures members,
    classViewers = M.empty,
    classCases = M.empty,
    accessPorts = getAccessPorts members,
    inputPorts = M.empty,
    sinkPorts = M.empty,
    outputPorts = M.empty,
    classBoxIOMaps = outBoxMap,
    classAnns = ann
  } 
  ST.modify $ \tp ->
    tp {
      resourceClasses = M.insert ident resCls (resourceClasses tp)
    }
genArchTypeDef ann tydef@(Class TestClass ident _ _ _) = do
  let members = getClassMembers tydef
  outBoxMap <-
    case runInOutClass tydef of
      Left err -> throwError err
      Right boxMap -> return boxMap
  let testCls = TPClass {
    classIdentifier = ident,
    classKind = TestClass,
    classProvides = [],
    classActions = M.empty,
    classMethods = getMethods members,
    classProcedures = M.empty,
    classViewers = M.empty,
    classCases = getCases members,
    accessPorts = getAccessPorts members,
    inputPorts = M.empty,
    sinkPorts = M.empty,
    outputPorts = getOutputPorts members,
    classBoxIOMaps = outBoxMap,
    classAnns = ann
  }
  ST.modify $ \tp ->
    tp {
      testClasses = M.insert ident testCls (testClasses tp)
    }
genArchTypeDef _ _ = return ()

genArchGlobal :: QualifiedName -> Global SemanticAnn -> ArchitectureMonad ()
genArchGlobal _ (Const identifier ty expr _ ann) = do
  ST.modify $ \tp ->
    tp {
      globalConstants = M.insert identifier (TPGlobalConstant identifier ty expr ann) (globalConstants tp)
    }
-- | Const expressions have been already substituted by the type checker
-- | Nothing to do here
genArchGlobal _ (ConstExpr {}) = return ()
genArchGlobal modName (Emitter ident (TTestEmitter ety) _ _ ann) =
  ST.modify $ \tp ->
    tp {
      testEmitters = M.insert ident (TPTestEmitter ident ety modName ann) (testEmitters tp)
    }
genArchGlobal modName (Emitter ident emitterCls mExpr _ ann) = do
  case emitterCls of
    (TGlobal EmitterClass "Interrupt") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPInterruptEmitter ident ann) (emitters tp)
      }
    (TGlobal EmitterClass "PeriodicTimer") -> 
      case mExpr of
        Just expr -> ST.modify $ \tp ->
          tp {
            emitters = M.insert ident (TPPeriodicTimerEmitter ident expr modName ann) (emitters tp)
          }
        Nothing -> throwError $ annotateError Internal EMissingPeriodicTimerInitializer
    (TGlobal EmitterClass "SystemInit") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPSystemInitEmitter ident ann) (emitters tp)
      }
    (TGlobal EmitterClass "SystemExcept") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPSystemExceptEmitter ident ann) (emitters tp)
      }
    -- | Any other emitter class is not supported (this should not happen)
    _ -> throwError $ annotateError Internal EUnsupportedEmitterClass
genArchGlobal modName (Task ident (TGlobal TaskClass tcls) (Just (StructInitializer assignments _)) modifiers tann) = do
  tpClass <- ST.get >>= \tp -> return $ fromJust (M.lookup tcls (taskClasses tp))
  (inpConns, sinkConns, outpConns, apConns) <- foldM (\(inp, sink, outp, accp) assignment ->
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case M.lookup pname (inputPorts tpClass) of
          Just _ -> do
            connectedTargets <- channelTargets <$> ST.get
            -- | Check if the target channel is already connected to an in port
            case M.lookup target connectedTargets of
              Nothing -> do
                addChannelTarget target ident pname cann
                return (M.insert pname (target, cann) inp, sink, outp, accp)
              Just (_, _, prevcann) ->
                throwError $ annotateError (getLocation cann) (EDuplicatedChannelConnection target (getLocation prevcann))
          Nothing -> 
            case M.lookup pname (sinkPorts tpClass) of
              Just _ -> do
                connectedEmitters <- emitterTargets <$> ST.get
                -- | Check if the target emitter is already connected to a sink port
                case M.lookup target connectedEmitters of
                  Nothing -> do
                    ST.modify $ \tp ->
                      tp {
                        emitterTargets = M.insert target (ident, pname, cann) connectedEmitters
                      }
                    return (inp, M.insert pname (target, cann) sink, outp, accp)
                  Just (_, _, prevcann) ->
                    throwError $ annotateError (getLocation cann) (EDuplicatedEmitterConnection target (getLocation prevcann))
              _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann ->
        case M.lookup pname (outputPorts tpClass) of
          Just _ -> do
            addChannelSource ident pname target cann
            return (inp, sink, M.insert pname (target, cann) outp, accp)
          Nothing -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann -> do
        addResourceSource target ident pname cann
        return (inp, sink, outp, M.insert pname (target, cann) accp)
      _ -> return (inp, sink, outp, accp)
    ) (M.empty, M.empty, M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      tasks = M.insert ident (TPTask ident tcls inpConns sinkConns outpConns apConns modifiers modName tann) (tasks tp)
    }
-- | Task declaration without struct initializer or a proper type specifier
-- This should not happen, since a task must define at least one inbound port
genArchGlobal _ (Task {}) = error "Internal error: invalid task declaration"
genArchGlobal modName (Resource ident (TGlobal ResourceClass rcls) initializer modifiers rann) =
  case initializer of
    Nothing -> ST.modify $ \tp ->
      tp {
        resources = M.insert ident (TPResource ident rcls M.empty modifiers (Just modName) rann) (resources tp)
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
          resources = M.insert ident (TPResource ident rcls apConns modifiers (Just modName) rann) (resources tp)
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
genArchGlobal modName (Resource ident (TDeque aty size) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      deques = M.insert ident (TPDeque ident aty size modName rann) (deques tp)
    }
genArchGlobal modName (Resource ident (TTestPool aty size) _ _ rann) =
  ST.modify $ \tp ->
    tp {
      testPools = M.insert ident (TPPool ident aty size modName rann) (testPools tp)
    }
genArchGlobal _ (Resource {}) = error "Internal error: invalid resource declaration"
genArchGlobal modName (Handler ident (TGlobal HandlerClass hcls) (Just (StructInitializer assignments _)) modifiers hann) = do
  tpClass <- ST.get >>= \tp -> return $ fromJust (M.lookup hcls (handlerClasses tp))
  (sinkConn, outpConns, apConns) <- foldM (\(sink, outp, accp) assignment -> do
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case M.lookup pname (sinkPorts tpClass) of
          Just _ -> do
            connectedEmitters <- emitterTargets <$> ST.get
            -- | Check if the target emitter is already connected to a sink port
            case M.lookup target connectedEmitters of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    emitterTargets = M.insert target (ident, pname, cann) connectedEmitters
                  }
                return (Just (pname, target, cann), outp, accp)
              Just (_, _, prevcann) ->
                throwError $ annotateError (getLocation cann) (EDuplicatedEmitterConnection target (getLocation prevcann))
          Nothing -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann ->
        case M.lookup pname (outputPorts tpClass) of
          Just _ -> do
            addChannelSource ident pname target cann
            return (sink, M.insert pname (target, cann) outp, accp)
          Nothing -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann  -> do
        addResourceSource target ident pname cann
        return (sink, outp, M.insert pname (target, cann) accp)
      _ -> return (sink, outp, accp)
    ) (Nothing, M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      handlers = M.insert ident (TPHandler ident hcls (fromJust sinkConn) outpConns apConns modifiers modName hann) (handlers tp)
    }
-- | Handler declaration without struct initializer or a proper type specifier
-- This should not happen, since a handler must define one sink port
genArchGlobal _ (Handler {}) = error "Internal error: invalid handler declaration"
genArchGlobal modName (Channel ident (TMsgQueue mty size) _ _ cann) =
  ST.modify $ \tp ->
    tp {
      channels = M.insert ident (TPMsgQueue ident mty size modName cann) (channels tp)
    }
-- | A channel of a unit test is a message queue that a case reads
genArchGlobal modName (Channel ident (TTestChannel mty size) _ _ cann) =
  ST.modify $ \tp ->
    tp {
      testChannels = M.insert ident (TPMsgQueue ident mty size modName cann) (testChannels tp)
    }
genArchGlobal _ (Channel {}) = error "Internal error: invalid channel declaration"
genArchGlobal modName (Test ident (TGlobal TestClass tcls) initializer _ tann) = do
  tpClass <- ST.get >>= \tp -> return $ fromJust (M.lookup tcls (testClasses tp))
  -- | A test class without ports has an instance without initializer
  let assignments = case initializer of
        Just (StructInitializer as _) -> as
        Just _ -> error "Internal error: test initializer is not a struct initializer"
        Nothing -> []
  (outpConns, apConns) <- foldM (\(outp, accp) assignment ->
    case assignment of
      FieldPortConnection OutboundPortConnection pname target cann ->
        case M.lookup pname (outputPorts tpClass) of
          Just _ -> do
            addChannelSource ident pname target cann
            return (M.insert pname (target, cann) outp, accp)
          Nothing -> error $ "Internal error: port " ++ pname ++ " is not an out port"
      FieldPortConnection AccessPortConnection pname target cann -> do
        addResourceSource target ident pname cann
        return (outp, M.insert pname (target, cann) accp)
      _ -> return (outp, accp)
    ) (M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      testInstances = M.insert ident (TPTest ident tcls outpConns apConns modName tann) (testInstances tp)
    }
genArchGlobal _ (Test {}) = error "Internal error: invalid test declaration"

genArchElement :: QualifiedName -> AnnASTElement SemanticAnn -> ArchitectureMonad ()
genArchElement _ (Function identifier params mRet block _ ann) = 
  ST.modify $ \tp ->
    tp {
      functions = M.insert identifier (TPFunction identifier params mRet block ann) (functions tp)
    }
-- | An extern function has no body for the architecture to keep
genArchElement _ (ExternFunction {}) = return ()
genArchElement modName (GlobalDeclaration glb) = genArchGlobal modName glb
genArchElement _ (TypeDefinition typeDef ann) = genArchTypeDef ann typeDef

emptyTerminaProgArch :: TerminaProgArch SemanticAnn
emptyTerminaProgArch = TerminaProgArch {
  emitters = M.fromList [
    ("system_init", TPSystemInitEmitter "system_init" (SemanticAnn (GTy (TGlobal EmitterClass "SystemInit")) Internal)),
    ("system_except", TPSystemExceptEmitter "system_except" (SemanticAnn (GTy (TGlobal EmitterClass "SystemExcept")) Internal))
  ],
  functions = M.empty,
  globalConstants = M.empty,
  emitterTargets = M.empty,
  taskClasses = M.empty,
  tasks = M.empty,
  handlerClasses = M.empty,
  handlers = M.empty,
  resourceClasses = M.empty,
  resources = M.fromList [
    ("system_entry", TPResource "system_entry" "SystemEntry" M.empty [] Nothing (SemanticAnn (GTy (TGlobal ResourceClass "SystemEntry")) Internal))
  ],
  pools = M.empty,
  atomics = M.empty,
  atomicArrays = M.empty,
  deques = M.empty,
  channels = M.empty,
  channelSources = M.empty,
  channelTargets = M.empty,
  resourceSources = M.empty,
  testClasses = M.empty,
  testInstances = M.empty,
  testEmitters = M.empty,
  testChannels = M.empty,
  testPools = M.empty
}

-- | Every program starts with the emitters of the platform and of the system
-- and with the system resource. The ones the application does not connect are
-- dropped once all its modules have been read, so they are neither checked nor
-- generated.
pruneUnconnectedSystemElements :: TerminaProgArch a -> TerminaProgArch a
pruneUnconnectedSystemElements tp = tp {
    emitters = M.filterWithKey keepEmitter (emitters tp),
    resources = M.filterWithKey keepResource (resources tp)
  }

  where

    keepEmitter ident emitter = not (isSystemEmitter emitter) || ident `M.member` emitterTargets tp

    isSystemEmitter (TPInterruptEmitter {}) = True
    isSystemEmitter (TPSystemInitEmitter {}) = True
    isSystemEmitter (TPSystemExceptEmitter {}) = True
    isSystemEmitter (TPPeriodicTimerEmitter {}) = False

    keepResource ident _ = ident /= "system_entry" || ident `M.member` resourceSources tp

runGenArchitecture ::
  TerminaProgArch SemanticAnn
  -> QualifiedName -> AnnotatedProgram SemanticAnn
  -> Either ArchitectureError (TerminaProgArch SemanticAnn)
runGenArchitecture tp modName elements =
  case flip ST.runState tp . runExceptT $ mapM_ (genArchElement modName) elements of
    (Left err, _) -> Left err
    (Right _, st) -> Right st

