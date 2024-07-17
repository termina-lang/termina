module DataFlow.Program where

import DataFlow.Program.Types
import Control.Monad.Except
import DataFlow.Program.Errors.Errors
import qualified Control.Monad.State.Strict as ST
import AST.Seman
import Semantic.Monad
import qualified Data.Map as M
import Data.Maybe
import DataFlow.Program.Utils
import Parser.Parsing
import Semantic.Types

type ProgramMonad = ExceptT ProgramError (ST.State (TerminaProgram SemanticAnns))

getTaskClass :: Identifier -> ProgramMonad (TypeDef SemanticAnns)
getTaskClass ident = do
  fromJust . M.lookup ident . taskClasses <$> ST.get

getResourceClass :: Identifier -> ProgramMonad (TypeDef SemanticAnns)
getResourceClass ident = do
  fromJust . M.lookup ident . resourceClasses <$> ST.get

getHandlerClass :: Identifier -> ProgramMonad (TypeDef SemanticAnns)
getHandlerClass ident = do
  fromJust . M.lookup ident . handlerClasses <$> ST.get

typeDefinitionCheck :: TypeDef SemanticAnns -> ProgramMonad ()
typeDefinitionCheck tydef@(Class TaskClass ident _ _ _) = 
  ST.modify $ \tp ->
    tp {
      taskClasses = M.insert ident tydef (taskClasses tp)
    }
typeDefinitionCheck tydef@(Class HandlerClass ident _ _ _) = 
  ST.modify $ \tp ->
    tp {
      handlerClasses = M.insert ident tydef (handlerClasses tp)
    }
typeDefinitionCheck tydef@(Class ResourceClass ident _ _ _) = 
  ST.modify $ \tp ->
    tp {
      resourceClasses = M.insert ident tydef (resourceClasses tp)
    }
typeDefinitionCheck _ = return ()

globalCheck :: Global SemanticAnns -> ProgramMonad ()
globalCheck (Const {}) = return ()
globalCheck (Emitter ident emitterCls _ _ ann) = do
  case emitterCls of
    (DefinedType "Interrupt") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPInterruptEmittter ident ann) (emitters tp)
      }
    (DefinedType "PeriodicTimer") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPPeriodicTimerEmitter ident ann) (emitters tp)
       }
    (DefinedType "SystemInit") -> ST.modify $ \tp ->
      tp {
        emitters = M.insert ident (TPSystemInitEmitter ident ann) (emitters tp)
      }
    _ -> throwError $ annnotateError (location ann) (UnsupportedEmitterClass ident)
globalCheck (Task ident (DefinedType tcls) (Just (StructInitializer assignments _ _)) _ tann) = do
  members <- ST.get >>= \tp -> return $ getClassMembers (fromJust (M.lookup tcls (taskClasses tp)))
  (inpConns, sinkConns, outpConns, apConns) <- foldM (\(inp, sink, outp, accp) assignment ->
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann ->
        case getPortType pname members of
          InPort {} -> do
            connectedTargets <- channelTargets <$> ST.get
            -- | Check if the target channel is already connected to an in port
            case M.lookup target connectedTargets of
              Nothing -> do
                ST.modify $ \tp ->
                  tp {
                    channelTargets = M.insert target (ident, pname, cann) connectedTargets
                  }
                return (M.insert pname (target, cann) inp, sink, outp, accp)
              Just (_, _, prevcann) ->
                throwError $ annnotateError (location cann) (DuplicatedChannelConnection target (location prevcann))
          SinkPort {} -> do
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
                throwError $ annnotateError (location cann) (DuplicatedEmitterConnection target (location prevcann))
          _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann -> 
        return (inp, sink, M.insert pname (target, cann) outp, accp)
      FieldPortConnection AccessPortConnection pname target cann  -> 
        return (inp, sink, outp, M.insert pname (target, cann) accp)
      _ -> return (inp, sink, outp, accp)
    ) (M.empty, M.empty, M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      tasks = M.insert ident (TPTask ident tcls inpConns sinkConns outpConns apConns tann) (tasks tp)
    }
-- | Task declaration without struct initializer or a proper type specifier
-- This should not happen, since a task must define at least one inbound port
globalCheck (Task {}) = error "Internal error: invalid task declaration"
globalCheck (Resource ident (DefinedType rcls) initializer _ rann) = 
  case initializer of
    Nothing -> ST.modify $ \tp ->
      tp {
        resources = M.insert ident (TPResource ident rcls M.empty rann) (resources tp)
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
          resources = M.insert ident (TPResource ident rcls apConns rann) (resources tp)
        }
    -- | Resource initializer is not a struct initializer
    -- This should not happen, since the type checker must not allow initializing a
    -- resource with anything other than a struct initializer 
    Just _ -> error "Internal error: resource initializer is not a struct initializer"
globalCheck (Resource ident (Atomic aty) _ _ rann) = 
  ST.modify $ \tp ->
    tp {
      atomics = M.insert ident (TPAtomic ident aty rann)  (atomics tp)
    }
globalCheck (Resource ident (AtomicArray aty size) _ _ rann) = 
  ST.modify $ \tp ->
    tp {
      atomicArrays = M.insert ident (TPAtomicArray ident aty size rann) (atomicArrays tp)
    }
globalCheck (Resource ident (Pool aty size) _ _ rann) = 
  ST.modify $ \tp ->
    tp {
      pools = M.insert ident (TPPool ident aty size rann) (pools tp)
    }
globalCheck (Resource {}) = error "Internal error: invalid resource declaration"
globalCheck (Handler ident (DefinedType hcls) (Just (StructInitializer assignments _ _)) _ hann) = do
  members <- ST.get >>= \tp -> 
    case M.lookup hcls (handlerClasses tp) of
      Nothing -> error $ "Handler class: " ++ hcls ++ " not found"
      Just cls -> return $ getClassMembers cls
  (sinkConn, outpConns, apConns) <- foldM (\(sink, outp, accp) assignment -> do
    case assignment of
      FieldPortConnection InboundPortConnection pname target cann -> 
        case getPortType pname members of
          SinkPort {} -> do
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
                throwError $ annnotateError (location cann) (DuplicatedEmitterConnection target (location prevcann))
          _ -> error $ "Internal error: port " ++ pname ++ " is not a sink port or an in port"
      FieldPortConnection OutboundPortConnection pname target cann -> 
        return (sink, M.insert pname (target, cann) outp, accp)
      FieldPortConnection AccessPortConnection pname target cann  -> 
        return (sink, outp, M.insert pname (target, cann) accp)
      _ -> return (sink, outp, accp)
    ) (Nothing, M.empty, M.empty) assignments
  ST.modify $ \tp ->
    tp {
      handlers = M.insert ident (TPHandler ident hcls (fromJust sinkConn) outpConns apConns hann) (handlers tp)
    }
-- | Handler declaration without struct initializer or a proper type specifier
-- This should not happen, since a handler must define one sink port
globalCheck (Handler {}) = error "Internal error: invalid handler declaration"
globalCheck (Channel ident (MsgQueue mty size) _ _ cann) = 
  ST.modify $ \tp ->
    tp {
      channels = M.insert ident (TPMsgQueue ident mty size cann) (channels tp)
    }
globalCheck (Channel {}) = error "Internal error: invalid channel declaration"

elementCheck :: AnnASTElement SemanticAnns -> ProgramMonad ()
elementCheck (Function {}) = return ()
elementCheck (GlobalDeclaration glb) = globalCheck glb
elementCheck (TypeDefinition typeDef _) = typeDefinitionCheck typeDef

emptyTerminaProgram :: TerminaProgram SemanticAnns
emptyTerminaProgram = TerminaProgram {
  emitters = M.fromList [
    ("system_init", TPSystemInitEmitter "system_init" (SemAnn Internal (GTy (GGlob (SEmitter (DefinedType "SystemInit"))))))
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

runArchitectureCheck :: 
  TerminaProgram SemanticAnns 
  -> AnnotatedProgram SemanticAnns 
  -> Either ProgramError (TerminaProgram SemanticAnns)
runArchitectureCheck tp elements =
  case flip ST.runState tp . runExceptT $ mapM_ elementCheck elements of
    (Left err, _) -> Left err
    (Right _, st) -> Right st


