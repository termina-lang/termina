-- | The facts that the transpiler derives from the deployment of a program
-- about each of its instances, as data that every output of the
-- documentation renders in its own words.
module Doc.Facts
  ( DocFact(..)
  , Endpoint(..)
  , ComponentKind(..)
  , instanceFacts
  ) where

import qualified Data.Map.Strict as M

import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils (genResourceLockings, getPriority, getStackSize, reachableResources)
import Core.AST (ClassKind(..), FieldAssignment'(..), Identifier, TInteger)
import Semantic.AST (Expression(..), TerminaType)

-- | A port of a component of the deployment: the component and its port.
data Endpoint = Endpoint Identifier Identifier
  deriving Show

-- | The kinds of components that a port connects to.
data ComponentKind
  = EmitterComponent
  | ChannelComponent
  | ResourceComponent
  | PoolComponent
  | AtomicComponent
  | AtomicArrayComponent
  | DequeComponent
  deriving (Show, Eq)

-- | A fact about an instance of the deployment.
data DocFact a
  = InstanceOf ClassKind Identifier -- ^ A task, handler or resource and its class
  | Priority TInteger -- ^ The priority of a task
  | StackSize TInteger -- ^ The size of the stack of a task, in bytes
  | ConnectedTo Identifier ComponentKind Identifier -- ^ A port of the instance and what it is connected to
  | ProtectedBy ResourceLock -- ^ How a resource or a pool is protected
  | UsedBy [Endpoint] -- ^ The ports connected to a resource, pool, atomic or deque
  | Reaches [Identifier] -- ^ The resources that a handler reaches through other resources
  | ChannelOf (TerminaType a) (Expression a) -- ^ The messages of a channel and its capacity
  | SentBy [Endpoint] -- ^ The ports that send through a channel
  | ReceivedBy Endpoint -- ^ The port that receives from a channel
  | PoolOf (TerminaType a) (Expression a) -- ^ The boxes of a pool and their number
  | AtomicOf (TerminaType a) (Maybe (Expression a)) -- ^ The value of an atomic, and the size of an atomic array
  | DequeOf (TerminaType a) (Expression a) -- ^ The boxes of a deque and its capacity
  | PeriodicTimerOf (Expression a) -- ^ The period of a periodic timer
  | InterruptOf -- ^ An emitter of an interrupt
  | Triggers Endpoint -- ^ The port that an emitter triggers
  deriving Show

-- | The facts of each instance of a deployment, by its name, given the size
-- of the stack of a task without one.
instanceFacts :: Integer -> TerminaProgArch a -> M.Map Identifier [DocFact a]
instanceFacts defaultStack arch = M.unions
  [ M.map taskFacts (tasks arch)
  , M.map handlerFacts (handlers arch)
  , M.map resourceFacts (resources arch)
  , M.mapWithKey poolFacts (pools arch)
  , M.mapWithKey atomicFacts (atomics arch)
  , M.mapWithKey atomicArrayFacts (atomicArrays arch)
  , M.mapWithKey dequeFacts (deques arch)
  , M.mapWithKey channelFacts (channels arch)
  , M.mapWithKey emitterFacts (emitters arch)
  ]

  where

    lockings = genResourceLockings arch

    taskFacts tsk =
      [ InstanceOf TaskClass (taskClass tsk), Priority (getPriority tsk), StackSize (getStackSize defaultStack tsk) ]
      ++ ports EmitterComponent (taskSinkPortConns tsk)
      ++ ports ChannelComponent (taskInputPortConns tsk)
      ++ ports ChannelComponent (taskOutputPortConns tsk)
      ++ accessConnections (taskAPConnections tsk)

    handlerFacts hdl =
      let (port, emitter, _) = handlerSinkPortConn hdl in
      [ InstanceOf HandlerClass (handlerClass hdl), ConnectedTo port EmitterComponent emitter ]
      ++ ports ChannelComponent (handlerOutputPortConns hdl)
      ++ accessConnections (handlerAPConnections hdl)
      ++ [ Reaches reached
         | let direct = [ target | (target, _) <- M.elems (handlerAPConnections hdl) ]
               reached = filter (`notElem` direct) (reachableResources arch (handlerName hdl))
         , not (null reached) ]

    resourceFacts res =
      [ InstanceOf ResourceClass (resourceClass res) ]
      ++ protection (resourceName res)
      ++ users (resourceName res)
      ++ accessConnections (resAPConnections res)

    poolFacts name (TPPool _ ty size _ _) = PoolOf ty size : protection name ++ users name
    atomicFacts name (TPAtomic _ ty _ _) = AtomicOf ty Nothing : users name
    atomicArrayFacts name (TPAtomicArray _ ty size _ _) = AtomicOf ty (Just size) : users name
    dequeFacts name (TPDeque _ ty capacity _ _) = DequeOf ty capacity : users name

    channelFacts name (TPMsgQueue _ ty size _ _) =
      ChannelOf ty size
      : [ SentBy [ Endpoint component port | (component, port, _) <- senders ]
        | Just senders <- [M.lookup name (channelSources arch)] ]
      ++ [ ReceivedBy (Endpoint component port) | Just (component, port, _) <- [M.lookup name (channelTargets arch)] ]

    emitterFacts name emitter =
      kind ++ [ Triggers (Endpoint component port) | Just (component, port, _) <- [M.lookup name (emitterTargets arch)] ]

      where

        kind = case emitter of
          TPPeriodicTimerEmitter _ (StructInitializer fields _) _ _ ->
            [ PeriodicTimerOf period | FieldValueAssignment "period" period _ <- fields ]
          TPInterruptEmitter {} -> [InterruptOf]
          _ -> []

    ports kind conns = [ ConnectedTo port kind target | (port, (target, _)) <- M.toList conns ]
    accessConnections conns = [ ConnectedTo port (componentKind target) target | (port, (target, _)) <- M.toList conns ]
    protection name = [ ProtectedBy lock | Just lock <- [M.lookup name lockings] ]
    users name = [ UsedBy [ Endpoint component port | (component, port, _) <- sources ]
                 | Just sources <- [M.lookup name (resourceSources arch)], not (null sources) ]

    -- The kind of a component of the deployment that an access port reaches
    componentKind target
      | M.member target (pools arch) = PoolComponent
      | M.member target (atomics arch) = AtomicComponent
      | M.member target (atomicArrays arch) = AtomicArrayComponent
      | M.member target (deques arch) = DequeComponent
      | otherwise = ResourceComponent
