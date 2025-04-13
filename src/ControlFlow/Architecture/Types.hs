module ControlFlow.Architecture.Types where

import Data.Map
import ControlFlow.BasicBlocks.AST
import Modules.Modules
import qualified Data.Set as S
import qualified Data.Map as M
import Utils.Annotations

-- This module contains the function thhat will be used to generate
-- map of the architecture of the program.

data TPFunction a = TPFunction
    Identifier -- ^ name of the member function
    [Parameter a] -- ^ list of parameters (possibly empty)
    (Maybe (TerminaType a)) -- ^ return type of the membber function
    (Block a) -- ^ statements block
    a -- ^ annotations
  deriving Show

data TPClass a = TPClass {

    -- | Name of the class
    classIdentifier :: Identifier,

    -- | Kind of the class
    -- It can be either a task, a handler, or a resource
    classKind :: ClassKind,
    
    -- | Class member functions
    classMemberFunctions :: M.Map Identifier (TPFunction a),

    -- | Map of the input ports of the task
    -- It maps the name of the port to the type of the data that is received by
    -- the port and the name of the action that is executed when a message is
    -- received from the port.
    inputPorts :: Map Identifier (TerminaType a, Identifier),

    -- | Map of the sink ports of the task
    -- It maps the name of the port to the type of the data that is received by
    -- the port and the name of the action that is executed when an event is
    -- received from the port.
    sinkPorts :: Map Identifier (TerminaType a, Identifier),

    -- | Map of the output ports of the task
    -- It maps the name of the port to the type of the data that is sent through
    -- the port.
    outputPorts :: Map Identifier (TerminaType a),

    -- | Map between the output ports that send a box and the port from which
    -- the box was originated.
    classBoxIOMaps :: BoxOutputInputMaps a,

    -- | Map between the actions and the output ports through which the action
    -- sends messages.
    actionForwardingMap :: ActionForwardingMap

} deriving Show

data TPTask a = TPTask {

    -- | Name of the task
    taskName :: Identifier,

    -- | Class of the task
    taskClass :: Identifier,

    -- | Map of the input ports of the task
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port.
    taskInputPortConns :: Map Identifier (Identifier, a),

    -- | Map of the sink ports of the task
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port.
    taskSinkPortConns :: Map Identifier (Identifier, a),

    -- | Map of the output ports of the task
    -- It maps the name of the port to the name of channel
    -- that is connected to the port.
    taskOutputPortConns :: Map Identifier (Identifier, a),

    -- | Map of the access ports of the task
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port.
    taskAPConnections :: Map Identifier (Identifier, a),

    -- | List of the modifiers of the task
    taskModifiers :: [Modifier a],

    -- | Name of the module that instantiates the task
    taskModule :: QualifiedName,

    taskAnns :: a -- ^ semantic annontations
    
} deriving Show

instance Annotated TPTask where
  getAnnotation = taskAnns
  updateAnnotation t a = t { taskAnns = a }

data TPEmitter a = 
  TPInterruptEmittter 
    Identifier -- ^ emitter identifier
    a -- ^ annotations
  | TPPeriodicTimerEmitter 
    Identifier -- ^ emitter identifier
    QualifiedName -- ^ Module that instantiates the timer
    a -- ^ annotations
  | TPSystemInitEmitter
    Identifier -- ^ emitter identifier
    a -- ^ annotations
  deriving Show

instance Annotated TPEmitter where
  getAnnotation (TPInterruptEmittter _ a) = a
  getAnnotation (TPPeriodicTimerEmitter _ _ a) = a
  getAnnotation (TPSystemInitEmitter _ a) = a

  updateAnnotation (TPInterruptEmittter i _) = TPInterruptEmittter i
  updateAnnotation (TPPeriodicTimerEmitter i m _) = TPPeriodicTimerEmitter i m
  updateAnnotation (TPSystemInitEmitter i _) = TPSystemInitEmitter i

data TPResource a = TPResource {

    -- | Name of the resource
    resourceName :: Identifier,

    -- | Class of the resource
    resourceClass :: Identifier,

    -- | Map of the access ports of the resource
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port.
    resAPConnections :: Map Identifier (Identifier, a),

    -- | Name of the module that instantiates the resource or 
    -- Nothing if it is an internal resource
    resourceModule :: Maybe QualifiedName,

    resourceAnns :: a -- ^ annotations

} deriving Show

instance Annotated TPResource where
  getAnnotation = resourceAnns
  updateAnnotation r a = r { resourceAnns = a }

data TPHandler a = TPHandler {
    
    -- | Name of the handler
    handlerName :: Identifier,

    -- | Class of the handler
    handlerClass :: Identifier,
    
    -- | Map of the input ports of the handler
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port. It also conntains the
    -- annotations associated with the port connection assignment.
    handlerSinkPortConn :: (Identifier, Identifier, a),

    -- | Map of the output ports of the handler
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port. It also conntains the
    -- annotations associated with the port connection assignment.
    handlerOutputPortConns :: Map Identifier (Identifier, a),

    -- | Map of the access ports of the handler
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port. It also conntains the
    -- annotations associated with the port connection assignment.
    handlerAPConnections :: Map Identifier (Identifier, a),

    -- | List of the modifiers of the handler
    handlerModifiers :: [Modifier a],

    -- | Name of the module that instantiates the handler
    handlerModule :: QualifiedName,

    -- | Annotations associated with the handler
    handlerAnns :: a

} deriving Show

instance Annotated TPHandler where
  getAnnotation = handlerAnns
  updateAnnotation h a = h { handlerAnns = a }

data TPAtomic a = TPAtomic 
    Identifier -- ^ atomic identifier
    (TerminaType a) -- ^ data type specifier
    QualifiedName -- ^ module that instantiates the atomic
    a -- ^ annontations
  deriving Show

instance Annotated TPAtomic where
  getAnnotation (TPAtomic _ _ _ a) = a
  updateAnnotation (TPAtomic i t m _) = TPAtomic i t m

data TPAtomicArray a = TPAtomicArray 
    Identifier -- ^ atomic array identifier
    (TerminaType a) -- ^ data type specifier
    (Expression a) -- ^ size of the array
    QualifiedName -- ^ module that instantiates the atomic array
    a -- ^ annontations
  deriving Show

instance Annotated TPAtomicArray where
  getAnnotation (TPAtomicArray _ _ _ _ a) = a
  updateAnnotation (TPAtomicArray i t s m _) = TPAtomicArray i t s m

data TPChannel a = TPMsgQueue
    Identifier -- ^ message queue identifier
    (TerminaType a) -- ^ data type specifier
    (Expression a) -- ^ size of the message queue
    QualifiedName -- ^ module that instantiates the message queue
    a -- ^ annontations
   deriving Show
  
instance Annotated TPChannel where
  getAnnotation (TPMsgQueue _ _ _ _ a) = a
  updateAnnotation (TPMsgQueue i t s m _) = TPMsgQueue i t s m

data TPPool a = TPPool 
    Identifier -- ^ pool identifier
    (TerminaType a) -- ^ data type specifier
    (Expression a) -- ^ size of the pool
    QualifiedName -- ^ module that instantiates the pool
    a -- ^ annontations
   deriving Show

instance Annotated TPPool where
  getAnnotation (TPPool _ _ _ _ a) = a
  updateAnnotation (TPPool i t s m _) = TPPool i t s m

data TPGlobalConstant a = TPGlobalConstant {

    -- | Name of the global constant
    constantName :: Identifier,

    -- | Type of the global constant
    constantType :: TerminaType a,

    -- | Value of the global constant
    constantValue :: Expression a,

    -- | Annotations associated with the global constant
    constantAnn :: a

} deriving Show

data TerminaProgArch a = TerminaProgArch {

    -- | Map of all the functions in the program
    functions :: Map Identifier (TPFunction a),

    -- | Map of all the global constants in the program
    globalConstants :: Map Identifier (TPGlobalConstant a),

    -- Map of all the event emitters in the program
    emitters :: Map Identifier (TPEmitter a),

    -- | Map of all the connected event emitters It maps the name of the emitter
    -- to the name of the task or handler and the name of the port that is
    -- connected to the emitter.
    emitterTargets :: Map Identifier (Identifier, Identifier, a),

    taskClasses :: Map Identifier (TPClass a),
    tasks :: Map Identifier (TPTask a),

    handlerClasses :: Map Identifier (TPClass a),
    handlers :: Map Identifier (TPHandler a),

    resourceClasses :: Map Identifier (TPClass a),

    resources :: Map Identifier (TPResource a),

    pools :: Map Identifier (TPPool a),
    atomics :: Map Identifier (TPAtomic a),
    atomicArrays :: Map Identifier (TPAtomicArray a),

    -- | Map of all the task classes in the program
    channels :: Map Identifier (TPChannel a),

    -- | Map the sources of all the connected channels
    channelSources :: Map Identifier [(Identifier, Identifier, a)],

    -- | Map of all the connected channels
    -- It maps the name of the channel to the name of the task and the
    -- name of the port that is connected to the channel.
    channelTargets :: Map Identifier (Identifier, Identifier, a),

    -- | Map of all the connected resources
    -- It maps the name of the resource to the name of the task, handler or
    -- resource that is connected to the resource and the name of the port that
    -- is used to access the resource.
    resourceSources :: Map Identifier [(Identifier, Identifier, a)]

} deriving Show

data InOptionBox a =
  InOptionBoxAlloc Identifier a
  | InOptionBoxProcedureCall Identifier Integer a

data InBox a =
  InBoxInput Identifier
  | InBoxAlloc Identifier a
  | InBoxProcedureCall Identifier Integer
  deriving (Show, Eq, Ord)

data BoxOutputInputMaps a = BoxOutputInputMaps {

  -- | Map between the box parameters of a procedure call and the port from 
  -- which the box was originated. The key is a tuple with the name of the
  -- port, the name of the procedure and the index of the box parameter.
  outBoxProcedureCall :: Map (Identifier, Identifier, Integer) [(a, InBox a)],

  -- | Map between the output ports that send a box and the port from which the
  -- box was originated.
  outBoxSend :: Map Identifier [(a, InBox a)],

  -- | Map between the allocator ports that are used to free a box and the
  -- port from which the box was originated.
  outBoxFree :: Map Identifier [(a, InBox a)]

} deriving Show

data BoxInOutState a = BoxInOutState {
    inBoxMap :: M.Map Identifier (InBox a),
    inOptionBoxMap :: M.Map Identifier (InOptionBox a),
    outputInputMaps :: BoxOutputInputMaps a
}

-- | Map between the action identifiers and the set of out ports through which
-- the action sends messages.
type ActionForwardingMap = Map Identifier (S.Set Identifier)