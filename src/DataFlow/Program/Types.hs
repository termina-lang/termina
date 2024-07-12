module DataFlow.Program.Types where

import Data.Map
import AST.Seman
import Semantic.Monad

-- This module contains the function thhat will be used to generate
-- map of the architecture of the program.

data TPSinkPort = TPSinkPort 
    Identifier -- ^ port identifier
    TypeSpecifier -- ^ data type specifier
    Identifier -- ^ action to be executed
  deriving Show

data TPInputPort = TPInputPort
    Identifier -- ^ port identifier
    TypeSpecifier -- ^ data type specifier
    Identifier -- ^ action to be executed
  deriving Show

data TPTask = TPTask {

    -- | Name of the task
    taskName :: Identifier,

    -- | Class of the task
    taskClass :: Identifier,

    -- | Map of the input ports of the task
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port.
    taskInputPortConns :: Map Identifier (TPInputPort, SemanticAnns),

    -- | Map of the output ports of the task
    -- It maps the name of the port to the name of channel
    -- that is connected to the port.
    taskOutputPortConns :: Map Identifier (Identifier, SemanticAnns),

    -- | Map of the sink ports of the task
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port.
    taskSinkPortConns :: Map Identifier (TPSinkPort, SemanticAnns),

    -- | Map of the access ports of the task
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port.
    taskAPConnections :: Map Identifier (Identifier, SemanticAnns),

    taskSemAnns :: SemanticAnns -- ^ semantic annontations
    
} deriving Show

data TPEmitter = 
  TPInterruptEmittter 
    Identifier -- ^ emitter identifier
  | TPPeriodicTimerEmitter 
    Identifier -- ^ emitter identifier
  | TPSystemInitEmitter
    Identifier -- ^ emitter identifier
  deriving Show

data TPResource = TPResource {

    -- | Name of the resource
    resourceName :: Identifier,

    -- | Class of the resource
    resourceClass :: Identifier,

    -- | Map of the access ports of the resource
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port.
    resAPConnections :: Map Identifier (Identifier, SemanticAnns),

    resourceSemAnns :: SemanticAnns -- ^ semantic annontations

} deriving Show

data TPHandler = TPHandler {
    handlerName :: Identifier,
    handlerClass :: Identifier,
    handlerSinkPortConn :: (TPSinkPort, SemanticAnns),
    handlerOutputPortConns :: Map Identifier (Identifier, SemanticAnns),
    handlerAPConnections :: Map Identifier (Identifier, SemanticAnns),
    handlerSemAnns :: SemanticAnns
} deriving Show

data TPAtomic = TPAtomic 
    Identifier -- ^ atomic identifier
    TypeSpecifier -- ^ data type specifier
  deriving Show

data TPAtomicArray = TPAtomicArray 
    Identifier -- ^ atomic array identifier
    TypeSpecifier -- ^ data type specifier
    Size -- ^ size of the array
    SemanticAnns -- ^ semantic annontations
  deriving Show

data TPChannel = TPMsgQueue
    Identifier -- ^ message queue identifier
    TypeSpecifier -- ^ data type specifier
    Size -- ^ size of the message queue
    SemanticAnns -- ^ semantic annontations
   deriving Show

data TPPool = TPPool 
    Identifier -- ^ pool identifier
    TypeSpecifier -- ^ data type specifier
    Size -- ^ size of the pool
    SemanticAnns -- ^ semantic annontations
   deriving Show

data TPTaskClass = TPTaskClass {

    taskClassName :: Identifier,

    -- | Map of the input ports of the task class
    -- It maps the name of the port to the name of the action
    -- that is triggered when it receives a message through the port.
    taskInputPorts :: Map Identifier (TypeSpecifier, Identifier),

    -- | Map of the event sink ports of the task class
    -- It maps the name of the port to the name of the action
    -- that is triggered when the event source connected to the port
    -- emits an event.
    taskSinkPorts :: Map Identifier (TypeSpecifier, Identifier),

    taskOutputPorts :: Map Identifier TypeSpecifier,

    taskAccessPorts :: Map Identifier TypeSpecifier

    -- In the future, we may need to include the full type definition

} deriving Show

data TPResourceClass = TPResourceClass {

    resClassName :: Identifier,

    resAccessPorts :: Map Identifier TypeSpecifier

} deriving Show

data TPHandlerClass = TPHandlerClass {

    handlerClassName :: Identifier,

    handlerSinkPort :: (TypeSpecifier, Identifier),

    handlerOutputPorts :: Map Identifier TypeSpecifier,

    handlerAccessPorts :: Map Identifier TypeSpecifier
  
} deriving Show

data TerminaProgram = TerminaProgram {

    -- Map of all the event emitters in the program
    emitters :: Map Identifier TPEmitter,

    -- | Map of all the connected event emitters
    -- It maps the name of the emitter to the name of the task or
    -- handler and the name of the port that is connected to the emitter.
    emitterTargets :: Map Identifier (Identifier, Identifier),

    taskClasses :: Map Identifier TPTaskClass,
    tasks :: Map Identifier TPTask,

    handlerClasses :: Map Identifier TPHandlerClass,
    handlers :: Map Identifier TPHandler,

    resourceClasses :: Map Identifier TPResourceClass,
    resources :: Map Identifier TPResource,

    pools :: Map Identifier TPPool,
    atomics :: Map Identifier TPAtomic,
    atomicArrays :: Map Identifier TPAtomicArray,

    -- | Map of all the task classes in the program
    channnels :: Map Identifier TPChannel,

    -- | Map of all the connected channels
    -- It maps the name of the channel to the name of the task and the
    -- name of the port that is connected to the channel.
    channelTargets :: Map Identifier (Identifier, Identifier)

} deriving Show