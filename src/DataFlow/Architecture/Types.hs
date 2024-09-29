module DataFlow.Architecture.Types where

import Data.Map
import ControlFlow.AST
import Semantic.Types
import Modules.Modules

-- This module contains the function thhat will be used to generate
-- map of the architecture of the program.

data TPTask a = TPTask {

    -- | Name of the task
    taskName :: Identifier,

    -- | Class of the task
    taskClass :: Identifier,

    -- | Map of the input ports of the task
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port.
    taskInputPortConns :: Map Identifier (TerminaType, Identifier, a),

    -- | Map of the sink ports of the task
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port.
    taskSinkPortConns :: Map Identifier (TerminaType, Identifier, a),

    -- | Map of the output ports of the task
    -- It maps the name of the port to the name of channel
    -- that is connected to the port.
    taskOutputPortConns :: Map Identifier (TerminaType, Identifier, a),

    -- | Map of the access ports of the task
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port.
    taskAPConnections :: Map Identifier (Identifier, a),

    -- | List of the modifiers of the task
    taskModifiers :: [Modifier],

    -- | Name of the module that instantiates the task
    taskModule :: QualifiedName,

    taskAnns :: a -- ^ semantic annontations
    
} deriving Show

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

data TPResource a = TPResource {

    -- | Name of the resource
    resourceName :: Identifier,

    -- | Class of the resource
    resourceClass :: Identifier,

    -- | Map of the access ports of the resource
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port.
    resAPConnections :: Map Identifier (Identifier, a),

    -- | Name of the module that instantiates the resource
    resourceModule :: QualifiedName,

    resourceAnns :: a -- ^ annotations

} deriving Show

data TPHandler a = TPHandler {
    
    -- | Name of the handler
    handlerName :: Identifier,

    -- | Class of the handler
    handlerClass :: Identifier,
    
    -- | Map of the input ports of the handler
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port. It also conntains the
    -- annotations associated with the port connection assignment.
    handlerSinkPortConn :: (Identifier, TerminaType, Identifier, a),

    -- | Map of the output ports of the handler
    -- It maps the name of the port to the name of the channel
    -- that is connected to the port. It also conntains the
    -- annotations associated with the port connection assignment.
    handlerOutputPortConns :: Map Identifier (TerminaType, Identifier, a),

    -- | Map of the access ports of the handler
    -- It maps the name of the port to the name of the resource
    -- that is connected to the port. It also conntains the
    -- annotations associated with the port connection assignment.
    handlerAPConnections :: Map Identifier (Identifier, a),

    -- | List of the modifiers of the handler
    handlerModifiers :: [Modifier],

    -- | Name of the module that instantiates the handler
    handlerModule :: QualifiedName,

    -- | Annotations associated with the handler
    handlerAnns :: a

} deriving Show

data TPAtomic a = TPAtomic 
    Identifier -- ^ atomic identifier
    TerminaType -- ^ data type specifier
    QualifiedName -- ^ module that instantiates the atomic
    a -- ^ annontations
  deriving Show

data TPAtomicArray a = TPAtomicArray 
    Identifier -- ^ atomic array identifier
    TerminaType -- ^ data type specifier
    Size -- ^ size of the array
    QualifiedName -- ^ module that instantiates the atomic array
    a -- ^ annontations
  deriving Show

data TPChannel a = TPMsgQueue
    Identifier -- ^ message queue identifier
    TerminaType -- ^ data type specifier
    Size -- ^ size of the message queue
    QualifiedName -- ^ module that instantiates the message queue
    a -- ^ annontations
   deriving Show

data TPPool a = TPPool 
    Identifier -- ^ pool identifier
    TerminaType -- ^ data type specifier
    Size -- ^ size of the pool
    QualifiedName -- ^ module that instantiates the pool
    a -- ^ annontations
   deriving Show

data TerminaProgArch a = TerminaProgArch {

    -- Map of all the event emitters in the program
    emitters :: Map Identifier (TPEmitter a),

    -- | Map of all the connected event emitters It maps the name of the emitter
    -- to the name of the task or handler, the name of the port that is
    -- connected to the emitter, and the name of the action to be executed when 
    -- the event is emitted.
    emitterTargets :: Map Identifier (Identifier, Identifier, Identifier, a),

    taskClasses :: Map Identifier (TypeDef a),
    tasks :: Map Identifier (TPTask a),

    handlerClasses :: Map Identifier (TypeDef a),
    handlers :: Map Identifier (TPHandler a),

    resourceClasses :: Map Identifier (TypeDef SemanticAnn),

    resources :: Map Identifier (TPResource a),

    pools :: Map Identifier (TPPool a),
    atomics :: Map Identifier (TPAtomic a),
    atomicArrays :: Map Identifier (TPAtomicArray a),

    -- | Map of all the task classes in the program
    channels :: Map Identifier (TPChannel a),

    -- | Map of all the connected channels
    -- It maps the name of the channel to the name of the task, the
    -- name of the port that is connected to the channel, and the 
    -- name of the action to be executed when a message is received
    -- from the channel.
    channelTargets :: Map Identifier (Identifier, Identifier, Identifier, a)

} deriving Show