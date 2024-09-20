module DataFlow.Architecture.Types where

import Data.Map
import Semantic.AST
import Semantic.Types

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

    taskAnns :: a -- ^ semantic annontations
    
} deriving Show

data TPEmitter a = 
  TPInterruptEmittter 
    Identifier -- ^ emitter identifier
    a -- ^ annotations
  | TPPeriodicTimerEmitter 
    Identifier -- ^ emitter identifier
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

    -- | Annotations associated with the handler
    handlerAnns :: a

} deriving Show

data TPAtomic a = TPAtomic 
    Identifier -- ^ atomic identifier
    TypeSpecifier -- ^ data type specifier
    a -- ^ annontations
  deriving Show

data TPAtomicArray a = TPAtomicArray 
    Identifier -- ^ atomic array identifier
    TypeSpecifier -- ^ data type specifier
    Size -- ^ size of the array
    a -- ^ annontations
  deriving Show

data TPChannel a = TPMsgQueue
    Identifier -- ^ message queue identifier
    TypeSpecifier -- ^ data type specifier
    Size -- ^ size of the message queue
    a -- ^ annontations
   deriving Show

data TPPool a = TPPool 
    Identifier -- ^ pool identifier
    TypeSpecifier -- ^ data type specifier
    Size -- ^ size of the pool
    a -- ^ annontations
   deriving Show

data TerminaProgArch a = TerminaProgArch {

    -- Map of all the event emitters in the program
    emitters :: Map Identifier (TPEmitter a),

    -- | Map of all the connected event emitters
    -- It maps the name of the emitter to the name of the task or
    -- handler and the name of the port that is connected to the emitter.
    emitterTargets :: Map Identifier (Identifier, Identifier, a),

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
    -- It maps the name of the channel to the name of the task and the
    -- name of the port that is connected to the channel.
    channelTargets :: Map Identifier (Identifier, Identifier, a)

} deriving Show