{-# LANGUAGE OverloadedStrings #-}

-- | Serialization of the program architecture ('TerminaProgArch') to JSON.
--
-- This module emits the /wiring/ view of the architecture: instances (tasks,
-- handlers, resources, pools, atomics, atomic arrays, channels and emitters),
-- their classes and modifiers, and the port-level connections between them. It
-- deliberately omits member-function bodies and the semantic annotations
-- carried by the architecture (the @a@ type parameter is ignored). Types,
-- sizes and modifier expressions are rendered to text, so no AST is dumped.
module ControlFlow.Architecture.JSON (genArchJSON, runArchJSONPrinter) where

import ControlFlow.Architecture.Types
import ControlFlow.BasicBlocks.AST
import Utils.Printer (showText)

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Render a single modifier as text, e.g. @priority(5)@ or @unprotected@.
renderModifier :: Modifier a -> T.Text
renderModifier (Modifier name Nothing) = T.pack name
renderModifier (Modifier name (Just e)) = T.pack name <> "(" <> showText e <> ")"

renderModifiers :: [Modifier a] -> Value
renderModifiers = toJSON . fmap renderModifier

-- | A map from port name to the single element it is connected to.
portConnMap :: M.Map Identifier (Identifier, a) -> Value
portConnMap = toJSON . M.map fst

-- | An (entity, port) endpoint, dropping the annotation.
endpoint :: (Identifier, Identifier, a) -> Value
endpoint (entity, port, _) = object ["entity" .= entity, "port" .= port]

emitterToJSON :: TPEmitter a -> Value
emitterToJSON (TPInterruptEmitter _ _) =
    object ["kind" .= ("interrupt" :: T.Text)]
emitterToJSON (TPPeriodicTimerEmitter _ period modul _) =
    object
        [ "kind" .= ("periodic_timer" :: T.Text)
        , "period" .= showText period
        , "module" .= modul
        ]
emitterToJSON (TPSystemInitEmitter _ _) =
    object ["kind" .= ("system_init" :: T.Text)]
emitterToJSON (TPSystemExceptEmitter _ _) =
    object ["kind" .= ("system_except" :: T.Text)]

taskToJSON :: TPTask a -> Value
taskToJSON tsk =
    object
        [ "class" .= taskClass tsk
        , "module" .= taskModule tsk
        , "modifiers" .= renderModifiers (taskModifiers tsk)
        , "inputPorts" .= portConnMap (taskInputPortConns tsk)
        , "sinkPorts" .= portConnMap (taskSinkPortConns tsk)
        , "outputPorts" .= portConnMap (taskOutputPortConns tsk)
        , "accessPorts" .= portConnMap (taskAPConnections tsk)
        ]

handlerToJSON :: TPHandler a -> Value
handlerToJSON hdl =
    object
        [ "class" .= handlerClass hdl
        , "module" .= handlerModule hdl
        , "modifiers" .= renderModifiers (handlerModifiers hdl)
        , "sinkPort" .= sinkObj (handlerSinkPortConn hdl)
        , "outputPorts" .= portConnMap (handlerOutputPortConns hdl)
        , "accessPorts" .= portConnMap (handlerAPConnections hdl)
        ]
  where
    sinkObj (port, source, _) = object ["port" .= port, "source" .= source]

resourceToJSON :: TPResource a -> Value
resourceToJSON res =
    object
        [ "class" .= resourceClass res
        , "module" .= resourceModule res
        , "modifiers" .= renderModifiers (resModifiers res)
        , "accessPorts" .= portConnMap (resAPConnections res)
        ]

poolToJSON :: TPPool a -> Value
poolToJSON (TPPool _ ty size modul _) =
    object ["type" .= showText ty, "size" .= showText size, "module" .= modul]

atomicToJSON :: TPAtomic a -> Value
atomicToJSON (TPAtomic _ ty modul _) =
    object ["type" .= showText ty, "module" .= modul]

atomicArrayToJSON :: TPAtomicArray a -> Value
atomicArrayToJSON (TPAtomicArray _ ty size modul _) =
    object ["type" .= showText ty, "size" .= showText size, "module" .= modul]

channelToJSON :: TPChannel a -> Value
channelToJSON (TPMsgQueue _ ty size modul _) =
    object
        [ "kind" .= ("message_queue" :: T.Text)
        , "type" .= showText ty
        , "size" .= showText size
        , "module" .= modul
        ]

-- | Build the JSON 'Value' describing the wiring of the architecture.
genArchJSON :: TerminaProgArch a -> Value
genArchJSON progArch =
    object
        [ "tasks" .= M.map taskToJSON (tasks progArch)
        , "handlers" .= M.map handlerToJSON (handlers progArch)
        , "resources" .= M.map resourceToJSON (resources progArch)
        , "pools" .= M.map poolToJSON (pools progArch)
        , "atomics" .= M.map atomicToJSON (atomics progArch)
        , "atomicArrays" .= M.map atomicArrayToJSON (atomicArrays progArch)
        , "channels" .= M.map channelToJSON (channels progArch)
        , "emitters" .= M.map emitterToJSON (emitters progArch)
        , "connections" .= object
            [ "emitterTargets" .= M.map endpoint (emitterTargets progArch)
            , "channelSources" .= M.map (fmap endpoint) (channelSources progArch)
            , "channelTargets" .= M.map endpoint (channelTargets progArch)
            , "resourceSources" .= M.map (fmap endpoint) (resourceSources progArch)
            ]
        ]

-- | Render the architecture as a pretty-printed JSON document.
runArchJSONPrinter :: TerminaProgArch a -> BL.ByteString
runArchJSONPrinter = encodePretty . genArchJSON
