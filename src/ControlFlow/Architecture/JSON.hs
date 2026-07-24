{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Serialization of the program architecture ('TerminaProgArch') to JSON.
--
-- This module emits the /wiring/ view of the architecture: instances (tasks,
-- handlers, resources, pools, atomics, atomic arrays, channels and emitters),
-- their classes and modifiers, and the port-level connections between them. It
-- deliberately omits member-function bodies. From the semantic annotation of
-- each instance it keeps only the source location (start and end line/column),
-- so consumers can navigate from the diagram to the code; no AST is dumped, and
-- types, sizes and modifier expressions are rendered to text.
module ControlFlow.Architecture.JSON (genArchJSON, runArchJSONPrinter) where

import ControlFlow.Architecture.Types
import ControlFlow.BasicBlocks.AST
import Utils.Annotations (Location (..), Located (..), Annotated (..))
import Utils.Printer (showText)

import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Parsec.Pos (sourceColumn, sourceLine)

-- | Render a source 'Location' as @{"start": {...}, "end": {...}}@, or 'Null'
-- for elements without a source position (builtins, internals).
locToJSON :: Location -> Value
locToJSON (Position _ start end) =
    object ["start" .= posToJSON start, "end" .= posToJSON end]
  where
    posToJSON p = object ["line" .= sourceLine p, "column" .= sourceColumn p]
locToJSON _ = A.Null

-- | Source location of any annotated architecture element.
locOf :: (Annotated d, Located a) => d a -> Value
locOf = locToJSON . getLocation . getAnnotation

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

emitterToJSON :: Located a => TPEmitter a -> Value
emitterToJSON e@(TPInterruptEmitter _ _) =
    object ["kind" .= ("interrupt" :: T.Text), "loc" .= locOf e]
emitterToJSON e@(TPPeriodicTimerEmitter _ period modul _) =
    object
        [ "kind" .= ("periodic_timer" :: T.Text)
        , "period" .= showText period
        , "module" .= modul
        , "loc" .= locOf e
        ]
emitterToJSON e@(TPSystemInitEmitter _ _) =
    object ["kind" .= ("system_init" :: T.Text), "loc" .= locOf e]
emitterToJSON e@(TPSystemExceptEmitter _ _) =
    object ["kind" .= ("system_except" :: T.Text), "loc" .= locOf e]

taskToJSON :: Located a => TPTask a -> Value
taskToJSON tsk =
    object
        [ "class" .= taskClass tsk
        , "module" .= taskModule tsk
        , "modifiers" .= renderModifiers (taskModifiers tsk)
        , "inputPorts" .= portConnMap (taskInputPortConns tsk)
        , "sinkPorts" .= portConnMap (taskSinkPortConns tsk)
        , "outputPorts" .= portConnMap (taskOutputPortConns tsk)
        , "accessPorts" .= portConnMap (taskAPConnections tsk)
        , "loc" .= locOf tsk
        ]

handlerToJSON :: Located a => TPHandler a -> Value
handlerToJSON hdl =
    object
        [ "class" .= handlerClass hdl
        , "module" .= handlerModule hdl
        , "modifiers" .= renderModifiers (handlerModifiers hdl)
        , "sinkPort" .= sinkObj (handlerSinkPortConn hdl)
        , "outputPorts" .= portConnMap (handlerOutputPortConns hdl)
        , "accessPorts" .= portConnMap (handlerAPConnections hdl)
        , "loc" .= locOf hdl
        ]
  where
    sinkObj (port, source, _) = object ["port" .= port, "source" .= source]

resourceToJSON :: Located a => TPResource a -> Value
resourceToJSON res =
    object
        [ "class" .= resourceClass res
        , "module" .= resourceModule res
        , "modifiers" .= renderModifiers (resModifiers res)
        , "accessPorts" .= portConnMap (resAPConnections res)
        , "loc" .= locOf res
        ]

poolToJSON :: Located a => TPPool a -> Value
poolToJSON p@(TPPool _ ty size modul _) =
    object
        [ "type" .= showText ty
        , "size" .= showText size
        , "module" .= modul
        , "loc" .= locOf p
        ]

atomicToJSON :: Located a => TPAtomic a -> Value
atomicToJSON a@(TPAtomic _ ty modul _) =
    object ["type" .= showText ty, "module" .= modul, "loc" .= locOf a]

atomicArrayToJSON :: Located a => TPAtomicArray a -> Value
atomicArrayToJSON a@(TPAtomicArray _ ty size modul _) =
    object
        [ "type" .= showText ty
        , "size" .= showText size
        , "module" .= modul
        , "loc" .= locOf a
        ]

channelToJSON :: Located a => TPChannel a -> Value
channelToJSON ch@(TPMsgQueue _ ty size modul _) =
    object
        [ "kind" .= ("message_queue" :: T.Text)
        , "type" .= showText ty
        , "size" .= showText size
        , "module" .= modul
        , "loc" .= locOf ch
        ]

-- | Build the JSON 'Value' describing the wiring of the architecture.
genArchJSON :: Located a => TerminaProgArch a -> Value
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
runArchJSONPrinter :: Located a => TerminaProgArch a -> BL.ByteString
runArchJSONPrinter = encodePretty . genArchJSON
