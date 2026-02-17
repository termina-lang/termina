
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.Architecture.PlantUML (runPlantUMLCmpGenerator, CmpDiagramRange(..)) where

import Extras.PlantUML.AST
import ControlFlow.Architecture.Types
import qualified Data.Map.Strict as M
import Core.AST
import qualified Data.Text as T
import Control.Monad.Except
import qualified Control.Monad.State as ST
import Utils.Printer
import Control.Monad

data CmpDiagramRange = CmpDiagramRange
  {
    items :: [Identifier]
    , highestDepth :: Maybe Int
  } deriving Show

data PlantUMLCmpGenState a = PlantUMLCmpGenState
    {
        arch :: TerminaProgArch a,
        range :: Maybe CmpDiagramRange,
        participants :: M.Map Identifier PlantUMLCmpParticipant
        , connections :: M.Map (Identifier, Identifier) PlantUMLCmpDiagramConnection
    } deriving Show

type PlantUMLCmpGenMonad a = ExceptT T.Text (ST.State (PlantUMLCmpGenState a))

genEmitter :: TPEmitter a -> PlantUMLCmpGenMonad a ()
genEmitter (TPInterruptEmitter emitterId _) = do
    cmps <- ST.gets participants
    if M.member emitterId cmps then
        return ()
    else do
        progArch <- ST.gets arch
        let emitterBoundary = PlantUMLCmpBoundary emitterId (Just $ T.pack "Interrupt") (Just $ PlantUMLColorName "Red")
        ST.modify $ \s -> s {
            participants = M.insert emitterId emitterBoundary (participants s)
        }
        case M.lookup emitterId (emitterTargets progArch) of
            Nothing -> throwError $ "Emitter " <> T.pack emitterId <> " has no target."
            Just (target, _, _) -> do
                genElement target
                genEventConnection emitterId target
genEmitter (TPPeriodicTimerEmitter emitterId _ _ _) = do
    cmps <- ST.gets participants
    if M.member emitterId cmps then
        return ()
    else do
        progArch <- ST.gets arch
        let emitterBoundary = PlantUMLCmpBoundary emitterId (Just $ T.pack "PeriodicTimer") (Just $ PlantUMLColorName "Red")
        ST.modify $ \s -> s {
            participants = M.insert emitterId emitterBoundary (participants s)
        }
        case M.lookup emitterId (emitterTargets progArch) of
            Nothing -> throwError $ "Emitter " <> T.pack emitterId <> " has no target."
            Just (target, _, _) -> do
                genElement target
                genEventConnection emitterId target
genEmitter (TPSystemInitEmitter emitterId _ ) = do
    cmps <- ST.gets participants
    if M.member emitterId cmps then
        return ()
    else do
        progArch <- ST.gets arch
        let emitterBoundary = PlantUMLCmpBoundary emitterId (Just $ T.pack "SystemInit") (Just $ PlantUMLColorName "Red")
        ST.modify $ \s -> s {
            participants = M.insert emitterId emitterBoundary (participants s)
        }
        case M.lookup emitterId (emitterTargets progArch) of
            Nothing -> throwError $ "Emitter " <> T.pack emitterId <> " has no target."
            Just (target, _, _) -> do
                genElement target
                genEventConnection emitterId target
genEmitter (TPSystemExceptEmitter emitterId _ ) =
    throwError $ "SystemExcept emitter " <> T.pack emitterId <> " not supported in PlantUML generation."

genResourceAccesses :: Int -> Identifier -> [Identifier] -> PlantUMLCmpGenMonad a ()
genResourceAccesses currentDepth entityId apConns = do
    mRange <- ST.gets range
    case mRange of
        Nothing ->
            mapM_ (\targetId -> do
                genTargetResource currentDepth targetId
                genInterfaceConnection entityId targetId
            ) apConns
        Just (CmpDiagramRange _ Nothing) ->
            mapM_ (\targetId -> do
                genTargetResource currentDepth targetId
                genInterfaceConnection entityId targetId
            ) apConns
        Just (CmpDiagramRange _ (Just hDepth)) ->
            when (hDepth > currentDepth) $ mapM_ (\targetId -> do
                    genTargetResource currentDepth targetId
                    genInterfaceConnection entityId targetId
                ) apConns

genTask :: TPTask a -> PlantUMLCmpGenMonad a ()
genTask (TPTask taskId taskCls _ _ outPorts apConns _ _ _) = do
    cmps <- ST.gets participants
    if M.member taskId cmps then
        return ()
    else do
        progArch <- ST.gets arch
        let cmp = PlantUMLCmpAction taskId (Just . T.pack $ taskCls) (Just $ PlantUMLColorName "PeachPuff")
        -- | Insert the new participant
        ST.modify $ \s -> s {
            participants = M.insert taskId cmp (participants s)
        }
        -- | Now create connections to message queues
        mapM_ (\(_, (queueId, _)) -> do
            case M.lookup queueId (channels progArch) of
                Nothing -> throwError $ "Channel " <> T.pack queueId <> " not found."
                Just channel -> do
                    genChannel channel
                    genChannelConnection taskId queueId
            ) (M.toList outPorts)
        genResourceAccesses 0 taskId (fst <$> M.elems apConns)

genHandler :: TPHandler a -> PlantUMLCmpGenMonad a ()
genHandler (TPHandler handlerId handlerCls _ outPorts apConns _ _ _) = do
    cmps <- ST.gets participants
    if M.member handlerId cmps then
        return ()
    else do
        progArch <- ST.gets arch
        let cmp = PlantUMLCmpAction handlerId (Just . T.pack $ handlerCls) (Just $ PlantUMLColorName "PaleGreen")
        -- | Insert the new participant
        ST.modify $ \s -> s {
            participants = M.insert handlerId cmp (participants s)
        }
        -- | Now create connections to message queues
        mapM_ (\(_, (queueId, _)) -> do
            case M.lookup queueId (channels progArch) of
                Nothing -> throwError $ "Channel " <> T.pack queueId <> " not found."
                Just channel -> do
                    genChannel channel
                    genChannelConnection handlerId queueId
            ) (M.toList outPorts)
        genResourceAccesses 0 handlerId (fst <$> M.elems apConns)

genInterfaceConnection :: Identifier -> Identifier -> PlantUMLCmpGenMonad a ()
genInterfaceConnection requirer provider =
    let conn = PlantUMLInterfaceConnection requirer provider
    in ST.modify $ \s -> s {
        connections = M.insert (requirer, provider) conn (connections s)
    }

genEventConnection :: Identifier -> Identifier -> PlantUMLCmpGenMonad a ()
genEventConnection source target =
    let conn = PlantUMLEventConnection source target
    in ST.modify $ \s -> s {
        connections = M.insert (source, target) conn (connections s)
    }

genChannelConnection :: Identifier -> Identifier -> PlantUMLCmpGenMonad a ()
genChannelConnection sourceCmpId queueId =
    let conn = PlantUMLChannelConnection sourceCmpId queueId
    in ST.modify $ \s -> s {
        connections = M.insert (sourceCmpId, queueId) conn (connections s)
    }

genTargetResource :: Int -> Identifier -> PlantUMLCmpGenMonad a ()
genTargetResource currentDepth targetId = do
    progArch <- ST.gets arch
    case M.lookup targetId (resources progArch) of
        Nothing -> case M.lookup targetId (pools progArch) of
            Nothing ->
                case M.lookup targetId (atomics progArch) of
                    Nothing -> case M.lookup targetId (atomicArrays progArch) of
                        Nothing -> throwError $ "Resource " <> T.pack targetId <> " not found."
                        Just atomicArray -> do
                            genAtomicArray atomicArray
                    Just atomic -> do
                        genAtomic atomic
            Just pool -> genPool pool
        Just target -> genResource currentDepth target

genResource :: Int -> TPResource a -> PlantUMLCmpGenMonad a ()
genResource currentDepth (TPResource resourceId resourceCls apConns _ _) = do
    let cmp = PlantUMLCmpComponent resourceId (Just . T.pack $ resourceCls) (Just $ PlantUMLColorName "LightYellow")
    -- | Insert the new participant
    ST.modify $ \s -> s {
        participants = M.insert resourceId cmp (participants s)
    }
    genResourceAccesses (currentDepth + 1) resourceId (fst <$> M.elems apConns)

genPool :: TPPool a -> PlantUMLCmpGenMonad a ()
genPool (TPPool poolId ty size _ _) = do
    let cmp = PlantUMLCmpComponent poolId
            (Just $ "Pool&#60;" <> showText ty <> "; " <> showText size <> "&#62;") (Just $ PlantUMLColorName "Plum")
    -- | Insert the new participant
    ST.modify $ \s -> s {
        participants = M.insert poolId cmp (participants s)
    }

genAtomic :: TPAtomic a -> PlantUMLCmpGenMonad a ()
genAtomic (TPAtomic atomicId ty _ _) = do
    let cmp = PlantUMLCmpComponent atomicId 
            (Just $ "Atomic&#60;" <> showText ty <> "&#62;") 
            (Just $ PlantUMLColorName "Coral")
    -- | Insert the new participant
    ST.modify $ \s -> s {
        participants = M.insert atomicId cmp (participants s)
    }

genAtomicArray :: TPAtomicArray a -> PlantUMLCmpGenMonad a ()
genAtomicArray (TPAtomicArray atomicArrayId ty size _ _) = do
    let cmp = PlantUMLCmpComponent atomicArrayId 
            (Just $ "AtomicArray&#60;" <> showText ty <> "," <> showText size <> "&#62;") 
            (Just $ PlantUMLColorName "Coral")
    -- | Insert the new participant
    ST.modify $ \s -> s {
        participants = M.insert atomicArrayId cmp (participants s)
    }

genChannel :: TPChannel a -> PlantUMLCmpGenMonad a ()
genChannel (TPMsgQueue channelId _ _ _ _) = do
    progArch <- ST.gets arch
    let channel = PlantUMLCmpChannel channelId (Just $ PlantUMLColorName "AliceBlue")
    ST.modify $ \s -> s {
        participants = M.insert channelId channel (participants s)
    }
    case M.lookup channelId (channelTargets progArch) of
        Nothing -> throwError $ "Channel " <> T.pack channelId <> " has no target."
        Just (target, _, _) -> do
            genElement target
            genChannelConnection channelId target

genElement :: Identifier -> PlantUMLCmpGenMonad a ()
genElement elemId = do
    progArch <- ST.gets arch
    case M.lookup elemId (emitters progArch) of
        Just emitter -> genEmitter emitter
        Nothing -> case M.lookup elemId (tasks progArch) of
            Just task -> genTask task
            Nothing -> case M.lookup elemId (handlers progArch) of
                Just handler -> genHandler handler
                Nothing -> case M.lookup elemId (resources progArch) of
                    Just resource ->
                        void $ genResource (-1) resource
                    Nothing -> throwError $ "Element " <> T.pack elemId <> " not found. Element must be an emitter, task, handler, or resource."

genComponentDiagram :: PlantUMLCmpGenMonad a ()
genComponentDiagram = do
    mRange <- ST.gets range
    case mRange of
        Nothing -> do
            progArch <- ST.gets arch
            mapM_ genEmitter (emitters progArch)
        Just (CmpDiagramRange [] _)-> do
            progArch <- ST.gets arch
            mapM_ genEmitter (emitters progArch)
        Just (CmpDiagramRange elems _) ->
             mapM_ genElement elems

runPlantUMLCmpGenerator :: TerminaProgArch a -> Maybe CmpDiagramRange -> Either T.Text PlantUMLDiagram
runPlantUMLCmpGenerator progArch mDiagramRange =
    let initialState = PlantUMLCmpGenState progArch mDiagramRange M.empty M.empty
        (result, finalState) = ST.runState (runExceptT genComponentDiagram) initialState
    in
        case result of
            Left err -> Left err
            Right _ ->
                let parts = M.elems (participants finalState)
                    conns = M.elems (connections finalState)
                in Right $ PlantUMLCmpDiag parts conns