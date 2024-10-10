module ControlFlow.Architecture.Checks where
import qualified Data.Map as M
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import ControlFlow.Architecture.Errors.Errors
import Control.Monad.Reader
import qualified Data.Set as S
import Control.Monad.Except
import Utils.Annotations
import qualified Control.Monad.State.Strict as ST

getDisconnectedEmitters :: TerminaProgArch SemanticAnn -> [Identifier]
getDisconnectedEmitters tp = M.keys . M.filter ((`M.notMember` emitterTargets tp) . getEmmiterIdentifier) . emitters $ tp

type BoxChannelMap = M.Map Identifier Identifier

type BoxChannelCheckMonad = ExceptT ArchitectureError (ST.State BoxChannelMap)

getBoxSourceTask :: TerminaProgArch SemanticAnn -> TPTask SemanticAnn -> Identifier -> BoxChannelCheckMonad Identifier
getBoxSourceTask progArchitecture task outPt = do
    let taskCls = taskClasses progArchitecture M.! taskClass task
    case M.lookup outPt (outBoxSend . classBoxIOMaps $ taskCls) of
        Just sources -> 
            case S.toList sources of
                [] -> throwError $ annotateError Internal EUnboxingTaskSource
                [source] -> fst <$> getBoxSourceTaskIOBox source
                source : xs -> do
                    (fstBoxSource, fstAnn) <- getBoxSourceTaskIOBox source
                    mapM_ (\source' -> do
                        (boxSource, ann) <- getBoxSourceTaskIOBox source'
                        unless (boxSource == fstBoxSource) 
                            (throwError $ annotateError (location ann) 
                                (EMismatchedBoxSourceTaskOutPort (taskName task) outPt boxSource (fstBoxSource, location fstAnn)))) xs
                    return fstBoxSource
        Nothing -> throwError $ annotateError Internal EUnboxingChannelSource

    where

        getBoxSourceTaskIOBox :: InBox -> BoxChannelCheckMonad (Identifier, SemanticAnn)
        getBoxSourceTaskIOBox (InBoxAlloc port) = 
            -- We reached a terminal allocator port. Now we only have to get the
            -- allocator source and we are done
            case M.lookup port (taskAPConnections task) of
                Just source -> return source
                Nothing -> throwError $ annotateError Internal EUnboxingTaskSource
        getBoxSourceTaskIOBox (InBoxInput port) = 
            -- | The box was received from a port connected to a channel
            case M.lookup port (taskInputPortConns task) of
                Just (channelId, ann) -> 
                    case M.lookup channelId (channels progArchitecture) of
                        Just nextChannel -> do
                            boxSource <- getBoxSourceChannel progArchitecture nextChannel
                            return (boxSource, ann)
                        Nothing -> throwError $ annotateError Internal EUnboxingTaskSource
                Nothing -> throwError $ annotateError Internal EUnboxingTaskSource
        -- | The rest of the cases should not happen, since task does not have procedures
        -- and no event emitter may send us a box  
        getBoxSourceTaskIOBox _ = throwError $ annotateError Internal EUnboxingTaskSource

getBoxSourceHandler :: TerminaProgArch SemanticAnn -> TPHandler SemanticAnn -> Identifier -> BoxChannelCheckMonad Identifier
getBoxSourceHandler progArchitecture handler outPt = do
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    case M.lookup outPt (outBoxSend . classBoxIOMaps $ handlerCls) of
        Just sources -> 
            case S.toList sources of
                [] -> throwError $ annotateError Internal EUnboxingHandlerSource
                [source] -> fst <$> getBoxSourceHandlerIOBox source
                source : xs -> do
                    (fstBoxSource, fstAnn) <- getBoxSourceHandlerIOBox source
                    mapM_ (\source' -> do
                        (boxSource, ann) <- getBoxSourceHandlerIOBox source'
                        unless (boxSource == fstBoxSource) 
                            (throwError $ annotateError (location ann) 
                                (EMismatchedBoxSourceHandlerOutPort (handlerName handler) outPt boxSource (fstBoxSource, location fstAnn)))) xs
                    return fstBoxSource
        Nothing -> throwError $ annotateError Internal EUnboxingChannelSource
    
    where

        getBoxSourceHandlerIOBox :: InBox -> BoxChannelCheckMonad (Identifier, SemanticAnn)
        getBoxSourceHandlerIOBox (InBoxAlloc port) = 
            -- We reached a terminal allocator port. Now we only have to get the
            -- allocator source and we are done
            case M.lookup port (handlerAPConnections handler) of
                Just source -> return source
                Nothing -> throwError $ annotateError Internal EUnboxingHandlerSource
        getBoxSourceHandlerIOBox _ = throwError $ annotateError Internal EUnboxingHandlerSource


getBoxSourceSend :: TerminaProgArch SemanticAnn -> Identifier -> Identifier -> BoxChannelCheckMonad Identifier
getBoxSourceSend progArchitecture source outPort = do
    -- | Check if the element is a task
    case M.lookup source (tasks progArchitecture) of
        Just task -> getBoxSourceTask progArchitecture task outPort
        Nothing ->
            case M.lookup source (handlers progArchitecture) of
                Just handler -> getBoxSourceHandler progArchitecture handler outPort
                Nothing -> throwError $ annotateError Internal EUnboxingChannelSource

getBoxSourceChannel :: 
    TerminaProgArch SemanticAnn
    -- | The current channel
    -> TPChannel SemanticAnn 
    -> BoxChannelCheckMonad Identifier
getBoxSourceChannel progArchitecture (TPMsgQueue channelId (BoxSubtype _) _ _ cann) = do
    currChannelMap <- ST.get
    -- | First we need to get check if the curren channel has been already visited
    case M.lookup channelId currChannelMap of
        Just boxSource -> return boxSource
        Nothing -> do
            -- | If the channel has not been visited, we need to add it to the map
            -- and check the pool from which the box was originated
            let sources = channelSources progArchitecture
            
            case M.lookup channelId sources of
                Nothing -> throwError $ annotateError (location cann) (EDisconnectedChannel channelId)
                Just [] -> throwError $ annotateError Internal EUnboxingChannelSource
                Just [(source, port, _)] -> do
                    boxSource <- getBoxSourceSend progArchitecture source port
                    ST.modify (M.insert channelId boxSource)
                    return boxSource
                Just ((source, port, ann) : xs) -> do
                    fstBoxSource <- getBoxSourceSend progArchitecture source port
                    mapM_ (\(source', port', ann') -> do
                            boxSource' <- getBoxSourceSend progArchitecture source' port'
                            unless (fstBoxSource == boxSource') 
                                (throwError $ annotateError (location ann')
                                    (EMismatchedBoxSourceChannelInbound channelId (source', port') (source, port, location ann)))
                        ) xs
                    ST.modify (M.insert channelId fstBoxSource)
                    return fstBoxSource
getBoxSourceChannel _ _ = throwError $ annotateError Internal EUnboxingChannelSource

checkChannelBoxSources ::
    TerminaProgArch SemanticAnn
    -> BoxChannelCheckMonad ()
checkChannelBoxSources progArchitecture = 
    mapM_ (getBoxSourceChannel progArchitecture) (channels progArchitecture)

runCheckChannelBoxSources :: TerminaProgArch SemanticAnn -> Either ArchitectureError BoxChannelMap
runCheckChannelBoxSources progArchitecture = 
    case ST.runState (runExceptT $ checkChannelBoxSources progArchitecture) M.empty of
        (Left err, _) -> Left err
        (Right _, boxMap) -> Right boxMap

data BoxSourceAllocator a =
    UnknownAllocator
    | BoxAllocator Identifier a

-- | Map between a procedure call with a boxed parameter and and the box from
-- which the was originated. The key is a tuple with the name of the calling element,
-- the name of the access port and the name of the procedure and the index of the box parameter.
type BoxProcedureCallMap = M.Map (Identifier, Identifier, Identifier, Integer) (BoxSourceAllocator SemanticAnn)

type BoxProcedureCallCheckMonad = ExceptT ArchitectureError (ST.State BoxProcedureCallMap)

getBoxSourceFreeTask :: 
    TerminaProgArch SemanticAnn
    -> TPTask SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxSourceFreeTask progArchitecture (TPTask _ classId _ _ _ _ _ _ _) outPt = do
    return ()

getBoxSourceFreeHandler ::
    TerminaProgArch SemanticAnn
    -> TPHandler SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxSourceFreeHandler progArchitecture (TPHandler _ classId _ _ _ _ _ _) outPt = do
    return ()

getBoxHandlerProcedureCall ::
    TerminaProgArch SemanticAnn
    -> TPHandler SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxHandlerProcedureCall progArchitecture handler accessPt procId argNum = do
    return ()

getBoxTaskProcedureCall ::
    TerminaProgArch SemanticAnn
    -> TPTask SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxTaskProcedureCall progArchitecture (TPTask taskName classId _ _ _ apConns _ _ _) calledPort calledProc calledArgNum = do
    let taskCls = resourceClasses progArchitecture M.! classId
    case M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall . classBoxIOMaps $ taskCls) of
        Nothing -> return UnknownAllocator -- ^ The procedure is never called from within this task
        Just sources -> 
            case S.toList sources of
                [] -> throwError $ annotateError Internal EUnboxingTaskSource
                [source] -> getBoxTaskProcedureCallIOBox source
                source : xs -> do
                    fstBoxSource <- getBoxTaskProcedureCallIOBox source
                    foldM (\acc source' -> do
                        boxSource <- getBoxTaskProcedureCallIOBox source'
                        case (acc, boxSource) of
                            (UnknownAllocator, UnknownAllocator) -> return UnknownAllocator
                            (BoxAllocator {}, UnknownAllocator) -> return acc
                            (UnknownAllocator, BoxAllocator {}) -> return boxSource
                            (BoxAllocator fstBoxSourceId fstAnn, BoxAllocator boxSourceId ann) -> do
                                unless (fstBoxSourceId == boxSourceId) 
                                    (throwError $ annotateError (location ann) 
                                        (EMismatchedBoxSourceProcedureCall taskName calledProc calledArgNum boxSourceId (fstBoxSourceId, location fstAnn)))
                                return boxSource
                        ) fstBoxSource xs

    where

        getBoxTaskProcedureCallIOBox :: InBox -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
        getBoxTaskProcedureCallIOBox (InBoxAlloc port) = 
            -- We reached a terminal allocator port. Now we only have to get the
            -- allocator source and we are done
            case M.lookup port apConns of
                Just (allocatorId, ann) -> return (BoxAllocator allocatorId ann)
                -- | This should not happen, since all the ports of the task
                -- must be connected to something
                Nothing -> throwError $ annotateError Internal EUnboxingTaskSource



getBoxProcedureCall ::
    TerminaProgArch SemanticAnn
    -> Identifier -- ^ The name of the element that is calling the procedure
    -> Identifier -- ^ The name of the access port of the calling element
    -> Identifier -- ^ The name of the procedure being called
    -> Integer -- ^ The number of the argument
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxProcedureCall progArchitecture element accessPt procId argNum = do
    -- | Check if the element is a task
    case M.lookup element (tasks progArchitecture) of
        Just task -> getBoxTaskProcedureCall progArchitecture task accessPt procId argNum
        Nothing -> 
            -- | Check if the element is a handler
            case M.lookup element (handlers progArchitecture) of
                Just handler -> getBoxHandlerProcedureCall progArchitecture handler accessPt procId argNum
                Nothing -> 
                    -- | Check if the element is a resource
                    case M.lookup element (resources progArchitecture) of
                        Just resource -> getBoxResourceProcedureCall progArchitecture resource accessPt procId argNum
                        -- | This should not happen, since all the elements in the program
                        -- must be either a task, a handler or a resource
                        Nothing -> throwError $ annotateError Internal EUnboxingProcedureCall

getBoxResourceProcedureCall :: 
    TerminaProgArch SemanticAnn
    -> TPResource SemanticAnn -- ^ The name of the resource
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxResourceProcedureCall progArchitecture (TPResource resourceId classId apConnections _ rann) calledPort calledProc calledArgNum = do
    -- | Check if we are actually calling the procedure from the code of the resource
    let resourceCls = resourceClasses progArchitecture M.! classId
    case M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall . classBoxIOMaps $ resourceCls) of
        Nothing -> return UnknownAllocator -- ^ The procedure is never called from within this resource
        Just sources -> 
            case S.toList sources of
                [] -> throwError $ annotateError Internal EUnboxingResourceSource
                [source] -> getBoxResourceProcedureCallIOBox source
                source : xs -> do
                    fstBoxSource <- getBoxResourceProcedureCallIOBox source
                    foldM (\acc source' -> do
                        boxSource <- getBoxResourceProcedureCallIOBox source'
                        case (acc, boxSource) of
                            (UnknownAllocator, UnknownAllocator) -> return UnknownAllocator
                            (BoxAllocator {}, UnknownAllocator) -> return acc
                            (UnknownAllocator, BoxAllocator {}) -> return boxSource
                            (BoxAllocator fstBoxSourceId fstAnn, BoxAllocator boxSourceId ann) -> do
                                unless (fstBoxSourceId == boxSourceId) 
                                    (throwError $ annotateError (location ann) 
                                        (EMismatchedBoxSourceProcedureCall resourceId calledProc calledArgNum boxSourceId (fstBoxSourceId, location fstAnn)))
                                return boxSource
                        ) fstBoxSource xs

    where

        getBoxResourceProcedureCallIOBox :: InBox -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
        getBoxResourceProcedureCallIOBox (InBoxAlloc port) = 
            -- We reached a terminal allocator port. Now we only have to get the
            -- allocator source and we are done
            case M.lookup port apConnections of
                Just (allocatorId, ann) -> return (BoxAllocator allocatorId ann)
                -- | This should not happen, since all the ports of the resource
                -- must be connected to something
                Nothing -> throwError $ annotateError Internal EUnboxingResourceSource
        getBoxResourceProcedureCallIOBox (InBoxProcedureCall procName argNum) = 
            -- | The box was received via a call to one of the procedures of the resource
            -- We need to inspect all the elements that are connected to the resource and
            -- see where do their boxes come from
            case M.lookup resourceId (resourceSources progArchitecture) of
                Just [(caller, accessPt, _)] -> 
                    getBoxProcedureCall progArchitecture caller accessPt procName argNum
                Just ((fstCaller, fstAccessPort, fstAnn) : xs) -> do
                    boxSource <- getBoxProcedureCall progArchitecture fstCaller fstAccessPort procName argNum
                    foldM (\acc (caller, accessPt, ann) -> do
                        boxSource' <- getBoxProcedureCall progArchitecture caller accessPt procName argNum
                        case (acc, boxSource') of
                            (UnknownAllocator, UnknownAllocator) -> return UnknownAllocator
                            (BoxAllocator {}, UnknownAllocator) -> return acc
                            (UnknownAllocator, BoxAllocator {}) -> return boxSource'
                            (BoxAllocator fstBoxSourceId _, BoxAllocator boxSourceId _) -> do
                                unless (fstBoxSourceId == boxSourceId) 
                                    (throwError $ annotateError (location ann) 
                                        (EMismatchedBoxSourceProcedureCall resourceId calledProc calledArgNum boxSourceId (fstBoxSourceId, location fstAnn)))
                                return boxSource'
                        ) boxSource xs
                Just [] -> throwError $ annotateError Internal EUnboxingResourceSource
                -- | This means that the resource is not connected to anything (which would be an error)
                Nothing -> throwError $ annotateError (location rann) (EDisconnectedResource classId)
        -- | The rest of the cases should not happen, since resources do not have procedures
        -- and no event emitter may send us a box  
        getBoxResourceProcedureCallIOBox _ = throwError $ annotateError Internal EUnboxingResourceSource

-- | Get the source of a box that is being freed by a resource
getBoxSourceFreeResource :: 
    TerminaProgArch SemanticAnn
    -> TPResource SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxProcedureCallCheckMonad (BoxSourceAllocator SemanticAnn)
getBoxSourceFreeResource progArchitecture (TPResource resourceId _ _ _ rann) freePt = do
    return ()
