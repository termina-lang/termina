module ControlFlow.Architecture.Checks (
    getDisconnectedEmitters,
    runCheckBoxSources
) where
import qualified Data.Map as M
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import Control.Monad.Reader
import Control.Monad.Except
import Utils.Annotations
import ControlFlow.Architecture.Errors.Errors

getDisconnectedEmitters :: TerminaProgArch SemanticAnn -> [Identifier]
getDisconnectedEmitters tp = M.keys . M.filter ((`M.notMember` emitterTargets tp) . getEmmiterIdentifier) . emitters $ tp

type BoxCheckMonad = ExceptT ArchitectureError (Reader (TerminaProgArch SemanticAnn))

checkNextSource :: Location -> BoxCheckMonad () -> BoxCheckMonad ()
checkNextSource loc = withExceptT (\case {
        AnnotatedError (EMismatchedBoxSource expectedSource actualSource traceLocations) lastLocation ->
            AnnotatedError (EMismatchedBoxSource expectedSource actualSource (loc : traceLocations)) lastLocation;
        err -> err
    })

checkBoxSourceProcedureCallResource ::
    Identifier -- ^ Expected source of the box
    -> TPResource SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxCheckMonad ()
checkBoxSourceProcedureCallResource expectedSource resource calledPort calledProc calledArgNum = do
    progArchitecture <- ask
    let resourceCls = resourceClasses progArchitecture M.! resourceClass resource
    forM_ (M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall. classBoxIOMaps $ resourceCls)) $
        mapM_ (checkResourceSourceInBox expectedSource resource)

checkBoxSourceProcedureCallHandler ::
    Identifier -- ^ Expected source of the box
    -> TPHandler SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxCheckMonad ()
checkBoxSourceProcedureCallHandler expectedSource handler calledPort calledProc calledArgNum = do
    progArchitecture <- ask
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    forM_ (M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall. classBoxIOMaps $ handlerCls)) $
        mapM_ (checkHandlerSourceInBox expectedSource handler)

checkBoxSourceProcedureCallTask ::
    Identifier -- ^ Expected source of the box
    -> TPTask SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxCheckMonad ()
checkBoxSourceProcedureCallTask expectedSource task calledPort calledProc calledArgNum = do
    progArchitecture <- ask
    let taskCls = taskClasses progArchitecture M.! taskClass task
    forM_ (M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall. classBoxIOMaps $ taskCls)) $
        mapM_ (checkTaskSourceInBox expectedSource task)

checkBoxProcedureCall ::
    Identifier -- ^ Expected source of the box
    -> Identifier -- ^ The name of the element that is calling the procedure
    -> Identifier -- ^ The name of the access port of the calling element
    -> Identifier -- ^ The name of the procedure being called
    -> Integer -- ^ The number of the argument
    -> BoxCheckMonad ()
checkBoxProcedureCall expectedSource elemnt accessPt procId argNum = do
    progArchitecture <- ask
    -- | Check if the element is a task
    case M.lookup elemnt (tasks progArchitecture) of
        Just task -> checkBoxSourceProcedureCallTask expectedSource task accessPt procId argNum
        Nothing ->
            -- | Check if the element is a handler
            case M.lookup elemnt (handlers progArchitecture) of
                Just handler -> checkBoxSourceProcedureCallHandler expectedSource handler accessPt procId argNum
                Nothing ->
                    -- | Check if the element is a resource
                    case M.lookup elemnt (resources progArchitecture) of
                        Just resource -> checkBoxSourceProcedureCallResource expectedSource resource accessPt procId argNum
                        -- | This should not happen, since all the elements in the program
                        -- must be either a task, a handler or a resource
                        Nothing -> throwError $ annotateError Internal EUnboxingProcedureCall

checkBoxSourceTaskSend :: Identifier -> TPTask SemanticAnn -> Identifier -> BoxCheckMonad ()
checkBoxSourceTaskSend expectedSource task outPt = do
    progArchitecture <- ask
    let taskCls = taskClasses progArchitecture M.! taskClass task
    case M.lookup outPt (outBoxSend . classBoxIOMaps $ taskCls) of
        Just sources ->
            mapM_ (checkTaskSourceInBox expectedSource task) sources
        Nothing -> throwError $ annotateError Internal EUnboxingTaskSource

checkBoxSourceHandlerSend :: Identifier -> TPHandler SemanticAnn -> Identifier -> BoxCheckMonad ()
checkBoxSourceHandlerSend expectedSource handler outPt = do
    progArchitecture <- ask
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    case M.lookup outPt (outBoxSend . classBoxIOMaps $ handlerCls) of
        Just sources ->
            mapM_ (checkHandlerSourceInBox expectedSource handler) sources
        Nothing -> throwError $ annotateError Internal EUnboxingHandlerSource

getBoxSourceSend :: Identifier -> (Identifier, Identifier, SemanticAnn) -> BoxCheckMonad ()
getBoxSourceSend expectedSource (source, outPort, ann) = do
    progArchitecture <- ask
    -- | Check if the element is a task
    case M.lookup source (tasks progArchitecture) of
        Just task -> checkNextSource (location ann) (checkBoxSourceTaskSend expectedSource task outPort)
        Nothing ->
            case M.lookup source (handlers progArchitecture) of
                Just handler -> checkNextSource (location ann) (checkBoxSourceHandlerSend expectedSource handler outPort)
                Nothing -> throwError $ annotateError Internal EUnboxingChannelSource

getBoxSourceChannel ::
    Identifier -- ^ Expected source of the box
    -> TPChannel SemanticAnn -- ^ The channel from which the box is being received
    -> BoxCheckMonad ()
getBoxSourceChannel expectedSource (TPMsgQueue channelId (BoxSubtype _) _ _ cann) = do
    -- | First we need to get check if the current channel has been already visited
    progArchitecture <- ask
    let sources = channelSources progArchitecture
    case M.lookup channelId sources of
        Nothing -> throwError $ annotateError (location cann) (EDisconnectedChannel channelId)
        Just ss -> mapM_ (getBoxSourceSend expectedSource) ss
getBoxSourceChannel _ _ = throwError $ annotateError Internal EUnboxingChannelSource

checkResourceSourceInBox ::
    Identifier -- ^ Expected source of the box
    -> TPResource SemanticAnn -- ^ The name of the resource
    -> InBox SemanticAnn
    -> BoxCheckMonad ()
checkResourceSourceInBox expectedSource resource (InBoxAlloc port ann) =
    -- We reached a terminal allocator port. Now we only have to get the
    -- allocator source and we are done
    case M.lookup port (resAPConnections resource) of
        Just (actualSource, _) -> unless (actualSource == expectedSource)
            (throwError $ annotateError (location ann) (EMismatchedBoxSource expectedSource actualSource []))
        -- | This should not happen, since all the ports of the resource
        -- must be connected to something
        Nothing -> throwError $ annotateError Internal EUnboxingResourceSource
checkResourceSourceInBox expectedSource resource (InBoxProcedureCall procName argNum ann) = do
    progArchitecture <- ask
    -- | The box was received via a call to one of the procedures of the resource
    -- We need to inspect all the elements that are connected to the resource and
    -- see where do their boxes come from
    case M.lookup (resourceName resource) (resourceSources progArchitecture) of
        Just callers -> mapM_ (\(caller, accessPt, _) ->
            checkNextSource (location ann) (checkBoxProcedureCall expectedSource caller accessPt procName argNum)) callers
        -- | This means that the resource is not connected to anything (which would be an error)
        Nothing -> throwError $ annotateError (location (resourceAnns resource)) (EDisconnectedResource (resourceName resource))
-- | The rest of the cases should not happen, since resources do not have procedures
-- and no event emitter may send us a box  
checkResourceSourceInBox _ _ _ = throwError $ annotateError Internal EUnboxingResourceSource

checkTaskSourceInBox ::
    Identifier -- ^ Expected source of the box
    -> TPTask SemanticAnn
    -> InBox SemanticAnn -> BoxCheckMonad ()
checkTaskSourceInBox expectedSource task (InBoxAlloc port ann) =
    -- We reached a terminal allocator port. Now we only have to get the
    -- allocator source and we are done
    case M.lookup port (taskAPConnections task) of
        Just (actualSource, _) -> unless (actualSource == expectedSource)
            (throwError $ annotateError (location ann) (EMismatchedBoxSource expectedSource actualSource []))
        -- | This should not happen, since all the ports of the task
        -- must be connected to something
        Nothing -> throwError $ annotateError Internal EUnboxingTaskSource
checkTaskSourceInBox expectedSource task (InBoxInput port ann) = do
    -- | The box was received from a port connected to a channel. We must
    -- obtain the name of the channel to which the port is connected and then
    -- obtain the source of the box from the box-channel map
    progArchitecture <- ask
    case M.lookup port (taskInputPortConns task) of
        Nothing -> throwError $ annotateError Internal EUnboxingTaskSource
        Just (channel, _) -> case M.lookup channel (channels progArchitecture) of
            Just nextChannel -> checkNextSource (location ann) (getBoxSourceChannel expectedSource nextChannel)
            Nothing -> throwError $ annotateError Internal EUnboxingTaskSource
-- | The rest of the cases should not happen, since tasks do not have procedures
checkTaskSourceInBox _ _ _ = throwError $ annotateError Internal EUnboxingTaskSource

checkHandlerSourceInBox ::
    Identifier -- ^ Expected source of the box
    -> TPHandler SemanticAnn
    -> InBox SemanticAnn
    -> BoxCheckMonad ()
checkHandlerSourceInBox expectedSource handler (InBoxAlloc port ann) =
    -- We reached a terminal allocator port. Now we only have to get the
    -- allocator source and we are done
    case M.lookup port (handlerAPConnections handler) of
        Just (actualSource, _) -> unless (actualSource == expectedSource)
            (throwError $ annotateError (location ann) (EMismatchedBoxSource expectedSource actualSource []))
        -- | This should not happen, since all the ports of the handler
        -- must be connected to something
        Nothing -> throwError $ annotateError Internal EUnboxingHandlerSource
checkHandlerSourceInBox _ _ _ = throwError $ annotateError Internal EUnboxingHandlerSource

checkBoxSourceResourceFree ::
    Identifier -- ^ Expected source of the box
    -> TPResource SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxCheckMonad ()
checkBoxSourceResourceFree expectedSource resource outPt = do
    progArchitecture <- ask
    let resourceCls = resourceClasses progArchitecture M.! resourceClass resource
    forM_ (M.lookup outPt (outBoxFree . classBoxIOMaps $ resourceCls)) $
        mapM_ (checkResourceSourceInBox expectedSource resource)

checkBoxSourceTaskFree ::
    Identifier -- ^ Expected source of the box
    -> TPTask SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxCheckMonad ()
checkBoxSourceTaskFree expectedSource task calledPort = do
    progArchitecture <- ask
    let taskCls = taskClasses progArchitecture M.! taskClass task
    -- | If the port is not on the outBoxFree map, it means that the task only
    -- allocates the box and does not free it
    forM_ (M.lookup calledPort (outBoxFree . classBoxIOMaps $ taskCls)) $ 
        mapM_ (checkTaskSourceInBox expectedSource task)

checkBoxSourceHandlerFree ::
    Identifier -- ^ Expected source of the box
    -> TPHandler SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxCheckMonad ()
checkBoxSourceHandlerFree expectedSource handler calledPort = do
    progArchitecture <- ask
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    -- | If the port is not on the outBoxFree map, it means that the handler only
    -- allocates the box and does not free it
    forM_ (M.lookup calledPort (outBoxFree . classBoxIOMaps $ handlerCls)) $
        mapM_ (checkHandlerSourceInBox expectedSource handler)

checkBoxSourceFree ::
    Identifier -- ^ Expected source of the box
    -> Identifier -- ^ The name of the element (task, handler or resource)
    -> Identifier -- ^ The name of the access port
    -> BoxCheckMonad ()
checkBoxSourceFree expectedSource elemnt accessPt = do
    progArchitecture <- ask
    -- | Check if the element is a task
    case M.lookup elemnt (tasks progArchitecture) of
        Just task -> checkBoxSourceTaskFree expectedSource task accessPt
        Nothing ->
            -- | Check if the element is a handler
            case M.lookup elemnt (handlers progArchitecture) of
                Just handler -> checkBoxSourceHandlerFree expectedSource handler accessPt
                Nothing ->
                    -- | Check if the element is a resource
                    case M.lookup elemnt (resources progArchitecture) of
                        Just resource -> checkBoxSourceResourceFree expectedSource resource accessPt
                        -- | This should not happen, since all the elements in the program
                        -- must be either a task, a handler or a resource
                        Nothing -> throwError $ annotateError Internal EUnboxingFree


checkBoxSource :: TPPool SemanticAnn -> BoxCheckMonad ()
checkBoxSource (TPPool poolName _ _ _ ann) = do
    progArchitecture <- ask
    case M.lookup poolName (resourceSources progArchitecture) of
        Just [] -> throwError $ annotateError Internal EUnboxingResourceSource
        Just callers -> do
            mapM_ (\(caller, accessPt, cann) -> do
                    checkNextSource (location cann) (checkBoxSourceFree poolName caller accessPt)
                ) callers
        -- | This means that the resource is not connected to anything (which would be an error)
        Nothing -> throwError $ annotateError (location ann) (EDisconnectedPool poolName)

checkBoxSources :: BoxCheckMonad ()
checkBoxSources = do
    progArchitecture <- ask
    mapM_ checkBoxSource (M.elems (pools progArchitecture))

runCheckBoxSources :: 
    TerminaProgArch SemanticAnn 
    -> Either ArchitectureError ()
runCheckBoxSources = runReader (runExceptT checkBoxSources)