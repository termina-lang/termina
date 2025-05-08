module ControlFlow.Architecture.Checks.BoxSources (
    checkBoxSources
) where


import Control.Monad
import Control.Monad.Except
import ControlFlow.Architecture.Errors
import Control.Monad.Reader
import ControlFlow.Architecture.Types
import Semantic.Types
import Utils.Annotations
import Core.AST
import qualified Data.Map as M

-- | This module provides functionality to verify the correctness of box sources
-- in a Termina program architecture. It ensures that boxes are allocated, 
-- transferred, and freed in a manner consistent with the program's architecture 
-- and semantics. The checks are performed using a monadic context that allows 
-- for error handling and access to the program's architecture.
--
-- The main exported function is:
--
-- * 'checkBoxSources': This function performs a comprehensive check of all 
--   box sources in the program architecture, ensuring that all pools, tasks, 
--   handlers, and resources adhere to the expected box source semantics.
--
-- The module defines several helper functions to handle specific cases:
--
-- * 'checkBoxSourceProcedureCallResource', 'checkBoxSourceProcedureCallHandler', 
--   and 'checkBoxSourceProcedureCallTask': These functions verify the source 
--   of boxes passed during procedure calls for resources, handlers, and tasks, 
--   respectively.
--
-- * 'checkBoxProcedureCall': A higher-level function that determines the type 
--   of the calling element (task, handler, or resource) and delegates the 
--   procedure call check to the appropriate function.
--
-- * 'checkBoxSourceTaskSend', 'checkBoxSourceHandlerSend', and 
--   'checkBoxSourceResourceFree': These functions validate the sources of boxes 
--   sent or freed by tasks, handlers, and resources.
--
-- * 'getBoxSourceChannel' and 'getBoxSourceSend': These functions trace the 
--   source of a box through channels and connections.
--
-- * 'checkResourceSourceInBox', 'checkTaskSourceInBox', and 
--   'checkHandlerSourceInBox': These functions validate the source of a box 
--   within a resource, task, or handler, ensuring that the box's origin matches 
--   the expected source.
--
-- The module uses the 'BoxSourcesCheckMonad', a combination of the 'ExceptT' 
-- and 'Reader' monads, to handle errors and provide access to the program 
-- architecture during the checks.
--
-- Errors are reported using the 'ArchitectureError' type, with detailed 
-- annotations to help trace the source of mismatches or inconsistencies.

type BoxSourcesCheckMonad = ExceptT ArchitectureError (Reader (TerminaProgArch SemanticAnn))

checkNextSource :: Location -> BoxSourcesCheckMonad () -> BoxSourcesCheckMonad ()
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
    -> BoxSourcesCheckMonad ()
checkBoxSourceProcedureCallResource expectedSource resource calledPort calledProc calledArgNum = do
    progArchitecture <- ask
    let resourceCls = resourceClasses progArchitecture M.! resourceClass resource
    forM_ (M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall. classBoxIOMaps $ resourceCls)) $
        mapM_ (uncurry (checkResourceSourceInBox expectedSource resource))

checkBoxSourceProcedureCallHandler ::
    Identifier -- ^ Expected source of the box
    -> TPHandler SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxSourcesCheckMonad ()
checkBoxSourceProcedureCallHandler expectedSource handler calledPort calledProc calledArgNum = do
    progArchitecture <- ask
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    forM_ (M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall. classBoxIOMaps $ handlerCls)) $
        mapM_ (uncurry (checkHandlerSourceInBox expectedSource handler))

checkBoxSourceProcedureCallTask ::
    Identifier -- ^ Expected source of the box
    -> TPTask SemanticAnn
    -> Identifier -- ^ The name of the access port
    -> Identifier -- ^ The name of the procedure
    -> Integer -- ^ The number of the argument
    -> BoxSourcesCheckMonad ()
checkBoxSourceProcedureCallTask expectedSource task calledPort calledProc calledArgNum = do
    progArchitecture <- ask
    let taskCls = taskClasses progArchitecture M.! taskClass task
    forM_ (M.lookup (calledPort, calledProc, calledArgNum) (outBoxProcedureCall. classBoxIOMaps $ taskCls)) $
        mapM_ (uncurry (checkTaskSourceInBox expectedSource task))

checkBoxProcedureCall ::
    Identifier -- ^ Expected source of the box
    -> Identifier -- ^ The name of the element that is calling the procedure
    -> Identifier -- ^ The name of the access port of the calling element
    -> Identifier -- ^ The name of the procedure being called
    -> Integer -- ^ The number of the argument
    -> BoxSourcesCheckMonad ()
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

checkBoxSourceTaskSend :: Identifier -> TPTask SemanticAnn -> Identifier -> BoxSourcesCheckMonad ()
checkBoxSourceTaskSend expectedSource task outPt = do
    progArchitecture <- ask
    let taskCls = taskClasses progArchitecture M.! taskClass task
    case M.lookup outPt (outBoxSend . classBoxIOMaps $ taskCls) of
        Just sources ->
            mapM_ (uncurry (checkTaskSourceInBox expectedSource task)) sources
        Nothing -> throwError $ annotateError Internal EUnboxingTask

checkBoxSourceHandlerSend :: Identifier -> TPHandler SemanticAnn -> Identifier -> BoxSourcesCheckMonad ()
checkBoxSourceHandlerSend expectedSource handler outPt = do
    progArchitecture <- ask
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    case M.lookup outPt (outBoxSend . classBoxIOMaps $ handlerCls) of
        Just sources ->
            mapM_ (uncurry (checkHandlerSourceInBox expectedSource handler)) sources
        Nothing -> throwError $ annotateError Internal EUnboxingHandler

getBoxSourceSend :: Identifier -> (Identifier, Identifier, SemanticAnn) -> BoxSourcesCheckMonad ()
getBoxSourceSend expectedSource (source, outPort, _ann) = do
    progArchitecture <- ask
    -- | Check if the element is a task
    case M.lookup source (tasks progArchitecture) of
        Just task -> checkBoxSourceTaskSend expectedSource task outPort
        Nothing ->
            case M.lookup source (handlers progArchitecture) of
                Just handler -> checkBoxSourceHandlerSend expectedSource handler outPort
                Nothing -> throwError $ annotateError Internal EUnboxingChannel

getBoxSourceChannel ::
    Identifier -- ^ Expected source of the box
    -> TPChannel SemanticAnn -- ^ The channel from which the box is being received
    -> BoxSourcesCheckMonad ()
getBoxSourceChannel expectedSource (TPMsgQueue channelId (TBoxSubtype _) _ _ _) = do
    -- | First we need to get check if the current channel has been already visited
    progArchitecture <- ask
    let sources = channelSources progArchitecture
    case M.lookup channelId sources of
        Nothing -> throwError $ annotateError Internal EUnboxingChannel
        Just ss -> mapM_ (getBoxSourceSend expectedSource) ss
getBoxSourceChannel _ _ = throwError $ annotateError Internal EUnboxingChannel

checkResourceSourceInBox ::
    Identifier -- ^ Expected source of the box
    -> TPResource SemanticAnn -- ^ The name of the resource
    -> SemanticAnn
    -> InBox SemanticAnn
    -> BoxSourcesCheckMonad ()
checkResourceSourceInBox expectedSource resource prevAnn (InBoxAlloc port ann) =
    -- We reached a terminal allocator port. Now we only have to get the
    -- allocator source and we are done
    case M.lookup port (resAPConnections resource) of
        Just (actualSource, _) -> unless (actualSource == expectedSource)
            (throwError $ annotateError (getLocation ann) (EMismatchedBoxSource expectedSource actualSource [getLocation prevAnn]))
        -- | This should not happen, since all the ports of the resource
        -- must be connected to something
        Nothing -> throwError $ annotateError Internal EUnboxingResource
checkResourceSourceInBox expectedSource resource prevAnn (InBoxProcedureCall procName argNum) = do
    progArchitecture <- ask
    -- | The box was received via a call to one of the procedures of the resource
    -- We need to inspect all the elements that are connected to the resource and
    -- see where do their boxes come from
    case M.lookup (resourceName resource) (resourceSources progArchitecture) of
        Just callers -> mapM_ (\(caller, accessPt, _) ->
            checkNextSource (getLocation prevAnn) (checkBoxProcedureCall expectedSource caller accessPt procName argNum)) callers
        -- | This means that the resource is not connected to anything (this shouuld not happen)
        Nothing -> throwError $ annotateError Internal EUnboxingResource
-- | The rest of the cases should not happen, since resources do not have procedures
-- and no event emitter may send us a box  
checkResourceSourceInBox _ _ _ _ = throwError $ annotateError Internal EUnboxingResource

checkTaskSourceInBox ::
    Identifier -- ^ Expected source of the box
    -> TPTask SemanticAnn
    -> SemanticAnn -> InBox SemanticAnn -> BoxSourcesCheckMonad ()
checkTaskSourceInBox expectedSource task prevAnn (InBoxAlloc port ann) =
    -- We reached a terminal allocator port. Now we only have to get the
    -- allocator source and we are done
    case M.lookup port (taskAPConnections task) of
        Just (actualSource, _) -> unless (actualSource == expectedSource)
            (throwError $ annotateError (getLocation ann) (EMismatchedBoxSource expectedSource actualSource [getLocation prevAnn]))
        -- | This should not happen, since all the ports of the task
        -- must be connected to something
        Nothing -> throwError $ annotateError Internal EUnboxingTask
checkTaskSourceInBox expectedSource task prevAnn (InBoxInput port) = do
    -- | The box was received from a port connected to a channel. We must
    -- obtain the name of the channel to which the port is connected and then
    -- obtain the source of the box from the box-channel map
    progArchitecture <- ask
    case M.lookup port (taskInputPortConns task) of
        Nothing -> throwError $ annotateError Internal EUnboxingTask
        Just (channel, _) -> case M.lookup channel (channels progArchitecture) of
            Just nextChannel -> checkNextSource (getLocation prevAnn) (getBoxSourceChannel expectedSource nextChannel)
            Nothing -> throwError $ annotateError Internal EUnboxingTask
-- | The rest of the cases should not happen, since tasks do not have procedures
checkTaskSourceInBox _ _ _ _ = throwError $ annotateError Internal EUnboxingTask

checkHandlerSourceInBox ::
    Identifier -- ^ Expected source of the box
    -> TPHandler SemanticAnn
    -> SemanticAnn -> InBox SemanticAnn
    -> BoxSourcesCheckMonad ()
checkHandlerSourceInBox expectedSource handler prevAnn (InBoxAlloc port ann) =
    -- We reached a terminal allocator port. Now we only have to get the
    -- allocator source and we are done
    case M.lookup port (handlerAPConnections handler) of
        Just (actualSource, _) -> unless (actualSource == expectedSource)
            (throwError $ annotateError (getLocation ann) (EMismatchedBoxSource expectedSource actualSource [getLocation prevAnn]))
        -- | This should not happen, since all the ports of the handler
        -- must be connected to something
        Nothing -> throwError $ annotateError Internal EUnboxingHandler
checkHandlerSourceInBox _ _ _ _ = throwError $ annotateError Internal EUnboxingHandler

checkBoxSourceResourceFree ::
    Identifier -- ^ Expected source of the box
    -> TPResource SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxSourcesCheckMonad ()
checkBoxSourceResourceFree expectedSource resource outPt = do
    progArchitecture <- ask
    let resourceCls = resourceClasses progArchitecture M.! resourceClass resource
    forM_ (M.lookup outPt (outBoxFree . classBoxIOMaps $ resourceCls)) $
        mapM_ (uncurry (checkResourceSourceInBox expectedSource resource))

checkBoxSourceTaskFree ::
    Identifier -- ^ Expected source of the box
    -> TPTask SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxSourcesCheckMonad ()
checkBoxSourceTaskFree expectedSource task calledPort = do
    progArchitecture <- ask
    let taskCls = taskClasses progArchitecture M.! taskClass task
    -- | If the port is not on the outBoxFree map, it means that the task only
    -- allocates the box and does not free it
    forM_ (M.lookup calledPort (outBoxFree . classBoxIOMaps $ taskCls)) $ 
        mapM_ (uncurry (checkTaskSourceInBox expectedSource task))

checkBoxSourceHandlerFree ::
    Identifier -- ^ Expected source of the box
    -> TPHandler SemanticAnn
    -> Identifier -- ^ The name of the port connected to the allocator where the box is to be freed
    -> BoxSourcesCheckMonad ()
checkBoxSourceHandlerFree expectedSource handler calledPort = do
    progArchitecture <- ask
    let handlerCls = handlerClasses progArchitecture M.! handlerClass handler
    -- | If the port is not on the outBoxFree map, it means that the handler only
    -- allocates the box and does not free it
    forM_ (M.lookup calledPort (outBoxFree . classBoxIOMaps $ handlerCls)) $
        mapM_ (uncurry (checkHandlerSourceInBox expectedSource handler))

checkBoxSourceFree ::
    Identifier -- ^ Expected source of the box
    -> Identifier -- ^ The name of the element (task, handler or resource)
    -> Identifier -- ^ The name of the access port
    -> BoxSourcesCheckMonad ()
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


checkBoxSource :: TPPool SemanticAnn -> BoxSourcesCheckMonad ()
checkBoxSource (TPPool poolName _ _ _ _) = do
    progArchitecture <- ask
    case M.lookup poolName (resourceSources progArchitecture) of
        Just [] -> throwError $ annotateError Internal EUnboxingResource
        Just callers -> do
            mapM_ (\(caller, accessPt, _cann) -> checkBoxSourceFree poolName caller accessPt) callers
        -- | This means that the resource is not connected to anything (this shouuld not happen)
        Nothing -> throwError $ annotateError Internal EUnboxingPool

checkBoxSources :: BoxSourcesCheckMonad ()
checkBoxSources = do
    progArchitecture <- ask
    mapM_ checkBoxSource (M.elems (pools progArchitecture))
