module EFP.Schedulability.MAST.Utils where
import EFP.Schedulability.Core.AST
import EFP.Schedulability.MAST.Monad
import qualified Data.Map.Strict as M
import Control.Monad.Except
import ControlFlow.Architecture.Types
import Utils.Annotations
import EFP.Schedulability.MAST.Errors
import Control.Monad.State

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) i1 i2 = i1 ++ "__" ++ i2

getEmitterHandlerName :: Identifier -> Identifier
getEmitterHandlerName emitterId = "__eh" <::> emitterId

getExternalEventName :: Identifier -> Identifier
getExternalEventName emitterId = "__ext_evt" <::> emitterId

getInternalEventName :: Identifier -> Identifier -> Identifier
getInternalEventName targetComponent targetAction = "__int_evt" <::> targetComponent <::> targetAction

getOperationName :: Identifier -> Identifier -> Identifier
getOperationName targetComponent targetAction = "__op" <::> targetComponent <::> targetAction

getTargetAction :: Identifier -> Identifier -> MASTGenMonad Identifier
getTargetAction componentName sinkPort = do
    arch <- gets progArch
    cmpCls <- case M.lookup componentName (tasks arch) of
            Just tsk -> return $ taskClasses arch M.! taskClass tsk
            Nothing -> case M.lookup componentName (handlers arch) of
                Just hdl -> return $ handlerClasses arch M.! handlerClass hdl
                Nothing -> throwError . annotateError Internal $ EUnknownComponent componentName
    case M.lookup sinkPort (sinkPorts cmpCls) of
        Just (_, actionId) -> return actionId
        Nothing -> throwError . annotateError Internal $ EUnknownSinkPort componentName sinkPort
