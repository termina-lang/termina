{-# LANGUAGE FlexibleContexts #-}
module ControlFlow.ConstFolding.Utils where

import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import Semantic.AST
import Semantic.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import qualified Data.Map as M

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError ConstFoldError m) => Object SemanticAnn -> m (TerminaType SemanticAnn)
getObjType (Variable _ (SemanticAnn (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (ArrayIndexExpression _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))    = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))            = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ ts)) _))     = return ts
getObjType (Dereference _ (SemanticAnn (ETy (ObjectType _ ts)) _))               = return ts
getObjType (Unbox _ (SemanticAnn (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _)) = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ ts)) _)) = return ts
getObjType _ = throwError $ annotateError Internal EUnboxingObject

-- | This function returns the type of an expression. The type is extracted from the
-- expression's semantic annotation. The function assumes that the expression is well-typed
-- and that the semantic annotation is correct. If the expression is not well-typed, the
-- function will throw an error.
getExprType :: (MonadError ConstFoldError m) => Expression SemanticAnn -> m (TerminaType SemanticAnn)
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (OptionVariantInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (FunctionCall _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (StructInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (StringInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType _ = throwError $ annotateError Internal EUnboxingExpression

-- | This function returns the name of a port. The function assumes that the object is
-- a port and that the object is well-typed. If the object is not a port or if the object
-- is not well-typed, the function will throw an error.    
getPortName :: (MonadError ConstFoldError m) => Object SemanticAnn -> m Identifier
getPortName obj = do
    obj_type <- getObjType obj
    case obj_type of 
        TAccessPort _ -> 
            case obj of
                (MemberAccess _ portName _) -> return portName
                (DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ annotateError Internal EUnboxingPort
        TOutPort _ -> 
            case obj of
                (MemberAccess _ portName _) -> return portName
                (DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ annotateError Internal EUnboxingPort
        _ -> throwError $ annotateError Internal EUnboxingPort

getFunctionMembers :: (MonadError ConstFoldError m) => 
    TerminaProgArch SemanticAnn -> Identifier -> m (M.Map Identifier (TPFunction SemanticAnn))
getFunctionMembers progArchitecture ident =
    case M.lookup ident (tasks progArchitecture) of
        Just task -> 
            maybe (throwError $ annotateError Internal (EUnknownTaskClass (taskClass task)))
                    (return . classMemberFunctions) $ 
                        M.lookup (taskClass task) (taskClasses progArchitecture)
        Nothing -> 
            case M.lookup ident (handlers progArchitecture) of
                Just handler -> 
                    maybe (throwError $ annotateError Internal (EUnknownHandlerClass (handlerClass handler)))
                            (return . classMemberFunctions) $ 
                                M.lookup (handlerClass handler) (handlerClasses progArchitecture)
                Nothing -> throwError $ annotateError Internal (EUnknownIdentifier ident)

followSendMessage :: (MonadError ConstFoldError m) =>
    TerminaProgArch SemanticAnn -> Identifier -> Identifier -> m (Identifier, TPFunction SemanticAnn)
followSendMessage progArchitecture currentId portName = do
    case M.lookup currentId (tasks progArchitecture) of
        Just currentTask -> do
            channel <- maybe (throwError $ annotateError Internal (EUnknownTask currentId)) (return . fst) $ 
                            M.lookup portName (taskOutputPortConns currentTask) 
            (targetTaskId, targetPort, _) <- 
                        maybe (throwError $ annotateError Internal (EUnknownChannel channel)) return $ 
                            M.lookup channel (channelTargets progArchitecture)
            targetTask <- maybe (throwError $ annotateError Internal (EUnknownTask targetTaskId)) return $ 
                            M.lookup targetTaskId (tasks progArchitecture)
            targetTaskCls <- maybe (throwError $ annotateError Internal (EUnknownTaskClass (taskClass targetTask))) return $ 
                            M.lookup (taskClass targetTask) (taskClasses progArchitecture)
            targetAction <- maybe (throwError $ annotateError Internal (EUnknownInputPort targetTaskId targetPort)) (return . snd) $ 
                            M.lookup targetPort (inputPorts targetTaskCls)
            actionFunction <- maybe (throwError $ annotateError Internal (EUnknownMemberFunction targetAction)) return $ 
                            M.lookup targetAction (classMemberFunctions targetTaskCls)
            return (targetTaskId, actionFunction)
        Nothing -> 
            case M.lookup currentId (handlers progArchitecture) of
                Just currentHandler -> do
                    channel <- maybe (throwError $ annotateError Internal (EUnknownHandler currentId)) (return . fst) $ M.lookup portName (handlerOutputPortConns currentHandler)
                    (targetTaskId, targetPort, _) <- 
                                maybe (throwError $ annotateError Internal (EUnknownChannel channel)) return $ 
                                    M.lookup channel (channelTargets progArchitecture)
                    targetTask <- maybe (throwError $ annotateError Internal (EUnknownTask targetTaskId)) return $ 
                                    M.lookup targetTaskId (tasks progArchitecture)
                    targetTaskCls <- maybe (throwError $ annotateError Internal (EUnknownTaskClass (taskClass targetTask))) return $ 
                                    M.lookup (taskClass targetTask) (taskClasses progArchitecture)
                    targetAction <- maybe (throwError $ annotateError Internal (EUnknownInputPort targetTaskId targetPort)) (return . snd) $ 
                                    M.lookup targetPort (inputPorts targetTaskCls)
                    actionFunction <- maybe (throwError $ annotateError Internal (EUnknownMemberFunction targetAction)) return $ 
                                    M.lookup targetAction (classMemberFunctions targetTaskCls)
                    return (targetTaskId, actionFunction)
                Nothing -> throwError $ annotateError Internal (EUnknownIdentifier currentId)

followProcedureCall :: (MonadError ConstFoldError m) =>
    TerminaProgArch SemanticAnn -> Identifier -> Identifier -> Identifier -> m (Identifier, TPFunction SemanticAnn)
followProcedureCall progArchitecture ident portName procName = do
  case M.lookup ident (tasks progArchitecture) of
    Just task -> do
        (targetResource, _) <- maybe (throwError $ annotateError Internal (EUnknownAccessPort ident portName)) return $ M.lookup portName (taskAPConnections task)
        resource <- maybe (throwError $ annotateError Internal (EUnknownResource targetResource)) return $ M.lookup targetResource (resources progArchitecture)
        resourceCls <- maybe (throwError $ annotateError Internal (EUnknownResourceClass (resourceClass resource))) return $ M.lookup (resourceClass resource) (resourceClasses progArchitecture)
        resourceProc <- maybe (throwError $ annotateError Internal (EUnknownResourceProcedure targetResource procName)) return $ M.lookup procName (classMemberFunctions resourceCls)
        return (targetResource, resourceProc)
    Nothing -> 
      case M.lookup ident (handlers progArchitecture) of
        Just handler -> do
            (targetResource, _) <- maybe (throwError $ annotateError Internal (EUnknownAccessPort ident portName)) return $ M.lookup portName (handlerAPConnections handler)
            resource <- maybe (throwError $ annotateError Internal (EUnknownResource targetResource)) return $ M.lookup targetResource (resources progArchitecture)
            resourceCls <- maybe (throwError $ annotateError Internal (EUnknownResourceClass (resourceClass resource))) return $ M.lookup (resourceClass resource) (resourceClasses progArchitecture)
            resourceProc <- maybe (throwError $ annotateError Internal (EUnknownResourceProcedure targetResource procName)) return $ M.lookup procName (classMemberFunctions resourceCls)
            return (targetResource, resourceProc)
        Nothing ->
            case M.lookup ident (resources progArchitecture) of
              Just resource -> do
                    (targetResource, _) <- maybe (throwError $ annotateError Internal (EUnknownAccessPort ident portName)) return $ M.lookup portName (resAPConnections resource)
                    resource' <- maybe (throwError $ annotateError Internal (EUnknownResource targetResource)) return $ M.lookup targetResource (resources progArchitecture)
                    resourceCls <- maybe (throwError $ annotateError Internal (EUnknownResourceClass (resourceClass resource'))) return $ M.lookup (resourceClass resource') (resourceClasses progArchitecture)
                    resourceProc <- maybe (throwError $ annotateError Internal (EUnknownResourceProcedure targetResource procName)) return $ M.lookup procName (classMemberFunctions resourceCls)
                    return (targetResource, resourceProc)
              Nothing -> throwError $ annotateError Internal (EUnknownIdentifier ident)