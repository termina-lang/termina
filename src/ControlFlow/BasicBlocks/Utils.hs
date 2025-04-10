{-# LANGUAGE FlexibleContexts #-}

module ControlFlow.BasicBlocks.Utils (
    getObjType,
    getExprType,
    getPortName
) where

import qualified Semantic.AST as SAST
import Core.AST
import Semantic.Types
import Control.Monad.Except
import ControlFlow.BasicBlocks.Errors

-- This module contains utility functions that are used in the control flow analysis.

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError BBGeneratorError m) => SAST.Object SemanticAnn -> m (SAST.TerminaType SemanticAnn)
getObjType (SAST.Variable _ (SemanticAnn (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (SAST.ArrayIndexExpression _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))    = return ts
getObjType (SAST.MemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))            = return ts
getObjType (SAST.MemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ ts)) _))     = return ts
getObjType (SAST.Dereference _ (SemanticAnn (ETy (ObjectType _ ts)) _))               = return ts
getObjType (SAST.Unbox _ (SemanticAnn (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (SAST.DereferenceMemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _)) = return ts
getObjType (SAST.DereferenceMemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

-- | This function returns the type of an expression. The type is extracted from the
-- expression's semantic annotation. The function assumes that the expression is well-typed
-- and that the semantic annotation is correct. If the expression is not well-typed, the
-- function will throw an error.
getExprType :: (MonadError BBGeneratorError m) => SAST.Expression SemanticAnn -> m (SAST.TerminaType SemanticAnn)
getExprType (SAST.AccessObject obj) = getObjType obj
getExprType (SAST.Constant _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.OptionVariantInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.BinOp _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ReferenceExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.Casting _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.FunctionCall _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.MemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.DerefMemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.StructInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.EnumVariantInitializer _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ArrayInitializer _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ArrayExprListInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

-- | This function returns the name of a port. The function assumes that the object is
-- a port and that the object is well-typed. If the object is not a port or if the object
-- is not well-typed, the function will throw an error.    
getPortName :: (MonadError BBGeneratorError m) => SAST.Object SemanticAnn -> m Identifier
getPortName obj = do
    obj_type <- getObjType obj
    case obj_type of 
        TAccessPort _ -> 
            case obj of
                (SAST.MemberAccess _ portName _) -> return portName
                (SAST.DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ InternalError ("unexpected object type" ++ show obj_type)
        TOutPort _ -> 
            case obj of
                (SAST.MemberAccess _ portName _) -> return portName
                (SAST.DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ InternalError ("unexpected object type" ++ show obj_type)
        _ -> throwError $ InternalError "object is not an accessible port"