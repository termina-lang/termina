{-# LANGUAGE FlexibleContexts #-}

module ControlFlow.BasicBlocks.Utils (
    getObjType,
    getExprType,
    getPortName
) where

import qualified Semantic.AST as SAST
import Core.AST
import Semantic.Types
import Utils.Annotations
import Control.Monad.Except
import ControlFlow.BasicBlocks.Errors.Errors

-- This module contains utility functions that are used in the control flow analysis.

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError BBGeneratorError m) => SAST.Object SemanticAnn -> m TerminaType
getObjType (SAST.Variable _ (Located (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (SAST.ArrayIndexExpression _ _ (Located (ETy (ObjectType _ ts)) _))    = return ts
getObjType (SAST.MemberAccess _ _ (Located (ETy (ObjectType _ ts)) _))            = return ts
getObjType (SAST.Dereference _ (Located (ETy (ObjectType _ ts)) _))               = return ts
getObjType (SAST.Unbox _ (Located (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (SAST.DereferenceMemberAccess _ _ (Located (ETy (ObjectType _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

-- | This function returns the type of an expression. The type is extracted from the
-- expression's semantic annotation. The function assumes that the expression is well-typed
-- and that the semantic annotation is correct. If the expression is not well-typed, the
-- function will throw an error.
getExprType :: (MonadError BBGeneratorError m) => SAST.Expression SemanticAnn -> m TerminaType
getExprType (SAST.AccessObject obj) = getObjType obj
getExprType (SAST.Constant _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.OptionVariantInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.BinOp _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ReferenceExpression _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.Casting _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.FunctionCall _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.MemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.DerefMemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (SAST.StructInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.EnumVariantInitializer _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ArrayInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (SAST.ArrayExprListInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

-- | This function returns the name of a port. The function assumes that the object is
-- a port and that the object is well-typed. If the object is not a port or if the object
-- is not well-typed, the function will throw an error.    
getPortName :: (MonadError BBGeneratorError m) => SAST.Object SemanticAnn -> m Identifier
getPortName obj = do
    obj_type <- getObjType obj
    case obj_type of 
        AccessPort _ -> 
            case obj of
                (SAST.MemberAccess _ portName _) -> return portName
                (SAST.DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ InternalError ("unexpected object type" ++ show obj_type)
        OutPort _ -> 
            case obj of
                (SAST.MemberAccess _ portName _) -> return portName
                (SAST.DereferenceMemberAccess _ portName _) -> return portName
                _ -> throwError $ InternalError ("unexpected object type" ++ show obj_type)
        _ -> throwError $ InternalError "object is not an accessible port"