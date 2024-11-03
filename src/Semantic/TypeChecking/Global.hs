module Semantic.TypeChecking.Global where

import Utils.Annotations
import Parser.AST as PAST

-- Termina Semantic AST
import qualified Semantic.AST as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
-- Error module
import Control.Monad.Except
import Semantic.Errors.Errors
-- Semantic Monad
import Semantic.Monad


import Parser.Types
import Semantic.TypeChecking.Expression
import Semantic.TypeChecking.Check

typeGlobal :: Global ParserAnn -> SemanticMonad (SAST.Global SemanticAnn, Located (GEntry SemanticAnn))
typeGlobal (Task ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns ts
  checkTerminaType anns ty
  case ty of
    TGlobal TaskClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Task ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    _ -> throwError $ annotateError anns EUnboxingClassType
typeGlobal (Handler ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns ts
  checkTerminaType anns ty
  case ty of
    TGlobal HandlerClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Handler ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    _ -> throwError $ annotateError anns EUnboxingClassType
typeGlobal (Resource ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns ts
  checkTerminaType anns ty
  case ty of
    -- | User-defined resources
    TGlobal ResourceClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    -- | Memory pools 
    TPool _ _ -> do
      exprty <-
        case mexpr of
          Just _ -> throwError $ annotateError anns EInvalidPoolInitialization
          Nothing   -> return Nothing
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    -- | Atomic variables 
    TAtomic ty' -> do
      exprty <-
        case mexpr of
          Just expr -> Just <$> typeAssignmentExpression ty' typeGlobalObject expr
          Nothing   -> throwError $ annotateError anns EAtomicUninitialized
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    -- | Atomic arrays
    TAtomicArray ty' size -> do
      exprty <-
        case mexpr of
          Just expr -> Just <$> typeAssignmentExpression (TArray ty' size) typeGlobalObject expr
          Nothing -> throwError $ annotateError anns EAtomicArrayUninitialized
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    _ -> throwError $ annotateError anns EUnboxingClassType
typeGlobal (Emitter ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns ts
  checkTerminaType anns ty
  case ty of
    TGlobal EmitterClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Emitter ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    _ -> throwError $ annotateError anns EUnboxingClassType
typeGlobal (Channel ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns ts
  checkTerminaType anns ty
  case ty of
    TGlobal ChannelClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Channel ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    TMsgQueue _ _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just _ -> throwError $ annotateError anns EInvalidMsgQueueInitialization
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> return Nothing
      tyMods <- mapM (typeModifier anns) mods
      return (SAST.Channel ident ty exprty tyMods (buildGlobalAnn anns ty), Located (GGlob ty) anns)
    _ -> throwError $ annotateError anns EUnboxingClassType
typeGlobal (Const ident ts expr mods anns) = do
  ty <- typeTypeSpecifier anns ts
  checkTerminaType anns ty
  typed_expr <- typeConstExpression ty expr
  value <- evalConstExpression ty expr
  tyMods <- mapM (typeModifier anns) mods
  return (SAST.Const ident ty typed_expr tyMods (buildGlobalAnn anns ty), Located (GConst ty value) anns)
