module Semantic.TypeChecking.Global where

import Utils.Annotations
import Parser.AST as PAST

-- Termina Semantic AST
import qualified Semantic.AST as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
import Control.Monad
-- Error module
import Control.Monad.Except
import Semantic.Errors
-- Semantic Monad
import Semantic.Monad


import Parser.Types
import Semantic.TypeChecking.Expression
import Core.Utils

typeGlobal :: Global ParserAnn -> SemanticMonad (SAST.Global SemanticAnn, LocatedElement (GEntry SemanticAnn))
typeGlobal (Task ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  case ty of
    TGlobal TaskClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      unless (any (\(SAST.Modifier modIdent _) -> modIdent == "priority") tyMods) $
        throwError $ annotateError anns (EMissingTaskPriority ident)
      return (SAST.Task ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    _ -> throwError $ annotateError anns (EInvalidTaskType ty)
typeGlobal (Handler ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  case ty of
    TGlobal HandlerClass _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Handler ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    _ -> throwError $ annotateError anns (EInvalidHandlerType ty)
typeGlobal (Test ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  case ty of
    TGlobal TestClass _ -> do
      exprty <-
        case mexpr of
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Test ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    _ -> throwError $ annotateError anns (EInvalidTestType ty)
typeGlobal (Resource ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  case ty of
    -- | User-defined resources
    TGlobal ResourceClass clsId -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      -- Check if the resource is unprotected, and if it is, check that it has no regular fields
      when (isUnprotected tyMods) $ 
          getGlobalTypeDef anns clsId >>= \case{
              LocatedElement (Class _clsKind _ident members _provides _mods) clsLoc ->
                let fields = [fld | ClassField fld@(FieldDefinition _ fty _) <- members, (not . portTy) fty] in
                unless (null fields) $ throwError $ annotateError anns (EUnprotectedResourceWithRegularFields (clsId, clsLoc));
              _ -> throwError $ annotateError Internal EExpectedClassType;
            } 
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Memory pools 
    TPool {} -> do
      exprty <-
        case mexpr of
          Just _ -> throwError $ annotateError anns EInvalidPoolInitialization
          Nothing   -> return Nothing
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Deques, which always start empty
    TDeque {} -> do
      exprty <-
        case mexpr of
          Just _ -> throwError $ annotateError anns EDequeInitialized
          Nothing   -> return Nothing
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Pools of a unit test, which start empty like any pool
    TTestPool {} -> do
      exprty <-
        case mexpr of
          Just _ -> throwError $ annotateError anns (ETestComponentInitialized ty)
          Nothing   -> return Nothing
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Atomic variables
    TAtomic {} -> do
      exprty <-
        case mexpr of
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Atomic arrays
    TAtomicArray {} -> do
      exprty <-
        case mexpr of
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Resource ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    _ -> throwError $ annotateError anns (EInvalidResourceType ty)
  
  where

    isUnprotected :: [SAST.Modifier a] -> Bool
    isUnprotected [] = False
    isUnprotected ((Modifier "unprotected" Nothing) : _) = True
    isUnprotected (_ : xs) = isUnprotected xs

typeGlobal (Emitter ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  case ty of
    TGlobal EmitterClass clsId -> do
      unless (emitterInstTy clsId) (throwError $ annotateError anns (EEmitterClassNotInstantiable clsId))
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just expr -> Just <$> typeAssignmentExpression ty typeGlobalObject expr
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> Just <$> typeAssignmentExpression ty typeGlobalObject (StructInitializer [] Nothing anns)
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Emitter ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Emitters of a unit test, which only a case fires
    TTestEmitter {} -> do
      exprty <-
        case mexpr of
          Just _ -> throwError $ annotateError anns (ETestComponentInitialized ty)
          Nothing -> return Nothing
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Emitter ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    _ -> throwError $ annotateError anns (EInvalidEmitterType ty)
typeGlobal (Channel ident ts mexpr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  case ty of
    TMsgQueue _ _ -> do
      exprty <-
        case mexpr of
          -- If it has an initial value great
          Just _ -> throwError $ annotateError anns EInvalidMsgQueueInitialization
          -- If it has not, we need to check that the type has actually NO fields
          Nothing -> return Nothing
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Channel ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    -- | Channels of a unit test, which a case reads
    TTestChannel {} -> do
      exprty <-
        case mexpr of
          Just _ -> throwError $ annotateError anns (ETestComponentInitialized ty)
          Nothing -> return Nothing
      tyMods <- typeModifiers anns typeGlobalObject mods
      return (SAST.Channel ident ty exprty tyMods (buildGlobalAnn anns ty), LocatedElement (GGlob ty) anns)
    _ -> throwError $ annotateError anns (EInvalidChannelType ty)
typeGlobal (Const ident ts expr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  globalConstTyOrFail anns ty
  -- | A constant has the type TConstSubtype of the declared one, which marks
  -- it, and every scalar read from it with a constant path, as a constant
  -- expression. Its initializer is typed against that type, which makes every
  -- field and element of it a constant too.
  let constType = TConstSubtype ty
  typed_expr <- typeAssignmentExpression constType typeGlobalObject expr
  tyMods <- typeModifiers anns typeGlobalObject mods
  return (SAST.Const ident constType typed_expr tyMods (buildGlobalAnn anns constType), LocatedElement (GConst constType typed_expr) anns)
typeGlobal (ConstExpr ident ts expr mods anns) = do
  ty <- typeTypeSpecifier anns typeGlobalObject ts
  checkTerminaType anns ty
  declTyOrFail anns ty
  typed_expr <- typeAssignmentExpression ty typeGlobalObject expr
  tyMods <- typeModifiers anns typeGlobalObject mods
  return (SAST.ConstExpr ident ty typed_expr tyMods (buildGlobalAnn anns ty), LocatedElement (GConstExpr ty typed_expr) anns)
