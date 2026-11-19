-- | Semantic Analysis Module i.e. Type checking
-- This module defines a mapping from |AST ParserAnn|
-- to |AST SemanticAnnotations|, | SemanAST {ParserInfo , TypeInfo}|

module Semantic.TypeChecking (
  runTypeChecking, typeTerminaModule
) where

-- Termina Ast and Utils
import Utils.Annotations
import Parser.AST as PAST

-- Termina Semantic AST
import qualified Semantic.AST as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
import Control.Monad
import Control.Monad.Except
import Semantic.Errors
-- Semantic Monad
import Semantic.Monad

import Data.Maybe

import qualified Control.Monad.State.Strict as ST
import Parser.Types
import Semantic.TypeChecking.Statement
import Semantic.TypeChecking.Global
import Semantic.TypeChecking.TypeDefinition
    ( typeParameter, typeTypeDefinition )
import Semantic.TypeChecking.Expression
import Semantic.Modifiers (Declaration(..))
import Semantic.Environment
import Utils.Monad
import qualified Data.Set as S

typeElement :: AnnASTElement ParserAnn
  -> SemanticMonad (SAST.AnnASTElement SemanticAnn)
typeElement (Function ident ps_ts mts bret mds_ts anns) = do
  ----------------------------------------
  -- Check the return type 
  mty <- mapM (typeTypeSpecifier (getLocation anns) typeGlobalObject >=>
      (\ty -> checkReturnType (getLocation anns) ty >> return ty)) mts
  (ps_ty, typedBret) <- localScope $ do
      ps_ty <- forM ps_ts (\param@(Parameter paramId _ pann) -> do
          typedParam <- typeParameter (getLocation anns) param
          insertLocalImmutObj (getLocation pann) paramId (paramType typedParam)
          return typedParam)
      typedBret <- typeBlock mty bret
      return (ps_ty, typedBret)
  mds_ty <- typeModifiers (getLocation anns) FunctionDeclaration typeGlobalObject mds_ts
  let functionSeman = FunctionSeman ps_ty (fromMaybe TUnit mty) Returns
  return (Function ident ps_ty mty typedBret mds_ty (SemanticAnn (FnTy functionSeman) anns))
typeElement (ExternFunction ident ps_ts mts mode mds_ts anns) = do
  -- A function that never comes back to the caller has nothing to return
  when (mode == NoReturn && isJust mts) $
    throwError $ annotateError (getLocation anns) (ENoReturnFunctionReturnType ident)
  -- Check the return type and the parameters with the rules of a function
  mty <- mapM (typeTypeSpecifier (getLocation anns) typeGlobalObject >=>
      (\ty -> checkReturnType (getLocation anns) ty >> return ty)) mts
  ps_ty <- localScope $ forM ps_ts (\param@(Parameter paramId _ pann) -> do
      typedParam <- typeParameter (getLocation anns) param
      insertLocalImmutObj (getLocation pann) paramId (paramType typedParam)
      return typedParam)
  mds_ty <- typeModifiers (getLocation anns) ExternFunctionDeclaration typeGlobalObject mds_ts
  let functionSeman = FunctionSeman ps_ty (fromMaybe TUnit mty) mode
  return (ExternFunction ident ps_ty mty mode mds_ty (SemanticAnn (FnTy functionSeman) anns))
typeElement (GlobalDeclaration gbl) = GlobalDeclaration <$> typeGlobal gbl
typeElement (TypeDefinition tydef ann) = do
  typed_tydef <- typeTypeDefinition (getLocation ann) tydef
  return (TypeDefinition typed_tydef (buildTypeAnn ann))

-- | Add a typed element to the global environment, as its declaration.
addElement :: SAST.AnnASTElement SemanticAnn -> SemanticMonad ()
addElement el = insertGlobal name (globalDecl el) (usedName name)

  where

    name = globalDeclName el
    usedName = case el of
      Function {} -> EUsedFunName
      ExternFunction {} -> EUsedFunName
      GlobalDeclaration {} -> EUsedGlobalName
      TypeDefinition {} -> EUsedTypeName

typeTerminaModule ::
  S.Set QualifiedName
  -> AnnotatedProgram ParserAnn
  -> SemanticMonad (SAST.AnnotatedProgram SemanticAnn)
typeTerminaModule visibleModules prog = do
  ST.modify (\st -> st { visible = visibleModules })
  mapM typeAndAdd prog

  where
    typeAndAdd :: AnnASTElement ParserAnn -> SemanticMonad (SAST.AnnASTElement SemanticAnn)
    typeAndAdd el = do
      typed_el <- typeElement el
      addElement typed_el
      return typed_el

runTypeChecking
  :: Environment
  -> SemanticMonad a
  -> Either SemanticErrors (a, Environment)
runTypeChecking initSt m = case flip ST.runState initSt . runExceptT $ m of
  (Left err, _) -> Left err
  (Right output, st) -> Right (output, st)
