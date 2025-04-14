-- | Semantic Analysis Module i.e. Type checking
-- This module defines a mapping from |AST ParserAnn|
-- to |AST SemanticAnnotations|, | SemanAST {ParserInfo , TypeInfo}|

module Semantic.TypeChecking (
  runTypeChecking, typeTerminaModule
) where

-- Termina Ast and Utils
import Utils.Annotations
import Parser.AST as PAST
import Core.Utils

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

import qualified Data.List  (map)
import Data.Maybe

import qualified Control.Monad.State.Strict as ST
import Parser.Types
import Semantic.TypeChecking.Statement
import Semantic.TypeChecking.Global
import Semantic.TypeChecking.TypeDefinition
import Semantic.TypeChecking.Expression
import Data.Bifunctor
import Semantic.Environment
import Utils.Monad

typeElement :: AnnASTElement ParserAnn
  -> SemanticMonad (SAST.AnnASTElement SemanticAnn, LocatedElement (GEntry SemanticAnn))
typeElement (Function ident ps_ts mts bret mds_ts anns) = do
  ----------------------------------------
  -- Check the return type 
  mty <- maybe (return Nothing) (typeTypeSpecifier anns typeGlobalObject >=>
      (\ty -> checkReturnType anns ty >> return (Just ty))) mts
  (ps_ty, typedBret) <- localScope $ do
      ps_ty <- forM ps_ts (\param@(Parameter paramId _) -> do
          typedParam <- typeParameter anns param
          insertLocalImmutObj anns paramId (paramType typedParam)
          return typedParam)
      typedBret <- typeBlock mty bret
      return (ps_ty, typedBret)
  mds_ty <- mapM (typeModifier anns typeGlobalObject) mds_ts
  let functionSeman = FunctionSeman (map paramType ps_ty) (fromMaybe TUnit mty)
  return (Function ident ps_ty mty typedBret mds_ty (SemanticAnn (FnTy functionSeman) anns), LocatedElement (GFun functionSeman) anns)
typeElement (GlobalDeclaration gbl) = first GlobalDeclaration <$> typeGlobal gbl
typeElement (TypeDefinition tydef ann) = do
  typed_tydef <- typeTypeDefinition ann tydef
  case typed_tydef of
    Struct {} -> return (TypeDefinition typed_tydef (buildTypeAnn ann), LocatedElement (GType (semanticTypeDef typed_tydef)) ann)
    Enum {} -> return (TypeDefinition typed_tydef (buildTypeAnn ann), LocatedElement (GType (semanticTypeDef typed_tydef)) ann)
    (Class _clsKind _ _ _ _) -> return (TypeDefinition typed_tydef (buildTypeAnn ann), LocatedElement (GType (semanticTypeDef typed_tydef)) ann)
    (Interface {}) -> do
      return (TypeDefinition typed_tydef (buildTypeAnn ann), LocatedElement (GType (semanticTypeDef typed_tydef)) ann)

semanticTypeDef :: SAST.TypeDef SemanticAnn -> SemanTypeDef SemanticAnn
semanticTypeDef (Struct i f m)  = Struct i f m
semanticTypeDef (Enum i e m)    = Enum i e m
semanticTypeDef (Class kind i cls ps m) = Class kind i (Data.List.map kClassMember cls) ps m
semanticTypeDef (Interface kind i extends cls m) = Interface kind i extends cls m

-- Adding Global elements to the environment.
addElement :: SAST.AnnASTElement SemanticAnn -> LocatedElement (GEntry SemanticAnn) -> SemanticMonad ()
addElement (Function ident _ _ _ _ _) el =
  insertGlobal ident el (EUsedFunName ident)
addElement (GlobalDeclaration glb) el =
  let global_name = getGlobalIdentifier glb in
  insertGlobal global_name el (EUsedGlobalName global_name)
addElement (TypeDefinition ty _) el =
  let type_name = getTypeIdentifier ty in
  insertGlobal type_name el (EUsedTypeName type_name)

typeTerminaModule ::
  AnnotatedProgram ParserAnn
  -> SemanticMonad (SAST.AnnotatedProgram SemanticAnn)
typeTerminaModule = mapM typeAndAdd

  where
    typeAndAdd :: AnnASTElement ParserAnn -> SemanticMonad (SAST.AnnASTElement SemanticAnn)
    typeAndAdd el = do
      (typed_el, entry) <- typeElement el
      addElement typed_el entry
      return typed_el

runTypeChecking
  :: Environment
  -> SemanticMonad a
  -> Either SemanticErrors (a, Environment)
runTypeChecking initSt m = case flip ST.runState initSt . runExceptT $ m of
  (Left err, _) -> Left err
  (Right output, st) -> Right (output, st)
