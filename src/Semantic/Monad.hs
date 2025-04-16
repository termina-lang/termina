{-# LANGUAGE LambdaCase    #-}

-- | Semantic Monad.
-- Here lives the monad we use to handle semantic pass effects.
-- It should be something like Exceptions + State

module Semantic.Monad where

import Data.Map as M

-- AST Info
import Utils.Annotations
import Core.AST as CAST
import Semantic.AST

import Semantic.Errors
import Semantic.Types
import Semantic.Environment
import Core.Utils

-- Monads
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.State.Strict as ST
import qualified Parser.Types as Parser
import Utils.Monad

import Control.Monad.State

type SemanticMonad = ExceptT SemanticErrors (ST.State Environment)

----------------------------------------

----------------------------------------
-- Monadic helpers.

-- | Execute computations in a temporal state wihtout
-- modifying current state.
withInState :: Environment -> SemanticMonad a -> SemanticMonad a
withInState tempState comp = localScope (put tempState >> comp)

----------------------------------------
-- Some helper functions to bring information from the environment.

-- | Get global definition of a Type
getGlobalTypeDef :: Location -> Identifier -> SemanticMonad (LocatedElement (SemanTypeDef SemanticAnn))
getGlobalTypeDef loc tid  = gets global >>=
  maybe
  -- if there is no varialbe name |tid|
  (throwError $ annotateError loc (ENoTypeFound tid))
  -- if so, return its type
  (\case {
      LocatedElement (GType tydef) tyloc -> return (LocatedElement tydef tyloc);
      LocatedElement _ entryLoc -> throwError $ annotateError loc (EGlobalNotType (tid, entryLoc))
      }) . M.lookup tid

getFunctionTy :: Location 
  -> Identifier 
  -> SemanticMonad ([Parameter SemanticAnn], TerminaType SemanticAnn, Location)
getFunctionTy loc iden =
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError loc (EFunctionNotFound iden))
  >>= \case
    LocatedElement (GFun (FunctionSeman args retty)) entryLoc -> return (args, retty, entryLoc)
    LocatedElement _ entryLoc -> throwError $ annotateError loc (EGlobalNotFunction (iden, entryLoc))

-- | Get the type definition of an enum type from the global environment.
-- This function is only called for a type that we know is an enum type. If
-- there is an error it is an internal error.
getEnumTy :: Location -> Identifier -> SemanticMonad (LocatedElement (SemanTypeDef SemanticAnn))
getEnumTy loc iden = 
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError Internal EUnboxingEnumType) >>=
  (\case {
      LocatedElement (GType tydef@(Enum {})) entryLoc  -> return (LocatedElement tydef entryLoc);
      -- | If we are here, it means that the type was not an enum type.
      -- This should never happen.
      _ -> throwError $ annotateError Internal EUnboxingEnumType
    })

-- | Insert mutable object (variable) in local scope.
insertLocalMutObj :: Location -> Identifier -> TerminaType SemanticAnn -> SemanticMonad ()
insertLocalMutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (LocatedElement (Mutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolAlreadyDefined (ident, prevloc)

-- | Insert immutable object (variable) in local scope.
insertLocalImmutObj :: Location -> Identifier -> TerminaType SemanticAnn -> SemanticMonad ()
insertLocalImmutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (LocatedElement (Immutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolAlreadyDefined (ident, prevloc)
  
insertGlobalTy :: Location -> SemanTypeDef SemanticAnn -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (LocatedElement (GType tydef) loc) (EUsedTypeName type_name)
 where
   type_name = getTypeIdentifier tydef

insertGlobalFun :: Location -> Identifier -> [Parameter SemanticAnn] -> TerminaType SemanticAnn -> SemanticMonad ()
insertGlobalFun loc ident ps rettype =
  insertGlobal ident (LocatedElement (GFun (FunctionSeman ps rettype)) loc) (EUsedFunName ident)

insertGlobal :: Identifier -> LocatedElement (GEntry SemanticAnn) -> (Location -> Error) -> SemanticMonad ()
insertGlobal ident entry err =
  glbWhereIsDefined ident >>=
  \case
    { Just l -> throwError (annotateError (location entry) (err l)) ;
      Nothing -> modify (\s -> s{global = M.insert ident entry (global s)}) ;
    }
  -- if b then throwError (annotateError (location entry) getError)
  -- else

insertLocalVariables :: Location -> [(Identifier , TerminaType SemanticAnn)] -> SemanticMonad ()
insertLocalVariables loc = mapM_ (\case
  (ident, ty@(TBoxSubtype _)) -> insertLocalMutObj loc ident ty;
  (ident, ty) -> insertLocalImmutObj loc ident ty)

-- | Get the type of a local (already) defined object. If it is not defined throw an error.
getLocalObjTy :: Location -> Identifier -> SemanticMonad (AccessKind, TerminaType SemanticAnn)
getLocalObjTy loc ident =
  gets local >>=
  -- | Get local objects map and check if |ident| is a member of that map
  (\case {
      Just ob -> return . element $ ob;
      Nothing -> throwError $ annotateError loc (ENotNamedObject ident)
  }) . M.lookup ident

-- | Get the Type of a defined entity variable. If it is not defined throw an error.
getGlobalEntry :: Location -> Identifier -> SemanticMonad (LocatedElement (GEntry SemanticAnn))
getGlobalEntry loc ident =
  gets global
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (EUnknownGlobal ident))) return . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy, getRHSVarTy, getGlobalVarTy ::
  Location
  -> Identifier
  -> SemanticMonad (AccessKind, TerminaType SemanticAnn)
getLHSVarTy loc ident =
  -- | Try first local environment
  catchError (getLocalObjTy loc ident >>= (\(ak, ts) -> return (ak, ts)))
  -- | If it is not defined there, check ro environment
    (\errorLocal ->
      case getError errorLocal of {
        ENotNamedObject _ -> catchError (getGlobalEntry loc ident)
          (\errorGlobal ->
            case getError errorGlobal of {
              EUnknownGlobal _ -> 
                throwError $ annotateError loc (ENotNamedObject ident);
              _  -> throwError errorGlobal;
            }
          ) >>= (\case{
                  LocatedElement (GGlob _) _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                  LocatedElement (GConst {}) _ -> throwError $ annotateError loc (EConstantIsReadOnly ident);
                  _ -> throwError $ annotateError loc (ENotNamedObject ident);
                });
          ;
        _  -> throwError errorLocal;
      })
getRHSVarTy loc ident =
  -- | Try first local environment
  catchError
    (getLocalObjTy loc ident >>= (\(ak, ts) -> return (ak, ts)))
  -- | If it is not defined there, check ro environment
    (\errorLocal ->
      case getError errorLocal of {
        ENotNamedObject _ -> catchError (getGlobalEntry loc ident)
          (\errorGlobal ->
            case getError errorGlobal of {
              EUnknownGlobal _ -> throwError $ annotateError loc (ENotNamedObject ident);
              _  -> throwError errorGlobal;
            }
          ) >>= (\case{
                  LocatedElement (GGlob _) _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                  LocatedElement (GConst ts _) _ -> return (Immutable, ts);
                  _ -> throwError $ annotateError loc (ENotNamedObject ident);
                });
          ;
        _  -> throwError errorLocal;
      })
getGlobalVarTy loc ident =
  catchError (getGlobalEntry loc ident)
             (\errorGlobal ->
                case getError errorGlobal of {
                  EUnknownGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                   _  -> throwError errorGlobal;
                }
              ) >>= (\case {
                        LocatedElement (GGlob ty@(TGlobal ResourceClass _)) _  -> return (Mutable, ty);
                        _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                      });


glbWhereIsDefined :: Identifier -> SemanticMonad (Maybe Location)
glbWhereIsDefined i = fmap location . M.lookup i <$> gets global

whereIsDefined :: Identifier -> SemanticMonad (Maybe Location)
whereIsDefined ident = do
  st <- get
  case M.lookup ident (global st) of
    Nothing -> case M.lookup ident (local st) of
      Nothing -> return Nothing
      Just ob -> return . Just . location $ ob
    Just ob -> return . Just . location $ ob

sameTyOrError :: Location -> TerminaType SemanticAnn -> TerminaType SemanticAnn -> SemanticMonad ()
sameTyOrError loc expectedTy actualTy =
  unless (sameTy expectedTy actualTy) (throwError $ annotateError loc $ EMismatch expectedTy actualTy)

unBox :: Object SemanticAnn -> Object SemanticAnn
unBox t = Unbox t (unboxTypeAnn (getAnnotation t))

unBoxExp :: Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
unBoxExp (AccessObject obj) =  return $ AccessObject (unBox obj)
unBoxExp _ = throwError $ annotateError Internal EUnboxingExpression

mustBeTy :: TerminaType SemanticAnn -> Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
mustBeTy ty expression =
  getExprType expression >>=
  sameTyOrError loc ty
  >> return expression
  where
    SemanticAnn _ loc = getAnnotation expression

getIntConst :: Const a -> SemanticMonad Integer
getIntConst (I (TInteger i _) _) = return i
getIntConst _ = throwError $ annotateError Internal EUnboxingIntConst

catchMismatch :: 
  -- | Location of the error
  Parser.ParserAnn 
  -- | Function to create the error
  -> (TerminaType SemanticAnn -> Error) 
  -- | Action to execute
  -> SemanticMonad a 
  -- | Action to execute
  -> SemanticMonad a
catchMismatch ann ferror action = catchError action (\err -> case getError err of
  EMismatch _ ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *simple* |simpleType|.
arrayTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
arrayTyOrFail pann ty = unless (arrayTy ty) (throwError (annotateError pann (EInvalidArrayType ty)))

msgTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
msgTyOrFail pann ty = unless (msgTy ty) (throwError (annotateError pann (EInvalidMessageType ty)))

structFieldTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
structFieldTyOrFail pann ty = unless (fieldTy ty) (throwError (annotateError pann (EInvalidStructFieldType ty)))

enumParamTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
enumParamTyOrFail pann ty = unless (fieldTy ty) (throwError (annotateError pann (EInvalidEnumParameterType ty)))

catchExpectedCopy ::
  -- | Location of the error
  Parser.ParserAnn
  -- | Function to create the error
  -> (TerminaType SemanticAnn -> Error)
  -- | Action to execute
  -> SemanticMonad a
  -- | Action to execute
  -> SemanticMonad a
catchExpectedCopy ann ferror action = catchError action (\err -> case getError err of
  EExpectedCopyType ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

catchExpectedNum ::
  -- | Location of the error
  Parser.ParserAnn
  -- | Function to create the error
  -> (TerminaType SemanticAnn -> Error)
  -- | Action to execute
  -> SemanticMonad a
  -- | Action to execute
  -> SemanticMonad a
catchExpectedNum ann ferror action = catchError action (\err -> case getError err of
  EExpectedNumType ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

catchExpectedConstSubtype ::
  Location
  -- | Function to create the error
  -> (TerminaType SemanticAnn -> Error)
  -> SemanticMonad a
  -> SemanticMonad a
catchExpectedConstSubtype ann ferror action = catchError action (\err -> case getError err of
  EInvalidConstType ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *copiable*.
copyTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
copyTyOrFail pann ty = unless (copyTy ty) (throwError (annotateError pann (EExpectedCopyType ty)))

numTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
numTyOrFail pann ty = unless (numTy ty) (throwError (annotateError pann (EExpectedNumType ty)))

constTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
constTyOrFail pann ty = unless (constTy ty) (throwError (annotateError pann (EInvalidConstType ty)))

optionTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
optionTyOrFail pann ty = unless (optionTy ty) (throwError (annotateError pann (EInvalidOptionType ty)))

refTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
refTyOrFail pann ty = unless (refTy ty) (throwError (annotateError pann (EInvalidReferenceType ty)))

boxTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
boxTyOrFail pann ty = unless (boxTy ty) (throwError (annotateError pann (EInvalidBoxType ty)))

locTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
locTyOrFail pann ty = unless (locTy ty) (throwError (annotateError pann (EInvalidFixedLocationType ty)))

declTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
declTyOrFail pann ty = unless (declTy ty) (throwError (annotateError pann (EInvalidDeclarationType ty)))

accessPortTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
accessPortTyOrFail pann ty = unless (accessPortTy ty) (throwError (annotateError pann (EInvalidAccessPortType ty)))

allocTyOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
allocTyOrFail pann ty = unless (allocTy ty) (throwError (annotateError pann (EInvalidAllocatorType ty)))

-- Helper function failing if a given |TerminaType| cannot be used to define a class field.
classFieldTyorFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
classFieldTyorFail pann ty = unless (classFieldTy ty) (throwError (annotateError pann (EInvalidClassFieldType ty)))

constSubtypeOrFail :: Location -> TerminaType SemanticAnn -> SemanticMonad ()
constSubtypeOrFail _ (TConstSubtype _) = return ()
constSubtypeOrFail loc _ = throwError (annotateError loc EExpressionNotConstant)

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an internal error.
getObjType :: Object SemanticAnn -> SemanticMonad (AccessKind, TerminaType SemanticAnn)
getObjType = maybe (throwError $ annotateError Internal EUnboxingObject) return . getObjectSAnns . getAnnotation

getExprType :: Expression SemanticAnn -> SemanticMonad (TerminaType SemanticAnn)
getExprType
  = maybe (throwError $ annotateError Internal EUnboxingExpression) return
  . getResultingType . getSemanticAnn . getAnnotation
