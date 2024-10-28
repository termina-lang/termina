{-# LANGUAGE LambdaCase    #-}

-- | Semantic Monad.
-- Here lives the monad we use to handle semantic pass effects.
-- It should be something like Exceptions + State

module Semantic.Monad where

import Data.Map as M
import Data.Maybe

-- Debugging
-- import Debugging

-- AST Info
import Utils.Annotations
import Core.AST as CAST
import Semantic.AST

import Semantic.Errors.Errors
import Semantic.Types
import Core.Utils

-- Monads
import Control.Monad.Except
import qualified Control.Monad.State.Strict as ST
import qualified Parser.Types as Parser
import Utils.Monad

----------------------------------------

getResultingType :: SemanticElems -> Maybe TerminaType
getResultingType (ETy ty) = 
  Just (
    case ty of {
      SimpleType t -> t; 
      ObjectType _ t -> t; 
      AppType _ t -> t;
      PortConnection _ -> TUnit})
getResultingType _        = Nothing

getObjectSAnns :: SemanticAnn -> Maybe (AccessKind, TerminaType)
getObjectSAnns (Located (ETy (ObjectType ak ty)) _) = Just (ak, ty)
getObjectSAnns _                                    = Nothing

getArgumentsType :: SemanticElems -> Maybe [TerminaType]
getArgumentsType (ETy (AppType ts _)) = Just ts
getArgumentsType _                    = Nothing

getMatchCaseTypes :: SemanticElems -> Maybe [TerminaType]
getMatchCaseTypes (STy (MatchCaseStmtType ts)) = Just ts
getMatchCaseTypes _                           = Nothing

isResultFromApp :: SemanticElems -> Bool
isResultFromApp = isJust . getArgumentsType
----------------------------------------

buildExpAnn :: Location -> TerminaType -> SemanticAnn
buildExpAnn loc = locate loc . ETy . SimpleType

buildExpAnnObj :: Location -> AccessKind -> TerminaType -> SemanticAnn
buildExpAnnObj loc ak = locate loc . ETy . ObjectType ak

buildExpAnnApp :: Location -> [TerminaType] -> TerminaType -> SemanticAnn
buildExpAnnApp loc tys = locate loc . ETy . AppType tys

-- | Build annotations for global objects (tasks, handlers, resources, channels or emitters)
buildGlobalAnn :: Location -> TerminaType -> SemanticAnn
buildGlobalAnn loc = locate loc . GTy 

buildStmtAnn :: Location -> SemanticAnn
buildStmtAnn = Located (STy SimpleStmtType)

buildStmtMatchCaseAnn :: Location -> [TerminaType] -> SemanticAnn
buildStmtMatchCaseAnn loc ts = locate loc (STy (MatchCaseStmtType ts))

buildOutPortConnAnn :: Location -> TerminaType -> SemanticAnn
buildOutPortConnAnn loc ts = locate loc (ETy (PortConnection (OutPConnTy ts)))

buildAccessPortConnAnn :: Location -> TerminaType -> [ProcedureSeman] -> SemanticAnn
buildAccessPortConnAnn loc ts procs = locate loc (ETy (PortConnection (APConnTy ts procs)))

buildPoolConnAnn :: Location -> TerminaType -> Size -> SemanticAnn
buildPoolConnAnn loc ts s = locate loc (ETy (PortConnection (APPoolConnTy ts s)))

buildAtomicConnAnn :: Location -> TerminaType -> SemanticAnn
buildAtomicConnAnn loc ts = locate loc (ETy (PortConnection (APAtomicConnTy ts)))

buildAtomicArrayConnAnn :: Location -> TerminaType -> Size -> SemanticAnn
buildAtomicArrayConnAnn loc ts s = locate loc (ETy (PortConnection (APAtomicArrayConnTy ts s)))

buildSinkPortConnAnn :: Location -> TerminaType -> Identifier -> SemanticAnn
buildSinkPortConnAnn loc ts action = locate loc (ETy (PortConnection (SPConnTy ts action)))

buildInPortConnAnn :: Location -> TerminaType -> Identifier -> SemanticAnn
buildInPortConnAnn loc ts action = locate loc (ETy (PortConnection (InPConnTy ts action)))

getSemanticAnn :: SemanticAnn -> SemanticElems
getSemanticAnn = element

forgetSemAnn :: SemanticAnn -> Location
forgetSemAnn = location

getTypeSemAnn :: SemanticAnn -> Maybe TerminaType
getTypeSemAnn  = getResultingType . getSemanticAnn

unboxExpType :: ExprSeman -> ExprSeman
unboxExpType (SimpleType (TBoxSubtype ty)) = SimpleType ty
unboxExpType (ObjectType ak (TBoxSubtype ty)) = ObjectType ak ty
unboxExpType (AppType ts (TBoxSubtype ty)) = AppType ts ty
unboxExpType _ = error "impossible 888+1"

unboxTypeAnn :: SemanticAnn -> SemanticAnn
unboxTypeAnn (Located (ETy en) p) = Located (ETy (unboxExpType en)) p
unboxTypeAnn _                    = error "impossible 888"

----------------------------------------
-- | Global env
-- It has global definitions
type GlobalEnv = Map Identifier (Located (GEntry SemanticAnn))

-- | Local env
-- variables to their type
type LocalEnv = Map Identifier (Located (AccessKind, TerminaType))

-- This may seem a bad decision, but each envornment represent something
-- different.
-- TODO We can use empty types to disable envirnoments and make Haskell do part
-- of our work.

-- | Environment required to type expression packed into just one type.
data Environment
 = ExprST
 { global :: GlobalEnv
 , local  :: LocalEnv
 }

getEntry :: Located (GEntry SemanticAnn) -> GEntry SemanticAnn
getEntry = element

stdlibGlobalEnv :: [(Identifier, Located (GEntry SemanticAnn))]
stdlibGlobalEnv =
  [("Result", Located (GType (Enum "Result" [EnumVariant "Ok" [], EnumVariant "Error" []] [])) Internal),
   ("TimeVal", Located (GType (Struct "TimeVal" [FieldDefinition "tv_sec" TUInt32, FieldDefinition "tv_usec" TUInt32] [])) Internal),
   ("Interrupt", Located (GType (Class EmitterClass "Interrupt" [] [] [])) Internal),
   ("SystemInit", Located (GType (Class EmitterClass "SystemInit" [] [] [])) Internal),
   ("system_init", Located (GGlob (TGlobal EmitterClass "SystemInit")) Internal),
   ("PeriodicTimer", Located (GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (TStruct "TimeVal")) (buildExpAnn Internal (TStruct "TimeVal"))] [] [])) Internal),
   ("clock_get_uptime", Located (GFun (FunctionSeman [TReference Mutable (TStruct "TimeVal")] TUnit)) Internal),
   ("delay_in", Located (GFun (FunctionSeman [TReference Immutable (TStruct "TimeVal")] TUnit)) Internal)]

makeInitialGlobalEnv :: [(Identifier, Located (GEntry SemanticAnn))] -> Environment
makeInitialGlobalEnv pltEnvironment = ExprST (fromList (stdlibGlobalEnv ++ pltEnvironment)) empty

type SemanticMonad = ExceptT SemanticErrors (ST.State Environment)

----------------------------------------
gets :: (Environment -> a) -> SemanticMonad a
gets = lift . ST.gets

get :: SemanticMonad Environment
get = lift ST.get

put :: Environment -> SemanticMonad ()
put = lift . ST.put

modify :: (Environment -> Environment) -> SemanticMonad ()
modify = lift . ST.modify

----------------------------------------
-- Monadic helpers.

-- | Execute computations in a temporal state wihtout
-- modifying current state.
withInState :: Environment -> SemanticMonad a -> SemanticMonad a
withInState tempState comp = localScope (put tempState >> comp)

----------------------------------------
-- Some helper functions to bring information from the environment.

-- | Get global definition of a Type
getGlobalTypeDef :: Location -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnn)
getGlobalTypeDef loc tid  = gets global >>=
  maybe
  -- if there is no varialbe name |tid|
  (throwError $ annotateError loc (ENoTypeFound tid))
  -- if so, return its type
  (\case {
      Located (GType tydef) _ -> return tydef;
      Located _ entryLoc -> throwError $ annotateError loc (EGlobalNotType (tid, entryLoc))
      }) . M.lookup tid

getFunctionTy :: Location -> Identifier -> SemanticMonad ([TerminaType], TerminaType, Location)
getFunctionTy loc iden =
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError loc (EFunctionNotFound iden))
  >>= \case
    Located (GFun (FunctionSeman args retty)) entryLoc -> return (args, retty, entryLoc)
    Located _ entryLoc -> throwError $ annotateError loc (EGlobalNotFunction (iden, entryLoc))

-- | Get the type definition of an enum type from the global environment.
-- This function is only called for a type that we know is an enum type. If
-- there is an error it is an internal error.
getEnumTy :: Location -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnn, Location)
getEnumTy loc iden = 
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError Internal EUnboxingEnumType) >>=
  (\case {
      Located (GType tydef@(Enum {})) entryLoc  -> return (tydef, entryLoc);
      -- | If we are here, it means that the type was not an enum type.
      -- This should never happen.
      _ -> throwError $ annotateError Internal EUnboxingEnumType
    })

-- | Add new *local* immutable objects and execute computation in the
-- new local environment.
addLocalImmutObjs :: Location -> [(Identifier, TerminaType)] -> SemanticMonad a -> SemanticMonad a
addLocalImmutObjs loc newVars ma  =
  localScope (addVariables newVars >> ma)
  where
    addVariables = mapM_ (uncurry (insertLocalImmutObj loc))

-- | Insert mutable object (variable) in local scope.
insertLocalMutObj :: Location -> Identifier -> TerminaType -> SemanticMonad ()
insertLocalMutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (Located (Mutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolAlreadyDefined (ident, prevloc)

-- | Insert immutable object (variable) in local scope.
insertLocalImmutObj :: Location -> Identifier -> TerminaType -> SemanticMonad ()
insertLocalImmutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (Located (Immutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolAlreadyDefined (ident, prevloc)
  
insertGlobalTy :: Location -> SemanTypeDef SemanticAnn -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (Located (GType tydef) loc) (EUsedTypeName type_name)
 where
   type_name = getTypeIdentifier tydef

insertGlobalFun :: Location -> Identifier -> [TerminaType] -> TerminaType -> SemanticMonad ()
insertGlobalFun loc ident ps rettype =
  insertGlobal ident (Located (GFun (FunctionSeman ps rettype)) loc) (EUsedFunName ident)

insertGlobal :: Identifier -> Located (GEntry SemanticAnn) -> (Location -> Error) -> SemanticMonad ()
insertGlobal ident entry err =
  glbWhereIsDefined ident >>=
  \case
    { Just l -> throwError (annotateError (location entry) (err l)) ;
      Nothing -> modify (\s -> s{global = M.insert ident entry (global s)}) ;
    }
  -- if b then throwError (annotateError (location entry) getError)
  -- else

insertLocalVariables :: Location -> [(Identifier , TerminaType)] -> SemanticMonad ()
insertLocalVariables loc = mapM_ (\case
  (ident, ty@(TBoxSubtype _)) -> insertLocalMutObj loc ident ty;
  (ident, ty) -> insertLocalImmutObj loc ident ty)

-- | Get the type of a local (already) defined object. If it is not defined throw an error.
getLocalObjTy :: Location -> Identifier -> SemanticMonad (AccessKind, TerminaType)
getLocalObjTy loc ident =
  gets local >>=
  -- | Get local objects map and check if |ident| is a member of that map
  (\case {
      Just ob -> return . element $ ob;
      Nothing -> throwError $ annotateError loc (ENotNamedObject ident)
  }) . M.lookup ident

-- | Get the Type of a defined read-only variable. If it is not defined throw an error.
getConst :: Location -> Identifier -> SemanticMonad (TerminaType, Const)
getConst loc ident = do
    catchError (getGlobalEntry loc ident)
        (\errorGlobal ->
            -- | We have not found a global object with the name |ident|
            case getError errorGlobal of {
              ENotNamedGlobal _ ->
                -- | If we have not found a global object with the name |ident|, then check the local objects.
                -- Any path that goes through here is an error.
                catchError (getLocalObjTy loc ident)
                (\errorLocal ->
                  case getError errorLocal of {
                    ENotNamedObject _ -> throwError $ annotateError loc (ENotNamedObject ident);
                    _ -> throwError errorLocal;
                  }) >>
                    -- | Ooops! We found a local object with the name |ident| but it is not a constant.
                    throwError (annotateError loc (ENotConstant ident));
              err  -> error $ "Impossible error: " ++ show err;
            }
          ) >>= (\case {
                    -- | It is a global constant!
                    Located (GConst ts value) _ -> return (ts, value);
                    -- | It is a global object, but not a constant.
                    _ -> throwError $ annotateError loc (ENotConstant ident);
    });

getIntSize :: Location -> Size -> SemanticMonad Integer
getIntSize loc (CAST.V ident) = do
  (ty, value) <- getConst loc ident
  sameTyOrError loc TUSize ty
  getIntConst loc value
getIntSize _loc (CAST.K (TInteger value _)) = return value

-- | Get the Type of a defined entity variable. If it is not defined throw an error.
getGlobalEntry :: Location -> Identifier -> SemanticMonad (Located (GEntry SemanticAnn))
getGlobalEntry loc ident =
  gets global
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedGlobal ident))) return . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy, getRHSVarTy, getGlobalVarTy ::
  Location
  -> Identifier
  -> SemanticMonad (AccessKind, TerminaType)
getLHSVarTy loc ident =
  -- | Try first local environment
  catchError (getLocalObjTy loc ident >>= (\(ak, ts) -> return (ak, ts)))
  -- | If it is not defined there, check ro environment
    (\errorLocal ->
      case getError errorLocal of {
        ENotNamedObject _ ->
          catchError (getConst loc ident)
          (\errorRO ->
            case getError errorRO of {
              ENotConstant _ -> catchError (getGlobalEntry loc ident)
                (\errorGlobal ->
                  case getError errorGlobal of {
                    ENotNamedGlobal _ -> 
                      throwError $ annotateError loc (ENotNamedObject ident);
                    _  -> throwError errorGlobal;
                  }
                ) >> throwError (annotateError loc (EInvalidAccessToGlobal ident));
              _ -> throwError errorRO;
            }) >> throwError (annotateError loc (EConstantIsReadOnly ident))
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
        ENotNamedObject _ ->
          catchError (getConst loc ident >>= (\(ts, _) -> return (Immutable, ts)))
          (\errorRO ->
            case getError errorRO of {
              ENotNamedObject _ -> catchError (getGlobalEntry loc ident)
                (\errorGlobal ->
                  case getError errorGlobal of {
                    ENotNamedGlobal _ -> throwError $ annotateError loc (ENotNamedObject ident);
                    _  -> throwError errorGlobal;
                  }
                ) >>= (\case{
                        Located (GGlob _) _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                        _ -> throwError $ annotateError loc (ENotNamedObject ident);
                      });
              _ -> throwError errorRO;
            })
          ;
        _  -> throwError errorLocal;
      })
getGlobalVarTy loc ident =
  catchError (getGlobalEntry loc ident)
             (\errorGlobal ->
                case getError errorGlobal of {
                  ENotNamedGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                   _  -> throwError errorGlobal;
                }
              ) >>= (\case {
                        Located (GGlob ty@(TGlobal ResourceClass _)) _  -> return (Mutable, ty);
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

-- | Checks if two type are the same numeric type.
-- If they are not, it throws a mismatch error.
sameTyOrError :: Location -> TerminaType -> TerminaType -> SemanticMonad ()
sameTyOrError loc t1 t2 =
  unless (sameTy t1 t2) (throwError $ annotateError loc $ EMismatch t1 t2)

unBox :: Object SemanticAnn -> Object SemanticAnn
unBox t = Unbox t (unboxTypeAnn (getAnnotation t))

unBoxExp :: Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
unBoxExp (AccessObject obj) =  return $ AccessObject (unBox obj)
unBoxExp _ = throwError $ annotateError Internal EUnboxingExpression

mustBeTy :: TerminaType -> Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
mustBeTy ty expression =
  getExprType expression >>=
  sameTyOrError loc ty
  >> return expression
  where
    ann_exp = getAnnotation expression
    loc = location ann_exp

getIntConst :: Location -> Const -> SemanticMonad Integer
getIntConst _ (I (TInteger i _) _) = return i
getIntConst loc e     = throwError $ annotateError loc $ ENotIntConst e

catchMismatch :: 
  -- | Location of the error
  Parser.ParserAnn 
  -- | Function to create the error
  -> (TerminaType -> Error) 
  -- | Action to execute
  -> SemanticMonad a 
  -- | Action to execute
  -> SemanticMonad a
catchMismatch ann ferror action = catchError action (\err -> case getError err of
  EMismatch _ ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *simple* |simpleType|.
arrayTyOrFail :: Location -> TerminaType -> SemanticMonad ()
arrayTyOrFail pann ty = unless (arrayTy ty) (throwError (annotateError pann (EInvalidArrayType ty)))

msgTyOrFail :: Location -> TerminaType -> SemanticMonad ()
msgTyOrFail pann ty = unless (msgTy ty) (throwError (annotateError pann (EInvalidMessageType ty)))

structFieldTyOrFail :: Location -> TerminaType -> SemanticMonad ()
structFieldTyOrFail pann ty = unless (fieldTy ty) (throwError (annotateError pann (EInvalidStructFieldType ty)))

enumParamTyOrFail :: Location -> TerminaType -> SemanticMonad ()
enumParamTyOrFail pann ty = unless (fieldTy ty) (throwError (annotateError pann (EInvalidEnumParameterType ty)))

catchExpectedCopy ::
  -- | Location of the error
  Parser.ParserAnn
  -- | Function to create the error
  -> (TerminaType -> Error)
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
  -> (TerminaType -> Error)
  -- | Action to execute
  -> SemanticMonad a
  -- | Action to execute
  -> SemanticMonad a
catchExpectedNum ann ferror action = catchError action (\err -> case getError err of
  EExpectedNumType ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *copiable*.
copyTyOrFail :: Location -> TerminaType -> SemanticMonad ()
copyTyOrFail pann ty = unless (copyTy ty) (throwError (annotateError pann (EExpectedCopyType ty)))

numTyOrFail :: Location -> TerminaType -> SemanticMonad ()
numTyOrFail pann ty = unless (numTy ty) (throwError (annotateError pann (EExpectedNumType ty)))

optionTyOrFail :: Location -> TerminaType -> SemanticMonad ()
optionTyOrFail pann ty = unless (optionTy ty) (throwError (annotateError pann (EInvalidOptionType ty)))

refTyOrFail :: Location -> TerminaType -> SemanticMonad ()
refTyOrFail pann ty = unless (refTy ty) (throwError (annotateError pann (EInvalidReferenceType ty)))

boxTyOrFail :: Location -> TerminaType -> SemanticMonad ()
boxTyOrFail pann ty = unless (boxTy ty) (throwError (annotateError pann (EInvalidBoxType ty)))

locTyOrFail :: Location -> TerminaType -> SemanticMonad ()
locTyOrFail pann ty = unless (locTy ty) (throwError (annotateError pann (EInvalidLocationType ty)))

declTyOrFail :: Location -> TerminaType -> SemanticMonad ()
declTyOrFail pann ty = unless (declTy ty) (throwError (annotateError pann (EInvalidDeclarationType ty)))

accessPortTyOrFail :: Location -> TerminaType -> SemanticMonad ()
accessPortTyOrFail pann ty = unless (accessPortTy ty) (throwError (annotateError pann (EInvalidAccessPortType ty)))

allocTyOrFail :: Location -> TerminaType -> SemanticMonad ()
allocTyOrFail pann ty = unless (allocTy ty) (throwError (annotateError pann (EInvalidAllocatorType ty)))

-- Helper function failing if a given |TerminaType| cannot be used to define a class field.
classFieldTyorFail :: Location -> TerminaType -> SemanticMonad ()
classFieldTyorFail pann ty = unless (classFieldTy ty) (throwError (annotateError pann (EInvalidClassFieldType ty)))

-- | Function that translates a |TypeSpecifier| into a |TerminaType|.
typeTypeSpecifier :: Location -> TypeSpecifier -> SemanticMonad TerminaType
typeTypeSpecifier loc (TSDefinedType ident []) = do
  -- Check that the type was defined
  glbTypeDef <- getGlobalTypeDef loc ident
  case glbTypeDef of 
    Struct name _ _ -> return $ TStruct name
    Enum name _ _ -> return $ TEnum name
    Class clsKind name _ _ _ -> return $ TGlobal clsKind name 
    Interface name _ _ -> return $ TInterface name
typeTypeSpecifier loc (TSDefinedType "Allocator" [TypeParamTypeSpec ts]) = 
  TAllocator <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSDefinedType "AtomicAccess" [TypeParamTypeSpec ts]) =
  TAtomicAccess <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSDefinedType "AtomicArrayAccess" [TypeParamTypeSpec ts, TypeParamSize s]) =
  TAtomicArrayAccess <$> typeTypeSpecifier loc ts <*> pure s
typeTypeSpecifier loc (TSDefinedType "Atomic" [TypeParamTypeSpec ts]) = 
  TAtomic <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSDefinedType "AtomicArray" [TypeParamTypeSpec ts, TypeParamSize s]) = 
  TAtomicArray <$> typeTypeSpecifier loc ts <*> pure s
typeTypeSpecifier loc (TSDefinedType "Option" [TypeParamTypeSpec ts]) = 
  TOption <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSDefinedType "MsgQueue" [TypeParamTypeSpec ts, TypeParamSize s]) = 
  TMsgQueue <$> typeTypeSpecifier loc ts <*> pure s
typeTypeSpecifier loc (TSDefinedType "Pool" [TypeParamTypeSpec ts, TypeParamSize s]) = 
  TPool <$> typeTypeSpecifier loc ts <*> pure s
typeTypeSpecifier loc (TSArray ts s) = 
  TArray <$> typeTypeSpecifier loc ts <*> pure s
typeTypeSpecifier loc (TSReference ak ts) = 
  TReference ak <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSBoxSubtype ts) = 
  TBoxSubtype <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSLocation ts) = 
  TLocation <$> typeTypeSpecifier loc ts
typeTypeSpecifier loc (TSAccessPort ty) =
  TAccessPort <$> typeTypeSpecifier loc ty
typeTypeSpecifier loc (TSSinkPort ty action) =
  TSinkPort <$> typeTypeSpecifier loc ty <*> pure action
typeTypeSpecifier loc (TSInPort ty action) = 
  TInPort <$> typeTypeSpecifier loc ty <*> pure action
typeTypeSpecifier loc (TSOutPort ty) = 
  TOutPort <$> typeTypeSpecifier loc ty
-- This is explicit just in case
typeTypeSpecifier _ TSUInt8  = return TUInt8
typeTypeSpecifier _ TSUInt16 = return TUInt16
typeTypeSpecifier _ TSUInt32 = return TUInt32
typeTypeSpecifier _ TSUInt64 = return TUInt64
typeTypeSpecifier _ TSInt8   = return TInt8
typeTypeSpecifier _ TSInt16  = return TInt16
typeTypeSpecifier _ TSInt32  = return TInt32
typeTypeSpecifier _ TSInt64  = return TInt64
typeTypeSpecifier _ TSUSize  = return TUSize
typeTypeSpecifier _ TSChar   = return TChar
typeTypeSpecifier _ TSBool   = return TBool
typeTypeSpecifier _ TSUnit   = return TUnit
typeTypeSpecifier loc ts = throwError $ annotateError loc (EInvalidTypeSpecifier ts)

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an internal error.
getObjType :: Object SemanticAnn -> SemanticMonad (AccessKind, TerminaType)
getObjType = maybe (throwError $ annotateError Internal EUnboxingObject) return . getObjectSAnns . getAnnotation

getExprType :: Expression SemanticAnn -> SemanticMonad TerminaType
getExprType
  = maybe (throwError $ annotateError Internal EUnboxingStmtExpr) return
  . getResultingType . getSemanticAnn . getAnnotation
