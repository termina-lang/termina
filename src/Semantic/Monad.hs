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
getEType :: SemanticElems -> Maybe ESeman
getEType (ETy t) = Just t
getEType _ = Nothing

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

getGEntry :: SemanticElems -> Maybe (GEntry SemanticAnn)
getGEntry (GTy a) = Just a
getGEntry _       = Nothing

buildExpAnn :: TLocation -> TerminaType -> SemanticAnn
buildExpAnn loc = locate loc . ETy . SimpleType

buildExpAnnObj :: TLocation -> AccessKind -> TerminaType -> SemanticAnn
buildExpAnnObj loc ak = locate loc . ETy . ObjectType ak

buildExpAnnApp :: TLocation -> [TerminaType] -> TerminaType -> SemanticAnn
buildExpAnnApp loc tys = locate loc . ETy . AppType tys

buildGlobalAnn :: TLocation -> SemGlobal -> SemanticAnn
buildGlobalAnn loc = locate loc . GTy . GGlob

buildGlobal :: TLocation -> GEntry SemanticAnn -> SemanticAnn
buildGlobal loc = locate loc . GTy

buildGlobalTy :: TLocation -> SemanTypeDef SemanticAnn -> SemanticAnn
buildGlobalTy loc = locate loc . GTy . GType

buildStmtAnn :: TLocation -> SemanticAnn
buildStmtAnn = Located (STy SimpleStmtType)

buildStmtMatchCaseAnn :: TLocation -> [TerminaType] -> SemanticAnn
buildStmtMatchCaseAnn loc ts = locate loc (STy (MatchCaseStmtType ts))

buildOutPortConnAnn :: TLocation -> TerminaType -> SemanticAnn
buildOutPortConnAnn loc ts = locate loc (ETy (PortConnection (OutPConnTy ts)))

buildAccessPortConnAnn :: TLocation -> TerminaType -> [SemanProcedure] -> SemanticAnn
buildAccessPortConnAnn loc ts procs = locate loc (ETy (PortConnection (APConnTy ts procs)))

buildPoolConnAnn :: TLocation -> TerminaType -> Size -> SemanticAnn
buildPoolConnAnn loc ts s = locate loc (ETy (PortConnection (APPoolConnTy ts s)))

buildAtomicConnAnn :: TLocation -> TerminaType -> SemanticAnn
buildAtomicConnAnn loc ts = locate loc (ETy (PortConnection (APAtomicConnTy ts)))

buildAtomicArrayConnAnn :: TLocation -> TerminaType -> Size -> SemanticAnn
buildAtomicArrayConnAnn loc ts s = locate loc (ETy (PortConnection (APAtomicArrayConnTy ts s)))

buildSinkPortConnAnn :: TLocation -> TerminaType -> Identifier -> SemanticAnn
buildSinkPortConnAnn loc ts action = locate loc (ETy (PortConnection (SPConnTy ts action)))

buildInPortConnAnn :: TLocation -> TerminaType -> Identifier -> SemanticAnn
buildInPortConnAnn loc ts action = locate loc (ETy (PortConnection (InPConnTy ts action)))

getSemanticAnn :: SemanticAnn -> SemanticElems
getSemanticAnn = element

forgetSemAnn :: SemanticAnn -> TLocation
forgetSemAnn = location

getTypeSemAnn :: SemanticAnn -> Maybe TerminaType
getTypeSemAnn  = getResultingType . getSemanticAnn

unboxExpType :: ESeman -> ESeman
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
   ("system_init", Located (GGlob (SEmitter (TDefinedType "SystemInit"))) Internal),
   ("PeriodicTimer", Located (GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (TDefinedType "TimeVal")) (buildExpAnn Internal (TDefinedType "TimeVal"))] [] [])) Internal),
   ("clock_get_uptime", Located (GFun [TReference Mutable (TDefinedType "TimeVal")] TUnit) Internal),
   ("delay_in", Located (GFun [TReference Immutable (TDefinedType "TimeVal")] TUnit) Internal)]

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
getGlobalTypeDef :: TLocation -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnn)
getGlobalTypeDef loc tid  = gets global >>=
  maybe
  -- if there is no varialbe name |tid|
  (throwError $ annotateError loc (ENoTypeFound tid))
  -- if so, return its type
  (\case {
      GType tydef -> return tydef;
        _         -> throwError $ annotateError loc (EGlobalNotType tid)
      } . getEntry) . M.lookup tid

getFunctionTy :: TLocation -> Identifier -> SemanticMonad ([TerminaType], TerminaType, TLocation)
getFunctionTy loc iden =
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError loc (EFunctionNotFound iden))
  >>= \case
  Located (GFun args retty) entryLoc -> return (args, retty, entryLoc)
  Located _ entryLoc -> throwError $ annotateError loc (EGlobalNotFunction (iden, entryLoc))

getEnumTy :: TLocation -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnn, TLocation)
getEnumTy loc iden = 
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError loc (ENoEnumFound iden)) >>=
  (\case {
      Located (GType tydef@(Enum {})) entryLoc  -> return (tydef, entryLoc);
      Located _ entryLoc           -> throwError $ annotateError loc (EGlobalNotEnum (iden, entryLoc))
    })

-- | Add new *local* immutable objects and execute computation in the
-- new local environment.
addLocalImmutObjs :: TLocation -> [(Identifier, TerminaType)] -> SemanticMonad a -> SemanticMonad a
addLocalImmutObjs loc newVars ma  =
  localScope (addVariables newVars >> ma)
  where
    addVariables = mapM_ (uncurry (insertLocalImmutObj loc))

-- | Insert mutable object (variable) in local scope.
insertLocalMutObj :: TLocation -> Identifier -> TerminaType -> SemanticMonad ()
insertLocalMutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (Located (Mutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolDefined ident prevloc

-- | Insert immutable object (variable) in local scope.
insertLocalImmutObj :: TLocation -> Identifier -> TerminaType -> SemanticMonad ()
insertLocalImmutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (Located (Immutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolDefined ident prevloc
  
insertGlobalTy :: TLocation -> SemanTypeDef SemanticAnn -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (Located (GType tydef) loc) (EUsedTypeName type_name)
 where
   type_name = identifierType tydef

insertGlobalFun :: TLocation -> Identifier -> [TerminaType] -> TerminaType -> SemanticMonad ()
insertGlobalFun loc ident ps rettype =
  insertGlobal ident (Located (GFun ps rettype) loc) (EUsedFunName ident)

insertGlobal :: Identifier -> Located (GEntry SemanticAnn) -> (TLocation -> Error TLocation) -> SemanticMonad ()
insertGlobal ident entry err =
  glbWhereIsDefined ident >>=
  \case
    { Just l -> throwError (annotateError (location entry) (err l)) ;
      Nothing -> modify (\s -> s{global = M.insert ident entry (global s)}) ;
    }
  -- if b then throwError (annotateError (location entry) getError)
  -- else

insertLocalVariables :: TLocation -> [(Identifier , TerminaType)] -> SemanticMonad ()
insertLocalVariables loc = mapM_ (\case
  (ident, ty@(TBoxSubtype _)) -> insertLocalMutObj loc ident ty;
  (ident, ty) -> insertLocalImmutObj loc ident ty)

-- | Get the type of a local (already) defined object. If it is not defined throw an error.
getLocalObjTy :: TLocation -> Identifier -> SemanticMonad (AccessKind, TerminaType)
getLocalObjTy loc ident =
  gets local >>=
  -- | Get local objects map and check if |ident| is a member of that map
  (\case {
      Just ob -> return . element $ ob;
      Nothing -> throwError $ annotateError loc (ENotNamedObject ident)
  }) . M.lookup ident

-- | Get the Type of a defined  readonlye variable. If it is not defined throw an error.
getConst :: TLocation -> Identifier -> SemanticMonad (TerminaType, Const)
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
                    Located (GGlob (SConst ts value)) _ -> return (ts, value);
                    -- | It is a global object, but not a constant.
                    _ -> throwError $ annotateError loc (ENotConstant ident);
    });

getIntSize :: TLocation -> Size -> SemanticMonad Integer
getIntSize loc (CAST.V ident) = do
  (ty, value) <- getConst loc ident
  sameTyOrError loc TUSize ty
  getIntConst loc value
getIntSize _loc (CAST.K (TInteger value _)) = return value

-- | Get the Type of a defined entity variable. If it is not defined throw an error.
getGlobalEntry :: TLocation -> Identifier -> SemanticMonad (Located (GEntry SemanticAnn))
getGlobalEntry loc ident =
  gets global
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedGlobal ident))) return . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy, getRHSVarTy, getGlobalVarTy ::
  TLocation
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
                        Located (GGlob (SResource ts)) _  -> return (Mutable, ts);
                        _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                      });


glbWhereIsDefined :: Identifier -> SemanticMonad (Maybe TLocation)
glbWhereIsDefined i = fmap location . M.lookup i <$> gets global

whereIsDefined :: Identifier -> SemanticMonad (Maybe TLocation)
whereIsDefined ident = do
  st <- get
  case M.lookup ident (global st) of
    Nothing -> case M.lookup ident (local st) of
      Nothing -> return Nothing
      Just ob -> return . Just . location $ ob
    Just ob -> return . Just . location $ ob

-------------
-- Type |Type| helpers!
-- | Checks if two type are the same numeric type.
sameNumTy :: TerminaType -> TerminaType -> Bool
sameNumTy a b = sameTy a b && numTy a

-- | Checks if two type are the same numeric type.
-- If they are not, it throws a mismatch error.
sameTyOrError :: TLocation -> TerminaType -> TerminaType -> SemanticMonad ()
sameTyOrError loc t1 t2 =
  unless (sameTy t1 t2) (throwError $ annotateError loc $ EMismatch t1 t2)

unBox :: Object SemanticAnn -> Object SemanticAnn
unBox t = Unbox t (unboxTypeAnn (getAnnotation t))

unBoxExp :: Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
unBoxExp (AccessObject obj) =  return $ AccessObject (unBox obj)
unBoxExp _ = throwError $ annotateError Internal EUnBoxExpression

mustBeTy :: TerminaType -> Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
mustBeTy ty expression =
  getExpType expression >>=
  sameTyOrError loc ty
  >> return expression
  where
    ann_exp = getAnnotation expression
    loc = location ann_exp

getIntConst :: TLocation -> Const -> SemanticMonad Integer
getIntConst _ (I (TInteger i _) _) = return i
getIntConst loc e     = throwError $ annotateError loc $ ENotIntConst e

catchMismatch :: 
  -- | Location of the error
  Parser.ParserAnn 
  -- | Function to create the error
  -> (TerminaType -> Error Parser.ParserAnn) 
  -- | Action to execute
  -> SemanticMonad a 
  -- | Action to execute
  -> SemanticMonad a
catchMismatch ann ferror action = catchError action (\err -> case getError err of
  EMismatch _ ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *simple* |simpleType|.
arrayTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
arrayTyOrFail pann ty = unless (arrayTy ty) (throwError (annotateError pann (EInvalidArrayType ty)))

msgTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
msgTyOrFail pann ty = unless (msgTy ty) (throwError (annotateError pann (EInvalidMessageType ty)))

structFieldTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
structFieldTyOrFail pann ty = unless (fieldTy ty) (throwError (annotateError pann (EInvalidStructFieldType ty)))

enumParamTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
enumParamTyOrFail pann ty = unless (fieldTy ty) (throwError (annotateError pann (EInvalidEnumParameterType ty)))

catchExpectedCopy ::
  -- | Location of the error
  Parser.ParserAnn
  -- | Function to create the error
  -> (TerminaType -> Error TLocation)
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
  -> (TerminaType -> Error TLocation)
  -- | Action to execute
  -> SemanticMonad a
  -- | Action to execute
  -> SemanticMonad a
catchExpectedNum ann ferror action = catchError action (\err -> case getError err of
  EExpectedNumType ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *copiable*.
copyTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
copyTyOrFail pann ty = unless (copyTy ty) (throwError (annotateError pann (EExpectedCopyType ty)))

numTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
numTyOrFail pann ty = unless (numTy ty) (throwError (annotateError pann (EExpectedNumType ty)))

optionTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
optionTyOrFail pann ty = unless (optionTy ty) (throwError (annotateError pann (EInvalidOptionType ty)))

refTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
refTyOrFail pann ty = unless (refTy ty) (throwError (annotateError pann (EInvalidReferenceType ty)))

boxTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
boxTyOrFail pann ty = unless (boxTy ty) (throwError (annotateError pann (EInvalidBoxType ty)))

locTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
locTyOrFail pann ty = unless (locTy ty) (throwError (annotateError pann (EInvalidLocationType ty)))

accessPortTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
accessPortTyOrFail pann ty = unless (accessPortTy ty) (throwError (annotateError pann (EInvalidAccessPortType ty)))

allocTyOrFail :: TLocation -> TerminaType -> SemanticMonad ()
allocTyOrFail pann ty = unless (allocTy ty) (throwError (annotateError pann (EInvalidAllocatorType ty)))

-- Helper function failing if a given |TerminaType| cannot be used to define a class field.
classFieldTyorFail :: TLocation -> TerminaType -> SemanticMonad ()
classFieldTyorFail pann ty = unless (classFieldTy ty) (throwError (annotateError pann (EInvalidClassFieldType ty)))

checkSize :: TLocation -> Size -> SemanticMonad ()
checkSize loc (CAST.K s) = checkIntConstant loc TUSize s
checkSize loc (CAST.V ident) = getConst loc ident >>= (\(ty, _) -> sameTyOrError loc TUSize ty) >> return ()

-- | Function checking that a TerminaType is well-defined.
-- This is not the same as defining a type, but it is similar.
-- Some types can be found in the wild, but user defined types are just check
-- they exist (they were defined preivously).
-- Note that we do not change the |TerminaType| in any way, that's why this
-- function return |()|.
checkTerminaType :: TLocation -> TerminaType -> SemanticMonad ()
checkTerminaType loc (TDefinedType identTy) =
  -- Check that the type was defined
  void (getGlobalTypeDef loc identTy)
  -- we assume that only well-formed types are added to globals.
checkTerminaType loc (TArray ty s) =
  checkTerminaType loc ty >>
  arrayTyOrFail loc ty >>
  checkSize loc s
checkTerminaType loc (TMsgQueue ty s) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty >>
  checkSize loc s
checkTerminaType loc (TPool ty s) = checkTerminaType loc ty >> checkSize loc s
-- TOption Subtyping
checkTerminaType loc (TOption ty) = 
  checkTerminaType loc ty >>
  optionTyOrFail loc ty
checkTerminaType loc (TReference _ ty) =
  checkTerminaType loc ty >>
  refTyOrFail loc ty
checkTerminaType loc (TBoxSubtype ty) =
  checkTerminaType loc ty >>
  boxTyOrFail loc ty
checkTerminaType loc (TLocation ty) =
  checkTerminaType loc ty >>
  locTyOrFail loc ty
checkTerminaType loc (TAccessPort ty) =
  checkTerminaType loc ty >>
  accessPortTyOrFail loc ty
checkTerminaType loc (TSinkPort ty _) =
  checkTerminaType loc ty >>
  msgTyOrFail loc ty
checkTerminaType loc (TInPort ty _) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty
checkTerminaType loc (TOutPort ty) = 
  checkTerminaType loc ty >>
  msgTyOrFail loc ty
checkTerminaType loc (TAllocator ty) = 
  checkTerminaType loc ty >>
  allocTyOrFail loc ty
checkTerminaType loc (TAtomicAccess ty) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicAccessInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomicArrayAccess ty s) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicArrayAccessInvalidType (numTyOrFail loc ty) >>
  checkSize loc s
checkTerminaType loc (TAtomic ty) = 
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicInvalidType (numTyOrFail loc ty)
checkTerminaType loc (TAtomicArray ty s) =
  checkTerminaType loc ty >>
  catchExpectedNum loc EAtomicArrayInvalidType (numTyOrFail loc ty) >>
  checkSize loc s
-- This is explicit just in case
checkTerminaType _ TUInt8                   = return ()
checkTerminaType _ TUInt16                  = return ()
checkTerminaType _ TUInt32                  = return ()
checkTerminaType _ TUInt64                  = return ()
checkTerminaType _ TInt8                    = return ()
checkTerminaType _ TInt16                   = return ()
checkTerminaType _ TInt32                   = return ()
checkTerminaType _ TInt64                   = return ()
checkTerminaType _ TUSize                   = return ()
checkTerminaType _ TChar                    = return ()
checkTerminaType _ TBool                    = return ()
checkTerminaType _ TUnit                    = return ()

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an internal error.
getObjectType :: Object SemanticAnn -> SemanticMonad (AccessKind, TerminaType)
getObjectType = maybe (throwError $ annotateError Internal EUnboxingObject) return . getObjectSAnns . getAnnotation

getExpType :: Expression SemanticAnn -> SemanticMonad TerminaType
getExpType
  = maybe (throwError $ annotateError Internal EUnboxingStmtExpr) return
  . getResultingType . getSemanticAnn . getAnnotation

checkConstant :: TLocation -> TerminaType -> Const -> SemanticMonad ()
checkConstant loc expected_type (I ti (Just type_c)) =
  -- |type_c| is correct
  checkTerminaType loc type_c >>
  -- | Check that the explicit type matches the expected type
  sameTyOrError loc expected_type type_c >>
  -- | Check that the constant is in the range of the type
  checkIntConstant loc type_c ti
checkConstant loc expected_type (I ti Nothing) =
  -- | Check that the constant is in the range of the type
  case expected_type of
    TBool -> throwError $ annotateError loc ENumericConstantNotBool
    TChar -> throwError $ annotateError loc ENumericConstantNotChar
    _ -> checkIntConstant loc expected_type ti
checkConstant loc expected_type (B {}) =
  sameTyOrError loc expected_type TBool
checkConstant loc expected_type (C {}) =
  sameTyOrError loc expected_type TChar

checkIntConstant :: TLocation -> TerminaType -> TInteger -> SemanticMonad ()
checkIntConstant loc tyI ti@(TInteger i _) =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I ti (Just tyI)))
