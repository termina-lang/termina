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
      PortConnection _ -> Unit})
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

buildExpAnn :: Location -> TerminaType -> SemanticAnn
buildExpAnn loc = locate loc . ETy . SimpleType

buildExpAnnObj :: Location -> AccessKind -> TerminaType -> SemanticAnn
buildExpAnnObj loc ak = locate loc . ETy . ObjectType ak

buildExpAnnApp :: Location -> [TerminaType] -> TerminaType -> SemanticAnn
buildExpAnnApp loc tys = locate loc . ETy . AppType tys

buildGlobalAnn :: Location -> SemGlobal -> SemanticAnn
buildGlobalAnn loc = locate loc . GTy . GGlob

buildGlobal :: Location -> GEntry SemanticAnn -> SemanticAnn
buildGlobal loc = locate loc . GTy

buildGlobalTy :: Location -> SemanTypeDef SemanticAnn -> SemanticAnn
buildGlobalTy loc = locate loc . GTy . GType

buildStmtAnn :: Location -> SemanticAnn
buildStmtAnn = Located (STy SimpleStmtType)

buildStmtMatchCaseAnn :: Location -> [TerminaType] -> SemanticAnn
buildStmtMatchCaseAnn loc ts = locate loc (STy (MatchCaseStmtType ts))

buildOutPortConnAnn :: Location -> TerminaType -> SemanticAnn
buildOutPortConnAnn loc ts = locate loc (ETy (PortConnection (OutPConnTy ts)))

buildAccessPortConnAnn :: Location -> TerminaType -> [SemanProcedure] -> SemanticAnn
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

unboxExpType :: ESeman -> ESeman
unboxExpType (SimpleType (BoxSubtype ty)) = SimpleType ty
unboxExpType (ObjectType ak (BoxSubtype ty)) = ObjectType ak ty
unboxExpType (AppType ts (BoxSubtype ty)) = AppType ts ty
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
   ("TimeVal", Located (GType (Struct "TimeVal" [FieldDefinition "tv_sec" UInt32, FieldDefinition "tv_usec" UInt32] [])) Internal),
   ("Interrupt", Located (GType (Class EmitterClass "Interrupt" [] [] [])) Internal),
   ("SystemInit", Located (GType (Class EmitterClass "SystemInit" [] [] [])) Internal),
   ("system_init", Located (GGlob (SEmitter (DefinedType "SystemInit"))) Internal),
   ("PeriodicTimer", Located (GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (DefinedType "TimeVal")) (buildExpAnn Internal (DefinedType "TimeVal"))] [] [])) Internal),
   ("clock_get_uptime", Located (GFun [Reference Mutable (DefinedType "TimeVal")] Unit) Internal),
   ("delay_in", Located (GFun [Reference Immutable (DefinedType "TimeVal")] Unit) Internal)]

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
      GType tydef -> return tydef;
        _         -> throwError $ annotateError loc (EGlobalNotType tid)
      } . getEntry) . M.lookup tid

getFunctionTy :: Location -> Identifier -> SemanticMonad ([TerminaType], TerminaType, Location)
getFunctionTy loc iden =
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError loc (EFunctionNotFound iden))
  >>= \case
  Located (GFun args retty) entryLoc -> return (args, retty, entryLoc)
  Located _ entryLoc -> throwError $ annotateError loc (EGlobalNotFunction (iden, entryLoc))

getEnumTy :: Location -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnn, Location)
getEnumTy loc iden = 
  catchError (getGlobalEntry loc iden) (\_ -> throwError $ annotateError loc (ENoEnumFound iden)) >>=
  (\case {
      Located (GType tydef@(Enum {})) entryLoc  -> return (tydef, entryLoc);
      Located _ entryLoc           -> throwError $ annotateError loc (EGlobalNotEnum (iden, entryLoc))
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
    Just prevloc -> throwError $ annotateError loc $ ESymbolDefined ident prevloc

-- | Insert immutable object (variable) in local scope.
insertLocalImmutObj :: Location -> Identifier -> TerminaType -> SemanticMonad ()
insertLocalImmutObj loc ident ty = do
  prev <- whereIsDefined ident
  case prev of
    Nothing -> modify (\s -> s{local = M.insert ident (Located (Immutable, ty) loc) (local s)})
    Just prevloc -> throwError $ annotateError loc $ ESymbolDefined ident prevloc
  
insertGlobalTy :: Location -> SemanTypeDef SemanticAnn -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (Located (GType tydef) loc) (EUsedTypeName type_name)
 where
   type_name = identifierType tydef

insertGlobalFun :: Location -> Identifier -> [TerminaType] -> TerminaType -> SemanticMonad ()
insertGlobalFun loc ident ps rettype =
  insertGlobal ident (Located (GFun ps rettype) loc) (EUsedFunName ident)

insertGlobal :: Identifier -> Located (GEntry SemanticAnn) -> (Location -> Error Location) -> SemanticMonad ()
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
  (ident, ty@(BoxSubtype _)) -> insertLocalMutObj loc ident ty;
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

-- | Get the Type of a defined  readonlye variable. If it is not defined throw an error.
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
                    Located (GGlob (SConst ts value)) _ -> return (ts, value);
                    -- | It is a global object, but not a constant.
                    _ -> throwError $ annotateError loc (ENotConstant ident);
    });

getIntSize :: Location -> Size -> SemanticMonad Integer
getIntSize loc (CAST.V ident) = do
  (ty, value) <- getConst loc ident
  checkEqTypesOrError loc USize ty
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
                        Located (GGlob (SResource ts)) _  -> return (Mutable, ts);
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

-------------
-- Type |Type| helpers!
-- | Checks if two type are the same numeric type.
sameNumTy :: TerminaType -> TerminaType -> Bool
sameNumTy a b = checkEqTypes a b && numTy a

-- | Checks if two type are the same numeric type.
-- If they are not, it throws a mismatch error.
checkEqTypesOrError :: Location -> TerminaType -> TerminaType -> SemanticMonad ()
checkEqTypesOrError loc t1 t2 =
  unless (checkEqTypes t1 t2) (throwError $ annotateError loc $ EMismatch t1 t2)

unBox :: Object SemanticAnn -> Object SemanticAnn
unBox t = Unbox t (unboxTypeAnn (getAnnotation t))

unBoxExp :: Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
unBoxExp (AccessObject obj) =  return $ AccessObject (unBox obj)
unBoxExp _ = throwError $ annotateError Internal EUnBoxExpression

mustBeTy :: TerminaType -> Expression SemanticAnn -> SemanticMonad (Expression SemanticAnn)
mustBeTy ty expression =
  getExpType expression >>=
  checkEqTypesOrError loc ty
  >> return expression
  where
    ann_exp = getAnnotation expression
    loc = location ann_exp

{--
blockRetTy :: TerminaType -> BlockRet SemanticAnn -> SemanticMonad ()
blockRetTy ty (BlockRet _bd (ReturnStmt _me ann)) =
  maybe
  (throwError (annotateError Internal EUnboxingBlockRet))
  (void . checkEqTypesOrError (location ann) ty) (getResultingType (getSemanticAnn ann)) --}

getIntConst :: Location -> Const -> SemanticMonad Integer
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

catchExpectedSimple ::
  -- | Location of the error
  Parser.ParserAnn
  -- | Function to create the error
  -> (TerminaType -> Error Location)
  -- | Action to execute
  -> SemanticMonad a
  -- | Action to execute
  -> SemanticMonad a
catchExpectedSimple ann ferror action = catchError action (\err -> case getError err of
  EExpectedSimple ty -> throwError $ annotateError ann (ferror ty)
  _ -> throwError err)

-- Helper function failing if a given |TerminaType| is not *simple* |simpleType|.
simpleTyorFail :: Location -> TerminaType -> SemanticMonad ()
simpleTyorFail pann ty = unless (simpleType ty) (throwError (annotateError pann (EExpectedSimple ty)))

-- Helper function failing if a given |TerminaType| cannot be used to define a class field.
classFieldTyorFail :: Location -> TerminaType -> SemanticMonad ()
classFieldTyorFail pann ty = unless (classFieldType ty) (throwError (annotateError pann (EInvalidClassFieldType ty)))

checkSize :: Location -> Size -> SemanticMonad ()
checkSize loc (CAST.K s) = checkIntConstant loc USize s
checkSize loc (CAST.V ident) = getConst loc ident >>= (\(ty, _) -> checkEqTypesOrError loc USize ty) >> return ()

-- | Function checking that a TerminaType is well-defined.
-- This is not the same as defining a type, but it is similar.
-- Some types can be found in the wild, but user defined types are just check
-- they exist (they were defined preivously).
-- Note that we do not change the |TerminaType| in any way, that's why this
-- function return |()|.
checkTerminaType :: Location -> TerminaType -> SemanticMonad ()
checkTerminaType loc (DefinedType identTy) =
  -- Check that the type was defined
  void (getGlobalTypeDef loc identTy)
  -- we assume that only well-formed types are added to globals.
checkTerminaType loc (Array ty s) =
  -- Doc: https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Arrays .
  -- Only arrays of simple types.
  catchExpectedSimple loc EInvalidArrayType (simpleTyorFail loc ty) >>
  checkTerminaType loc ty >>
  checkSize loc s
checkTerminaType loc (MsgQueue ty s) = checkTerminaType loc ty >> checkSize loc s
checkTerminaType loc (Pool ty s) = checkTerminaType loc ty >> checkSize loc s
-- Box Subtyping
checkTerminaType loc (Option tyd@(BoxSubtype _ty)) = checkTerminaType loc tyd
-- Regular option subtyping
checkTerminaType loc (Option (Option _))     = throwError $ annotateError loc EOptionNested
checkTerminaType loc (Option ty) = simpleTyorFail loc ty >> checkTerminaType loc ty
checkTerminaType loc (Reference _ ty)        =
  -- Unless we are referencing a reference we are good
  unless (referenceType ty) (throwError (annotateError loc (EReferenceTy ty))) >>
  checkTerminaType loc ty
checkTerminaType loc (BoxSubtype ty@(Option _)) = throwError $ annotateError loc (EInvalidBoxType ty)
checkTerminaType loc (BoxSubtype ty) =
  catchExpectedSimple loc EInvalidBoxType (simpleTyorFail loc ty) >>
  checkTerminaType loc ty
checkTerminaType loc (Location ty) =
  simpleTyorFail loc ty >>
  checkTerminaType loc ty
checkTerminaType loc (AccessPort ty) =
  case ty of
    (Allocator (Option _))  -> throwError $ annotateError loc EOptionNested
    (Allocator ty') -> simpleTyorFail loc ty' >> checkTerminaType loc ty'
    (AtomicAccess ty') -> unless (numTy ty') (throwError $ annotateError loc (EAtomicAccessInvalidType ty'))
    (AtomicArrayAccess ty' s) -> 
      unless (numTy ty') (throwError $ annotateError loc (EAtomicArrayAccessInvalidType ty')) >> checkSize loc s
    (DefinedType identTy) ->
      getGlobalTypeDef loc identTy >>=
        \case
          (Interface {}) -> return ()
          _ -> throwError $ annotateError loc $ EAccessPortNotInterface ty
    _ -> throwError $ annotateError loc $ EAccessPortNotInterface ty
checkTerminaType loc (SinkPort (Option _) _) = throwError $ annotateError loc EOptionNested
checkTerminaType loc (SinkPort ty' _) = simpleTyorFail loc ty' >> checkTerminaType loc ty'
checkTerminaType loc (InPort (Option _) _) = throwError $ annotateError loc EOptionNested
checkTerminaType loc (InPort (BoxSubtype ty') _) = simpleTyorFail loc ty' >> checkTerminaType loc ty'
checkTerminaType loc (InPort ty' _) = simpleTyorFail loc ty' >> checkTerminaType loc ty'
checkTerminaType loc (OutPort (Option _)) = throwError $ annotateError loc EOptionNested
checkTerminaType loc (OutPort (BoxSubtype ty')) = simpleTyorFail loc ty' >> checkTerminaType loc ty'
checkTerminaType loc (OutPort ty') = simpleTyorFail loc ty' >> checkTerminaType loc ty'
checkTerminaType loc (Allocator (Option _)) = throwError $ annotateError loc EOptionNested
checkTerminaType loc (Allocator ty') = simpleTyorFail loc ty' >> checkTerminaType loc ty'
checkTerminaType loc (AtomicAccess ty') = unless (numTy ty') (throwError $ annotateError loc (EAtomicAccessInvalidType ty'))
checkTerminaType loc (AtomicArrayAccess ty' s) = 
  unless (numTy ty') (throwError $ annotateError loc (EAtomicArrayAccessInvalidType ty')) >> checkSize loc s
checkTerminaType loc (Atomic ty') = unless (numTy ty') (throwError $ annotateError loc (EAtomicInvalidType ty'))
checkTerminaType loc (AtomicArray ty' s) =
  unless (numTy ty') (throwError $ annotateError loc (EAtomicArrayInvalidType ty')) >> checkSize loc s
-- This is explicit just in case
checkTerminaType _ UInt8                   = return ()
checkTerminaType _ UInt16                  = return ()
checkTerminaType _ UInt32                  = return ()
checkTerminaType _ UInt64                  = return ()
checkTerminaType _ Int8                    = return ()
checkTerminaType _ Int16                   = return ()
checkTerminaType _ Int32                   = return ()
checkTerminaType _ Int64                   = return ()
checkTerminaType _ USize                   = return ()
checkTerminaType _ Char                    = return ()
checkTerminaType _ Bool                    = return ()
checkTerminaType _ Unit                    = return ()

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an internal error.
getObjectType :: Object SemanticAnn -> SemanticMonad (AccessKind, TerminaType)
getObjectType = maybe (throwError $ annotateError Internal EUnboxingObject) return . getObjectSAnns . getAnnotation

getExpType :: Expression SemanticAnn -> SemanticMonad TerminaType
getExpType
  = maybe (throwError $ annotateError Internal EUnboxingStmtExpr) return
  . getResultingType . getSemanticAnn . getAnnotation

checkConstant :: Location -> TerminaType -> Const -> SemanticMonad ()
checkConstant loc expected_type (I ti (Just type_c)) =
  -- |type_c| is correct
  checkTerminaType loc type_c >>
  -- | Check that the explicit type matches the expected type
  checkEqTypesOrError loc expected_type type_c >>
  -- | Check that the constant is in the range of the type
  checkIntConstant loc type_c ti
checkConstant loc expected_type (I ti Nothing) =
  -- | Check that the constant is in the range of the type
  checkIntConstant loc expected_type ti
checkConstant loc expected_type (B {}) =
  checkEqTypesOrError loc expected_type Bool
checkConstant loc expected_type (C {}) =
  checkEqTypesOrError loc expected_type Char

checkIntConstant :: Location -> TerminaType -> TInteger -> SemanticMonad ()
checkIntConstant loc tyI ti@(TInteger i _) =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I ti (Just tyI)))
