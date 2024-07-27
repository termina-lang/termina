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
import AST.Parser
import AST.Seman as SAST
import AST.Core as CAST
import Utils.AST.Parser (checkEqTypes)

import Semantic.Errors.Errors
import Semantic.Types
import Utils.TypeSpecifier

-- Monads
import Control.Monad.Except
import qualified Control.Monad.State.Strict as ST
import Data.Functor

data SAnns a = SemAnn
  { -- | Location on source code
    location :: Location
    -- | Type after type checking
  , ty_ann   :: a}
  deriving Show

data ObjectAnn = ObjectAnn
  {
    accessKind :: AccessKind
  , objTy      :: TypeSpecifier
  } deriving Show

instance Annotated SAnns where
  getAnnotation = ty_ann

data ESeman
  = SimpleType TypeSpecifier
  | ObjectType AccessKind TypeSpecifier
  | AppType [Parameter] TypeSpecifier
  deriving Show

newtype SemanProcedure = SemanProcedure Identifier
  deriving (Show)

data ConnectionSeman =
    -- | Access port connection
  APConnTy
  -- | Type specifier of the connected resource
    TypeSpecifier
    -- | List of procedures that can be called on the connected resource
    [SemanProcedure]
  | APAtomicConnTy
    -- | Type specifier of the connected atomic
    TypeSpecifier
  | APAtomicArrayConnTy
    -- | type specifier of the connected atomic array
    TypeSpecifier
    -- | Size of the connected atomic array
    Size
  | APPoolConnTy
    -- | Type specifier of the connected pool
    TypeSpecifier
    -- | Size of the connected pool
    Size
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    TypeSpecifier
    -- | Name of the action that will be triggered when the event emitter emits an event 
    Identifier 
  -- | In port connection
  | InPConnTy
    -- | Type specifier of the connected channel
    TypeSpecifier
    -- | Name of the action that will be triggered when the channel receives a message
    Identifier
  | OutPConnTy
    -- | Type specifier of the connected channel
    TypeSpecifier
  deriving Show

-- | Semantic elements
-- we have three different semantic elements:
data SemanticElems
  =
  -- | Expressions with their types
  ETy ESeman
  -- | Statements with no types
  | STy
  -- | Port connections
  | CTy ConnectionSeman
  -- | Global elements
  | GTy (GEntry SemanticAnns)
  deriving Show

----------------------------------------
getEType :: SemanticElems -> Maybe ESeman
getEType (ETy t) = Just t
getEType _ = Nothing

getResultingType :: SemanticElems -> Maybe TypeSpecifier
getResultingType (ETy ty) = Just (case ty of {SimpleType t -> t; ObjectType _ t -> t; AppType _ t -> t})
getResultingType _        = Nothing

getObjectSAnns :: SemanticAnns -> Maybe (AccessKind, TypeSpecifier)
getObjectSAnns (SemAnn _ (ETy (ObjectType ak ty))) = Just (ak, ty)
getObjectSAnns _                                   = Nothing

getArgumentsType :: SemanticElems -> Maybe [Parameter]
getArgumentsType (ETy (AppType ts _)) = Just ts
getArgumentsType _                    = Nothing

isResultFromApp :: SemanticElems -> Bool
isResultFromApp = isJust . getArgumentsType
----------------------------------------

getGEntry :: SemanticElems -> Maybe (GEntry SemanticAnns)
getGEntry (GTy a) = Just a
getGEntry _       = Nothing

buildExpAnn :: Location -> TypeSpecifier -> SAnns SemanticElems
buildExpAnn loc = SemAnn loc . ETy . SimpleType

buildExpAnnObj :: Location -> AccessKind -> TypeSpecifier -> SAnns SemanticElems
buildExpAnnObj loc ak = SemAnn loc . ETy . ObjectType ak

buildExpAnnApp :: Location -> [Parameter] -> TypeSpecifier -> SAnns SemanticElems
buildExpAnnApp loc tys = SemAnn loc . ETy . AppType tys

buildGlobalAnn :: Location -> SemGlobal -> SAnns SemanticElems
buildGlobalAnn loc = SemAnn loc . GTy . GGlob

buildGlobal :: Location -> GEntry SemanticAnns -> SAnns SemanticElems
buildGlobal loc = SemAnn loc . GTy

buildGlobalTy :: Location -> SemanTypeDef SemanticAnns -> SAnns SemanticElems
buildGlobalTy loc = SemAnn loc . GTy . GType

buildStmtAnn :: Location -> SAnns SemanticElems
buildStmtAnn = flip SemAnn STy

buildOutPortConnAnn :: Location -> TypeSpecifier -> SAnns SemanticElems
buildOutPortConnAnn loc ts = SemAnn loc (CTy (OutPConnTy ts))

buildAccessPortConnAnn :: Location -> TypeSpecifier -> [SemanProcedure] -> SAnns SemanticElems
buildAccessPortConnAnn loc ts procs = SemAnn loc (CTy (APConnTy ts procs))

buildPoolConnAnn :: Location -> TypeSpecifier -> Size -> SAnns SemanticElems
buildPoolConnAnn loc ts s = SemAnn loc (CTy (APPoolConnTy ts s))

buildAtomicConnAnn :: Location -> TypeSpecifier -> SAnns SemanticElems
buildAtomicConnAnn loc ts = SemAnn loc (CTy (APAtomicConnTy ts))

buildAtomicArrayConnAnn :: Location -> TypeSpecifier -> Size -> SAnns SemanticElems
buildAtomicArrayConnAnn loc ts s = SemAnn loc (CTy (APAtomicArrayConnTy ts s))

buildSinkPortConnAnn :: Location -> TypeSpecifier -> Identifier -> SAnns SemanticElems
buildSinkPortConnAnn loc ts action = SemAnn loc (CTy (SPConnTy ts action))

buildInPortConnAnn :: Location -> TypeSpecifier -> Identifier -> SAnns SemanticElems
buildInPortConnAnn loc ts action = SemAnn loc (CTy (InPConnTy ts action))

-- | Expression Semantic Annotations
type SemanticAnns = SAnns SemanticElems

forgetSemAnn :: SAnns a -> Location
forgetSemAnn = location

getTypeSAnns :: SemanticAnns -> Maybe TypeSpecifier
getTypeSAnns  = getResultingType . ty_ann

undynExpType :: ESeman -> ESeman
undynExpType (SimpleType (DynamicSubtype ty)) = SimpleType ty
undynExpType (ObjectType ak (DynamicSubtype ty)) = ObjectType ak ty
undynExpType (AppType ts (DynamicSubtype ty)) = AppType ts ty
undynExpType _ = error "impossible 888+1"

undynTypeAnn :: SemanticAnns -> SemanticAnns
undynTypeAnn (SemAnn p (ETy en)) = SemAnn p (ETy (undynExpType en))
undynTypeAnn _                                    = error "impossible 888"

----------------------------------------
-- | Global env
-- It has global definitions
type GlobalEnv = Map Identifier (SAnns (GEntry SemanticAnns))

-- | Local env
-- variables to their type
type LocalEnv = Map Identifier (AccessKind, TypeSpecifier)

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

stdlibGlobalEnv :: [(Identifier, SAnns (GEntry SemanticAnns))]
stdlibGlobalEnv =
  [("Result", Internal `SemAnn` GType (Enum "Result" [EnumVariant "Ok" [], EnumVariant "Error" []] [])),
   ("TimeVal",Internal `SemAnn` GType (Struct "TimeVal" [FieldDefinition "tv_sec" UInt32, FieldDefinition "tv_usec" UInt32] [])),
   ("Interrupt", Internal `SemAnn` GType (Class EmitterClass "Interrupt" [] [] [])),
   ("SystemInit", Internal `SemAnn` GType (Class EmitterClass "SystemInit" [] [] [])),
   ("system_init", Internal `SemAnn` GGlob (SEmitter (DefinedType "SystemInit"))),
   ("PeriodicTimer", Internal `SemAnn` GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (DefinedType "TimeVal")) (buildExpAnn Internal (DefinedType "TimeVal"))] [] [])),
   ("clock_get_uptime",Internal `SemAnn` GFun [Parameter "uptime" (Reference Mutable (DefinedType "TimeVal"))] Unit),
   ("delay_in",Internal `SemAnn` GFun [Parameter "time_val" (Reference Immutable (DefinedType "TimeVal"))] Unit)]

makeInitialGlobalEnv :: [(Identifier, SAnns (GEntry SemanticAnns))] -> Environment
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

-- Execute comp but bracktracking the state
-- Useful for blocks semantic analysis
localScope :: SemanticMonad a -> SemanticMonad a
localScope comp = do
  prevst <- get
  res <- comp
  put prevst
  return res

----------------------------------------
-- Some helper functions to bring information from the environment.

-- | Get global definition of a Type
getGlobalTypeDef :: Location -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnns)
getGlobalTypeDef loc tid  = gets global >>=
  maybe
  -- if there is no varialbe name |tid|
  (throwError $ annotateError loc (ENoTyFound tid))
  -- if so, return its type
  (\case {
      GType tydef -> return tydef;
        _         -> throwError $ annotateError loc (EGlobalNoType tid)
      } . ty_ann) . M.lookup tid

getFunctionTy :: Location -> Identifier -> SemanticMonad ([Parameter],TypeSpecifier, Location)
getFunctionTy loc iden =
  catchError (getGlobalEntry loc iden ) (\_ -> throwError $ annotateError loc (EFunctionNotFound iden))
  >>= \case
  SemAnn entryLoc (GFun args retty) -> return (args, retty, entryLoc)
  SemAnn {} -> throwError $ annotateError loc (EFunctionNotFound iden)

-- | Add new *local* immutable objects and execute computation in the
-- new local environment.
addLocalImmutObjs :: Location -> [(Identifier, TypeSpecifier)] -> SemanticMonad a -> SemanticMonad a
addLocalImmutObjs loc newVars ma  =
  localScope (addVariables newVars >> ma)
  where
    addVariables = mapM_ (uncurry (insertLocalImmutObj loc))

-- | Insert mutable object (variable) in local scope.
insertLocalMutObj :: Location -> Identifier -> TypeSpecifier -> SemanticMonad ()
insertLocalMutObj loc ident ty =
  isDefined ident
  >>= \b -> if b
  then -- | if there is throw error
  throwError $ annotateError loc $ EVarDefined ident
  else -- | If there is no variable named |ident|
  modify (\s -> s{local = M.insert ident (Mutable, ty) (local s)})

-- | Insert immutable object (variable) in local scope.
insertLocalImmutObj :: Location -> Identifier -> TypeSpecifier -> SemanticMonad ()
insertLocalImmutObj loc ident ty =
  isDefined ident
  >>= \b -> if b
  then -- | if there is throw error
  throwError $ annotateError loc $ EVarDefined ident
  else -- | If there is no variable named |ident|
  modify (\s -> s{local = M.insert ident (Immutable, ty) (local s)})

insertGlobalTy :: Location -> SemanTypeDef SemanticAnns -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (loc `SemAnn` GType tydef) (EUsedTypeName type_name)
 where
   type_name = identifierType tydef

insertGlobalFun :: Location -> Identifier -> [Parameter] -> TypeSpecifier -> SemanticMonad ()
insertGlobalFun loc ident ps rettype =
  insertGlobal ident (loc `SemAnn` GFun ps rettype) (EUsedFunName ident)

insertGlobal :: Identifier -> SAnns (GEntry SemanticAnns) -> (Location -> Error Location) -> SemanticMonad ()
insertGlobal ident entry err =
  glbWhereIsDefined ident >>=
  \case
    { Just l -> throwError (annotateError (location entry) (err l)) ;
      Nothing -> modify (\s -> s{global = M.insert ident entry (global s)}) ;
    }
  -- if b then throwError (annotateError (location entry) getError)
  -- else

insertLocalVariables :: Location -> [(Identifier , TypeSpecifier)] -> SemanticMonad ()
insertLocalVariables loc = mapM_ (\case
  (ident, ty@(DynamicSubtype _)) -> insertLocalMutObj loc ident ty;
  (ident, ty) -> insertLocalImmutObj loc ident ty)

-- | Get the type of a local (already) defined object. If it is not defined throw an error.
getLocalObjTy :: Location -> Identifier -> SemanticMonad (AccessKind, TypeSpecifier)
getLocalObjTy loc ident =
  -- | Get local objects map and check if |ident| is a member of that map
  maybe
    -- | if |ident| is not a member throw error |ENotNamedObject|
    (throwError $ annotateError loc (ENotNamedObject ident))
    -- | if |ident| is a member return its type and access kind
    return . M.lookup ident =<< gets local

-- | Get the Type of a defined  readonlye variable. If it is not defined throw an error.
getConst :: Location -> Identifier -> SemanticMonad (TypeSpecifier, Const)
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
                    SemAnn _ (GGlob (SConst ts value))  -> return (ts, value);
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
getGlobalEntry :: Location -> Identifier -> SemanticMonad (SAnns (GEntry SemanticAnns))
getGlobalEntry loc ident =
  gets global
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedGlobal ident))) return . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy, getRHSVarTy, getGlobalVarTy ::
  Location
  -> Identifier
  -> SemanticMonad (AccessKind, TypeSpecifier)
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
            }) >> throwError (annotateError loc (EObjectIsReadOnly ident))
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
                    ENotNamedGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                    _  -> throwError errorGlobal;
                  }
                ) >>= (\case{
                        SemAnn _ (GGlob _) -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
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
                        SemAnn _ (GGlob (SResource ts))  -> return (Mutable, ts);
                        _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                      });


-- | Lookups |idenfitier| in a given scope.
-- Returns true if the identifier is defined within the scope or false otherwise.
isDefinedIn :: Identifier -> M.Map Identifier a -> Bool
isDefinedIn = M.member

glbWhereIsDefined :: Identifier -> SemanticMonad (Maybe Location)
glbWhereIsDefined i = fmap location . M.lookup i <$> gets global

isDefined :: Identifier -> SemanticMonad Bool
isDefined ident =
  get <&> (\st -> isDefinedIn ident (global st) || isDefinedIn ident (local st))

-------------
-- Type |Type| helpers!
-- | Checks if two type are the same numeric type.
sameNumTy :: TypeSpecifier -> TypeSpecifier -> Bool
sameNumTy a b = checkEqTypes a b && numTy a

-- | Checks if two type are the same numeric type.
-- If they are not, it throws a mismatch error.
checkEqTypesOrError :: Location -> TypeSpecifier -> TypeSpecifier -> SemanticMonad ()
checkEqTypesOrError loc t1 t2 =
  unless (checkEqTypes t1 t2) (throwError $ annotateError loc $ EMismatch t1 t2)

unDyn :: SAST.Object SemanticAnns -> SAST.Object SemanticAnns
unDyn t = Undyn t (undynTypeAnn (getAnnotation t))

unDynExp :: SAST.Expression SemanticAnns -> SemanticMonad (SAST.Expression SemanticAnns)
unDynExp (SAST.AccessObject obj) =  return $ SAST.AccessObject (unDyn obj)
unDynExp _ = throwError $ annotateError Internal EUnDynExpression

mustBeTy :: TypeSpecifier -> SAST.Expression SemanticAnns -> SemanticMonad (SAST.Expression SemanticAnns)
mustBeTy ty expression =
  getExpType expression >>=
  checkEqTypesOrError loc ty
  >> return expression
  where
    ann_exp = getAnnotation expression
    loc = location ann_exp

blockRetTy :: TypeSpecifier -> SAST.BlockRet SemanticAnns -> SemanticMonad ()
blockRetTy ty (BlockRet _bd (ReturnStmt _me ann)) =
  maybe
  (throwError (annotateError Internal EUnboxingBlockRet))
  (void . checkEqTypesOrError (location ann) ty) (getResultingType (ty_ann ann))

getIntConst :: Location -> Const -> SemanticMonad Integer
getIntConst _ (I (TInteger i _) _) = return i
getIntConst loc e     = throwError $ annotateError loc $ ENotIntConst e

-- Helper function failing if a given |TypeSpecifier| is not *simple* |simpleType|.
simpleTyorFail :: Location -> TypeSpecifier -> SemanticMonad ()
simpleTyorFail pann ty = unless (simpleType ty) (throwError (annotateError pann (EExpectedSimple ty)))

-- Helper function failing if a given |TypeSpecifier| cannot be used to define a class field.
classFieldTyorFail :: Location -> TypeSpecifier -> SemanticMonad ()
classFieldTyorFail pann ty = unless (classFieldType ty) (throwError (annotateError pann (EInvalidClassFieldType ty)))

checkSize :: Location -> Size -> SemanticMonad ()
checkSize loc (CAST.K s) = checkIntConstant loc USize s
checkSize loc (CAST.V ident) = getConst loc ident >>= (\(ty, _) -> checkEqTypesOrError loc USize ty) >> return ()

-- | Function checking that a TypeSpecifier is well-defined.
-- This is not the same as defining a type, but it is similar.
-- Some types can be found in the wild, but user defined types are just check
-- they exist (they were defined preivously).
-- Note that we do not change the |TypeSpecifier| in any way, that's why this
-- function return |()|.
checkTypeSpecifier :: Location -> TypeSpecifier -> SemanticMonad ()
checkTypeSpecifier loc (DefinedType identTy) =
  -- Check that the type was defined
  void (getGlobalTypeDef loc identTy)
  -- we assume that only well-formed types are added to globals.
checkTypeSpecifier loc (Array ty s) =
  -- Doc: https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Arrays .
  -- Only arrays of simple types.
  simpleTyorFail loc ty >>
  checkTypeSpecifier loc ty >>
  checkSize loc s
checkTypeSpecifier loc (Slice ty) =
  -- Only slices of simple types.
  simpleTyorFail loc ty >>
  checkTypeSpecifier loc ty
checkTypeSpecifier loc (MsgQueue ty s) = checkTypeSpecifier loc ty >> checkSize loc s
checkTypeSpecifier loc (Pool ty s) = checkTypeSpecifier loc ty >> checkSize loc s
-- Dynamic Subtyping
checkTypeSpecifier loc (Option tyd@(DynamicSubtype _ty)) = checkTypeSpecifier loc tyd
-- Regular option subtyping
checkTypeSpecifier loc (Option (Option _))     = throwError $ annotateError loc EOptionNested
checkTypeSpecifier loc (Option ty) = simpleTyorFail loc ty >> checkTypeSpecifier loc ty
checkTypeSpecifier loc (Reference _ ty)        =
  -- Unless we are referencing a reference we are good
  unless (referenceType ty) (throwError (annotateError loc (EReferenceTy ty))) >>
  checkTypeSpecifier loc ty
checkTypeSpecifier loc (DynamicSubtype (Option _)) = throwError $ annotateError loc EOptionNested
checkTypeSpecifier loc (DynamicSubtype ty) =
  simpleTyorFail loc ty >>
  checkTypeSpecifier loc ty
checkTypeSpecifier loc (Location ty) =
  simpleTyorFail loc ty >>
  checkTypeSpecifier loc ty
checkTypeSpecifier loc (AccessPort ty) =
  case ty of
    (Allocator (Option _))  -> throwError $ annotateError loc EOptionNested
    (Allocator ty') -> simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
    (AtomicAccess ty') -> unless (numTy ty') (throwError $ annotateError loc (EAtomicAccessInvalidType ty'))
    (AtomicArrayAccess ty' s) -> 
      unless (numTy ty') (throwError $ annotateError loc (EAtomicArrayAccessInvalidType ty')) >> checkSize loc s
    (DefinedType identTy) ->
      getGlobalTypeDef loc identTy >>=
        \case
          (Interface {}) -> return ()
          _ -> throwError $ annotateError loc $ EAccessPortNotInterface ty
    _ -> throwError $ annotateError loc $ EAccessPortNotInterface ty
checkTypeSpecifier loc (SinkPort (Option _) _) = throwError $ annotateError loc EOptionNested
checkTypeSpecifier loc (SinkPort ty' _) = simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
checkTypeSpecifier loc (InPort (Option _) _) = throwError $ annotateError loc EOptionNested
checkTypeSpecifier loc (InPort (DynamicSubtype ty') _) = simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
checkTypeSpecifier loc (InPort ty' _) = simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
checkTypeSpecifier loc (OutPort (Option _)) = throwError $ annotateError loc EOptionNested
checkTypeSpecifier loc (OutPort (DynamicSubtype ty')) = simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
checkTypeSpecifier loc (OutPort ty') = simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
checkTypeSpecifier loc (Allocator (Option _)) = throwError $ annotateError loc EOptionNested
checkTypeSpecifier loc (Allocator ty') = simpleTyorFail loc ty' >> checkTypeSpecifier loc ty'
checkTypeSpecifier loc (AtomicAccess ty') = unless (numTy ty') (throwError $ annotateError loc (EAtomicAccessInvalidType ty'))
checkTypeSpecifier loc (AtomicArrayAccess ty' s) = 
  unless (numTy ty') (throwError $ annotateError loc (EAtomicArrayAccessInvalidType ty')) >> checkSize loc s
checkTypeSpecifier loc (Atomic ty') = unless (numTy ty') (throwError $ annotateError loc (EAtomicInvalidType ty'))
checkTypeSpecifier loc (AtomicArray ty' s) =
  unless (numTy ty') (throwError $ annotateError loc (EAtomicArrayInvalidType ty')) >> checkSize loc s
-- This is explicit just in case
checkTypeSpecifier _ UInt8                   = return ()
checkTypeSpecifier _ UInt16                  = return ()
checkTypeSpecifier _ UInt32                  = return ()
checkTypeSpecifier _ UInt64                  = return ()
checkTypeSpecifier _ Int8                    = return ()
checkTypeSpecifier _ Int16                   = return ()
checkTypeSpecifier _ Int32                   = return ()
checkTypeSpecifier _ Int64                   = return ()
checkTypeSpecifier _ USize                   = return ()
checkTypeSpecifier _ Char                    = return ()
checkTypeSpecifier _ Bool                    = return ()
checkTypeSpecifier _ Unit                    = return ()

-- | This function gets the access kind and type of an already semantically
-- annotated object. If the object is not annotated properly, it throws an internal error.
getObjectType :: SAST.Object SemanticAnns -> SemanticMonad (AccessKind, TypeSpecifier)
getObjectType = maybe (throwError $ annotateError Internal EUnboxingObject) return . getObjectSAnns . getAnnotation

getExpType :: SAST.Expression SemanticAnns -> SemanticMonad TypeSpecifier
getExpType
  = maybe (throwError $ annotateError Internal EUnboxingStmtExpr) return
  . getResultingType . ty_ann . getAnnotation

checkConstant :: Location -> TypeSpecifier -> Const -> SemanticMonad ()
checkConstant loc expected_type (I ti (Just type_c)) =
  -- |type_c| is correct
  checkTypeSpecifier loc type_c >>
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

checkIntConstant :: Location -> TypeSpecifier -> TInteger -> SemanticMonad ()
checkIntConstant loc tyI ti@(TInteger i _) =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I ti (Just tyI)))

buildSize :: ConstExpression SemanticAnns -> SemanticMonad Size
buildSize (KC (I ti _) _) = return (SAST.K ti)
buildSize (KV ident _) = return (SAST.V ident)
buildSize (KC c _) = throwError $ annotateError Internal $ ENotIntConst c