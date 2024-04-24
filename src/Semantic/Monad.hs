{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}


-- | Semantic Monad.
-- Here lives the monad we use to handle semantic pass effects.
-- It should be something like Exceptions + State

module Semantic.Monad where

import           Data.Map                   as M
import           Data.Maybe

-- Debugging
-- import Debugging

-- AST Info
import           Annotations
import           AST.Parser
import           AST.Seman                   as SAST
import           AST.Core                    as CAST
import           Utils.AST.Parser                  (groundTyEq)

import qualified Parser.Parsing                    as Parser (Annotation (..))
import           Semantic.Errors
import           Semantic.Types
import           Utils.TypeSpecifier

-- Monads
import           Control.Monad.Except
import qualified Control.Monad.State.Strict as ST

type Locations = Parser.Annotation

internalErrorSeman :: Locations
internalErrorSeman = Parser.Internal

data SAnns a = SemAnn
  { -- | Location on source code
    location :: Locations
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
  | AppType [ConstParameter] [Parameter] TypeSpecifier
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
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    TypeSpecifier
  -- | In port connection
  | InPConnTy
    -- | Type specifier of the connected channel
    TypeSpecifier
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
getResultingType (ETy ty) = Just (case ty of {SimpleType t -> t; ObjectType _ t -> t; AppType _ _ t -> t})
getResultingType _        = Nothing

getObjectSAnns :: SemanticAnns -> Maybe (AccessKind, TypeSpecifier)
getObjectSAnns (SemAnn _ (ETy (ObjectType ak ty))) = Just (ak, ty)
getObjectSAnns _                                   = Nothing

getArgumentsType :: SemanticElems -> Maybe ([ConstParameter], [Parameter])
getArgumentsType (ETy (AppType pts ts _)) = Just (pts, ts)
getArgumentsType _                    = Nothing

isResultFromApp :: SemanticElems -> Bool
isResultFromApp = isJust . getArgumentsType
----------------------------------------

getGEntry :: SemanticElems -> Maybe (GEntry SemanticAnns)
getGEntry (GTy a) = Just a
getGEntry _       = Nothing

buildExpAnn :: Locations -> TypeSpecifier -> SAnns SemanticElems
buildExpAnn loc = SemAnn loc . ETy . SimpleType

buildExpAnnObj :: Locations -> AccessKind -> TypeSpecifier -> SAnns SemanticElems
buildExpAnnObj loc ak = SemAnn loc . ETy . ObjectType ak

buildExpAnnApp :: Locations -> [ConstParameter] -> [Parameter] -> TypeSpecifier -> SAnns SemanticElems
buildExpAnnApp loc ctys tys = SemAnn loc . ETy . AppType ctys tys

buildGlobalAnn :: Locations -> SemGlobal -> SAnns SemanticElems
buildGlobalAnn loc = SemAnn loc . GTy . GGlob

buildGlobal :: Locations -> GEntry SemanticAnns -> SAnns SemanticElems
buildGlobal loc = SemAnn loc . GTy

buildGlobalTy :: Locations -> SemanTypeDef SemanticAnns -> SAnns SemanticElems
buildGlobalTy loc = SemAnn loc . GTy . GType

buildStmtAnn :: Locations -> SAnns SemanticElems
buildStmtAnn = flip SemAnn STy

buildOutPortConnAnn :: Locations -> TypeSpecifier -> SAnns SemanticElems
buildOutPortConnAnn loc ts = SemAnn loc (CTy (OutPConnTy ts))

buildAccessPortConnAnn :: Locations -> TypeSpecifier -> [SemanProcedure] -> SAnns SemanticElems
buildAccessPortConnAnn loc ts procs = SemAnn loc (CTy (APConnTy ts procs))

buildSinkPortConnAnn :: Locations -> TypeSpecifier -> SAnns SemanticElems
buildSinkPortConnAnn loc ts = SemAnn loc (CTy (SPConnTy ts))

buildInPortConnAnn :: Locations -> TypeSpecifier -> SAnns SemanticElems
buildInPortConnAnn loc ts = SemAnn loc (CTy (InPConnTy ts))

-- | Expression Semantic Annotations
type SemanticAnns = SAnns SemanticElems

forgetSemAnn :: SAnns a -> Locations
forgetSemAnn = location

getTypeSAnns :: SemanticAnns -> Maybe TypeSpecifier
getTypeSAnns  = getResultingType . ty_ann

undynExpType :: ESeman -> ESeman
undynExpType (SimpleType (DynamicSubtype ty)) = SimpleType ty
undynExpType (ObjectType ak (DynamicSubtype ty)) = ObjectType ak ty
undynExpType (AppType cts ts (DynamicSubtype ty)) = AppType cts ts ty
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
-- | Constants Environment.
type ConstEnv = Map Identifier TypeSpecifier
-- This may seem a bad decision, but each envornment represent something
-- different.
-- TODO We can use empty types to disable envirnoments and make Haskell do part
-- of our work.

-- | Environment required to type expression packed into just one type.
data ExpressionState
 = ExprST
 { global :: GlobalEnv
 , local  :: LocalEnv
 , consts :: ConstEnv
 }

data EnvKind =
  LocalKind AccessKind | ConstKind | GlobalKind
  deriving Show

 -- | This is the initial global environment.
 -- This is a temporary solution until we figure out how to manage the
 -- standard library. For the time being, initial type definitions such as
 -- "TaskRet" and "Result" are defined here.
initialGlobalEnv :: GlobalEnv
initialGlobalEnv = fromList initGlb

initGlb :: [(Identifier, SAnns (GEntry SemanticAnns))]
initGlb =
  [("Result", internalErrorSeman `SemAnn` GType (Enum "Result" [EnumVariant "Ok" [], EnumVariant "Error" []] [])),
   ("TimeVal",internalErrorSeman `SemAnn` GType (Struct "TimeVal" [FieldDefinition "tv_sec" UInt32, FieldDefinition "tv_usec" UInt32] [])),
   ("Interrupt", internalErrorSeman `SemAnn` GType (Class EmitterClass "Interrupt" [] [] [])),
   ("irq_3", internalErrorSeman `SemAnn` GGlob (SEmitter (DefinedType "Interrupt"))),
   ("SystemInit", internalErrorSeman `SemAnn` GType (Class EmitterClass "SystemInit" [] [] [])),
   ("system_init", internalErrorSeman `SemAnn` GGlob (SEmitter (DefinedType "SystemInit"))),
   ("PeriodicTimer", internalErrorSeman `SemAnn` GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (DefinedType "TimeVal")) (buildExpAnn internalErrorSeman (DefinedType "TimeVal"))] [] [])),
   ("clock_get_uptime",internalErrorSeman `SemAnn` GFun [] [Parameter "uptime" (Reference Mutable (DefinedType "TimeVal"))] Unit),
   ("delay_in",internalErrorSeman `SemAnn` GFun [] [Parameter "time_val" (Reference Immutable (DefinedType "TimeVal"))] Unit)]
  -- [("TaskRet", GType (Enum "TaskRet" [EnumVariant "Continue" [], EnumVariant "Finish" [], EnumVariant "Abort" [UInt32]] [])),
  --  ("Result", GType (Enum "Result" [EnumVariant "Ok" [], EnumVariant "Error" [UInt32]] [])),
  --  ("TimeVal", GType (Struct "TimeVal" [FieldDefinition "tv_sec" UInt32, FieldDefinition "tv_usec" UInt32] [])),
  --  ("clock_get_uptime", GFun [] (DefinedType "TimeVal")),
  --  ("delay_in", GFun [Parameter "time_val" (DefinedType "TimeVal")] Unit)]

makeInitial :: GlobalEnv -> ExpressionState
makeInitial e = ExprST e empty empty

initialExpressionSt :: ExpressionState
initialExpressionSt = ExprST initialGlobalEnv empty empty

type SemanticMonad = ExceptT SemanticErrors (ST.State ExpressionState)

----------------------------------------
gets :: (ExpressionState -> a) -> SemanticMonad a
gets = lift . ST.gets

get :: SemanticMonad ExpressionState
get = lift ST.get

put :: ExpressionState -> SemanticMonad ()
put = lift . ST.put

modify :: (ExpressionState -> ExpressionState) -> SemanticMonad ()
modify = lift . ST.modify

----------------------------------------
-- Monadic helpers.

-- | Execute computations in a temporal state wihtout
-- modifying current state.
withInState :: ExpressionState -> SemanticMonad a -> SemanticMonad a
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
getGlobalTy :: Locations -> Identifier -> SemanticMonad (SemanTypeDef SemanticAnns)
getGlobalTy loc tid  = gets global >>=
  maybe
  -- if there is no varialbe name |tid|
  (throwError $ annotateError loc (ENoTyFound tid))
  -- if so, return its type
  (\case {
      GType tydef -> return tydef;
        _         -> throwError $ annotateError loc (EGlobalNoType tid)
      } . ty_ann) . M.lookup tid

-- | From a global name get enum variations
getGlobalEnumTy :: Locations -> Identifier -> SemanticMonad [EnumVariant]
getGlobalEnumTy loc tid  = getGlobalTy loc tid  >>= \case
  Enum _ fs _mods -> return fs
  ty              -> throwError $ annotateError loc $ EMismatchIdNotEnum tid (fmap forgetSemAnn ty)

getFunctionTy :: Locations -> Identifier -> SemanticMonad ([ConstParameter], [Parameter],TypeSpecifier)
getFunctionTy loc iden =
  catchError (getGlobalGEnTy loc iden ) (\_ -> throwError $ annotateError loc (ENotAFun iden))
  >>= \case
  GFun constArgs args retty -> return (constArgs, args, retty)
  ge -> throwError $ annotateError loc (ENotFoundFun iden (fmap forgetSemAnn ge))



-- | Add new *local* immutable objects and execute computation in the
-- new local environment.
addLocalImmutObjs :: Locations -> [(Identifier, TypeSpecifier)] -> SemanticMonad a -> SemanticMonad a
addLocalImmutObjs loc newVars ma  =
  localScope (addVariables newVars >> ma)
  where
    addVariables = mapM_ (uncurry (insertLocalImmutObj loc))

-- | Insert mutable object (variable) in local scope.
insertLocalMutObj :: Locations -> Identifier -> TypeSpecifier -> SemanticMonad ()
insertLocalMutObj loc ident ty =
  isDefined ident
  >>= \b -> if b
  then -- | if there is throw error
  throwError $ annotateError loc $ EVarDefined ident
  else -- | If there is no variable named |ident|
  modify (\s -> s{local = M.insert ident (Mutable, ty) (local s)})

-- | Insert immutable object (variable) in local scope.
insertLocalImmutObj :: Locations -> Identifier -> TypeSpecifier -> SemanticMonad ()
insertLocalImmutObj loc ident ty =
  isDefined ident
  >>= \b -> if b
  then -- | if there is throw error
  throwError $ annotateError loc $ EVarDefined ident
  else -- | If there is no variable named |ident|
  modify (\s -> s{local = M.insert ident (Immutable, ty) (local s)})

insertConstObj :: Locations -> Identifier -> TypeSpecifier -> SemanticMonad ()
insertConstObj loc ident ty =
  isDefined ident
  >>= \b -> if b
  then -- | if there is throw error
  throwError $ annotateError loc $ EVarDefined ident
  else -- | If there is no variable named |ident|
  modify (\s -> s{consts = M.insert ident ty (consts s)})

insertGlobalTy :: Locations -> SemanTypeDef SemanticAnns -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (loc `SemAnn` GType tydef) (EUsedTypeName type_name)
 where
   type_name = identifierType tydef

insertGlobalFun :: Locations -> Identifier -> [ConstParameter] -> [Parameter] -> TypeSpecifier -> SemanticMonad ()
insertGlobalFun loc ident cps ps rettype =
  insertGlobal ident (loc `SemAnn` GFun cps ps rettype) (EUsedFunName ident)

insertGlobal :: Identifier -> SAnns (GEntry SemanticAnns) -> (Locations -> Errors Locations) -> SemanticMonad ()
insertGlobal ident entry err =
  glbWhereIsDefined ident >>=
  \case
    { Just l -> throwError (annotateError (location entry) (err l)) ;
      Nothing -> modify (\s -> s{global = M.insert ident entry (global s)}) ;
    }
  -- if b then throwError (annotateError (location entry) err)
  -- else

insertLocalVariables :: Locations -> [(Identifier , TypeSpecifier)] -> SemanticMonad ()
insertLocalVariables loc = mapM_ (\case
  (ident, ty@(DynamicSubtype _)) -> insertLocalMutObj loc ident ty;
  (ident, ty) -> insertLocalImmutObj loc ident ty)

-- | Get the type of a local (already) defined object. If it is not defined throw an error.
getLocalObjTy :: Locations -> Identifier -> SemanticMonad (AccessKind, TypeSpecifier)
getLocalObjTy loc ident =
  -- | Get local objects map and check if |ident| is a member of that map
  maybe
    -- | if |ident| is not a member throw error |ENotNamedObject|
    (throwError $ annotateError loc (ENotNamedObject ident))
    -- | if |ident| is a member return its type and access kind
    return . M.lookup ident =<< gets local

-- | Get the Type of a defined  readonlye variable. If it is not defined throw an error.
getConstTy :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getConstTy loc ident = do
    -- Check the local constants first
    constants <- gets consts
    case M.lookup ident constants of
      Just ty -> return ty
      Nothing -> -- throwError $ annotateError loc (ENotNamedObject ident)
        -- If not found, then check the globals
        catchError (getGlobalGEnTy loc ident) (\errorGlobal ->
                case semError errorGlobal of {
                  ENotNamedGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                  _  -> throwError errorGlobal;
                }
              ) >>= (\case {
                        GGlob (SConst ts)  -> return ts;
                        _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
        });

-- | Get the Type of a defined entity variable. If it is not defined throw an error.
getGlobalGEnTy :: Locations -> Identifier -> SemanticMonad (GEntry SemanticAnns)
getGlobalGEnTy loc ident =
  gets global
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedGlobal ident))) (return . ty_ann) . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy, getRHSVarTy, getGlobalVarTy ::
  Locations
  -> Identifier
  -> SemanticMonad (EnvKind, TypeSpecifier)
getLHSVarTy loc ident =
  -- | Try first local environment
  catchError (getLocalObjTy loc ident >>= (\(ak, ts) -> return (LocalKind ak, ts)))
  -- | If it is not defined there, check ro environment
    (\errorLocal ->
      case semError errorLocal of {
        ENotNamedObject _ ->
          catchError (getConstTy loc ident)
          (\errorRO ->
            case semError errorRO of {
              ENotNamedObject _ -> catchError (getGlobalGEnTy loc ident)
                (\errorGlobal ->
                  case semError errorGlobal of {
                    ENotNamedGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                    _  -> throwError errorGlobal;
                  }
                ) >>= (\case{
                        GGlob _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                        _ -> throwError $ annotateError loc (ENotNamedObject ident);
                      });
              _ -> throwError errorRO;
            }) >> throwError (annotateError loc (EObjectIsReadOnly ident))
          ;
        _  -> throwError errorLocal;
      })
getRHSVarTy loc ident =
  -- | Try first local environment
  catchError
    (getLocalObjTy loc ident >>= (\(ak, ts) -> return (LocalKind ak, ts)))
  -- | If it is not defined there, check ro environment
    (\errorLocal ->
      case semError errorLocal of {
        ENotNamedObject _ ->
          catchError (getConstTy loc ident >>= (\ts -> return (ConstKind, ts)))
          (\errorRO ->
            case semError errorRO of {
              ENotNamedObject _ -> catchError (getGlobalGEnTy loc ident)
                (\errorGlobal ->
                  case semError errorGlobal of {
                    ENotNamedGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                    _  -> throwError errorGlobal;
                  }
                ) >>= (\case{
                        GGlob _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                        _ -> throwError $ annotateError loc (ENotNamedObject ident);
                      });
              _ -> throwError errorRO;
            })
          ;
        _  -> throwError errorLocal;
      })
getGlobalVarTy loc ident =
  catchError (getGlobalGEnTy loc ident)
             (\errorGlobal ->
                case semError errorGlobal of {
                  ENotNamedGlobal errvar ->
                    if errvar == ident then
                      throwError $ annotateError loc (ENotNamedObject ident);
                    else
                      throwError errorGlobal;
                   _  -> throwError errorGlobal;
                }
              ) >>= (\case {
                        GGlob (SResource ts)  -> return (GlobalKind, ts);
                        _ -> throwError $ annotateError loc (EInvalidAccessToGlobal ident);
                      });


-- | Lookups |idenfitier| in local scope first (I assuming this is the most
-- frequent case) and then the global scope.
-- Note that it looks for every identifier and returns whatever it is.
-- It is kinda weird, because we should know what to expect.
-- Returns the type of |identifier| in case it is defined.

isDefinedIn :: Identifier -> M.Map Identifier a -> Bool
isDefinedIn = M.member

glbWhereIsDefined :: Identifier -> SemanticMonad (Maybe Locations)
-- M.Map Identifier (SAnns a) -> Maybe Locations
glbWhereIsDefined i = fmap location . M.lookup i <$> gets global

isDefined :: Identifier -> SemanticMonad Bool
isDefined ident =
  get >>= return . (\st -> isDefinedIn ident (global st)
                            || isDefinedIn ident (local st)
                            || isDefinedIn ident (consts st))

-------------
-- Type |Type| helpers!
-- | Checks if two type are the same numeric type.
sameNumTy :: TypeSpecifier -> TypeSpecifier -> Bool
sameNumTy a b = groundTyEq a b && numTy a

sameOrErr :: Locations -> TypeSpecifier -> TypeSpecifier -> SemanticMonad ()
sameOrErr loc t1 t2 =
  unless (groundTyEq t1 t2) (throwError $ annotateError loc $ EMismatch t1 t2)

unDyn :: SAST.Object SemanticAnns -> SAST.Object SemanticAnns
unDyn t = Undyn t (undynTypeAnn (getAnnotation t))

unDynExp :: SAST.Expression SemanticAnns -> SemanticMonad (SAST.Expression SemanticAnns)
unDynExp (SAST.AccessObject obj) =  return $ SAST.AccessObject (unDyn obj)
unDynExp _ = throwError $ annotateError internalErrorSeman EUnDynExpression

mustBeTy :: TypeSpecifier -> SAST.Expression SemanticAnns -> SemanticMonad (SAST.Expression SemanticAnns)
mustBeTy ty expression =
  getExpType expression >>=
  sameOrErr loc ty
  >> return expression
  where
    ann_exp = getAnnotation expression
    loc = location ann_exp

blockRetTy :: TypeSpecifier -> SAST.BlockRet SemanticAnns -> SemanticMonad ()
blockRetTy ty (BlockRet _bd (ReturnStmt _me ann)) =
  maybe
  (throwError (annotateError internalErrorSeman EUnboxingBlockRet))
  (void . sameOrErr (location ann) ty) (getResultingType (ty_ann ann))

getIntConst :: Locations -> Const -> SemanticMonad Integer
getIntConst _ (I (TInteger i _) _) = return i
getIntConst loc e     = throwError $ annotateError loc $ ENotIntConst e

-- Helper function failing if a given |TypeSpecifier| is not *simple* |simpleType|.
simpleTyorFail :: Locations -> TypeSpecifier -> SemanticMonad ()
simpleTyorFail pann ty = unless (simpleType ty) (throwError (annotateError pann (EExpectedSimple ty)))

-- Helper function failing if a given |TypeSpecifier| cannot be used to define a class field.
classFieldTyorFail :: Locations -> TypeSpecifier -> SemanticMonad ()
classFieldTyorFail pann ty = unless (classFieldType ty) (throwError (annotateError pann (EInvalidClassFieldType ty)))

-- | Function checking that a TypeSpecifier is well-defined.
-- This is not the same as defining a type, but it is similar.
-- Some types can be found in the wild, but user defined types are just check
-- they exist (they were defined preivously).
-- Note that we do not change the |TypeSpecifier| in any way, that's why this
-- function return |()|.

checkSize :: Locations -> Size -> SemanticMonad ()
checkSize loc (CAST.K s) = checkIntConstant loc USize s
checkSize loc (CAST.V ident) = getConstTy loc ident >>= sameOrErr loc USize >> return ()

checkTypeDefinition :: Locations -> TypeSpecifier -> SemanticMonad ()
checkTypeDefinition loc (DefinedType identTy) =
  -- Check that the type was defined
  void (getGlobalTy loc identTy)
  -- we assume that only well-formed types are added to globals.
checkTypeDefinition loc (Vector ty (CAST.K s)) =
  -- Doc: https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Arrays .
  -- Only arrays of simple types.
  simpleTyorFail loc ty >>
  -- Numeric contast
  checkIntConstant loc USize s >>
  --
  checkTypeDefinition loc ty
checkTypeDefinition loc (Vector ty (CAST.V ident)) =
  -- Only arrays of simple types.
  simpleTyorFail loc ty >>
  getConstTy loc ident >>=
  sameOrErr loc USize >>
  checkTypeDefinition loc ty
checkTypeDefinition loc (MsgQueue ty _)       = checkTypeDefinition loc ty
checkTypeDefinition loc (Pool ty _)           = checkTypeDefinition loc ty
-- Dynamic Subtyping
checkTypeDefinition loc (Option tyd@(DynamicSubtype _ty)) = checkTypeDefinition loc tyd
-- Regular option subtyping
checkTypeDefinition loc (Option (Option _))     = throwError $ annotateError loc EOptionNested
checkTypeDefinition loc (Option ty) = simpleTyorFail loc ty >> checkTypeDefinition loc ty
checkTypeDefinition loc (Reference _ ty)        =
  -- Unless we are referencing a reference we are good
  unless (referenceType ty) (throwError (annotateError loc (EReferenceTy ty))) >>
  checkTypeDefinition loc ty
checkTypeDefinition loc (DynamicSubtype (Option _)) = throwError $ annotateError loc EOptionNested
checkTypeDefinition loc (DynamicSubtype ty) =
  simpleTyorFail loc ty >>
  checkTypeDefinition loc ty
checkTypeDefinition loc (Location ty) =
  simpleTyorFail loc ty >>
  checkTypeDefinition loc ty
checkTypeDefinition loc (AccessPort ty) =
  case ty of
    (Allocator (Option _))  -> throwError $ annotateError loc EOptionNested
    (Allocator ty') -> simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
    (DefinedType identTy) ->
      getGlobalTy loc identTy >>=
        \case
          (Interface {}) -> return ()
          _ -> throwError $ annotateError loc $ EAccessPortNotInterface ty
    _ -> throwError $ annotateError loc $ EAccessPortNotInterface ty
checkTypeDefinition loc (SinkPort (Option _) _) = throwError $ annotateError loc EOptionNested
checkTypeDefinition loc (SinkPort ty' _) = simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
checkTypeDefinition loc (InPort (Option _) _) = throwError $ annotateError loc EOptionNested
checkTypeDefinition loc (InPort (DynamicSubtype ty') _) = simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
checkTypeDefinition loc (InPort ty' _) = simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
checkTypeDefinition loc (OutPort (Option _)) = throwError $ annotateError loc EOptionNested
checkTypeDefinition loc (OutPort (DynamicSubtype ty')) = simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
checkTypeDefinition loc (OutPort ty') = simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
checkTypeDefinition loc (Allocator (Option _)) = throwError $ annotateError loc EOptionNested
checkTypeDefinition loc (Allocator ty') = simpleTyorFail loc ty' >> checkTypeDefinition loc ty'
-- This is explicit just in case
checkTypeDefinition _ UInt8                   = return ()
checkTypeDefinition _ UInt16                  = return ()
checkTypeDefinition _ UInt32                  = return ()
checkTypeDefinition _ UInt64                  = return ()
checkTypeDefinition _ Int8                    = return ()
checkTypeDefinition _ Int16                   = return ()
checkTypeDefinition _ Int32                   = return ()
checkTypeDefinition _ Int64                   = return ()
checkTypeDefinition _ USize                   = return ()
checkTypeDefinition _ Char                    = return ()
checkTypeDefinition _ Bool                    = return ()
checkTypeDefinition _ Unit                    = return ()

-- | Type does not have Dynamic Subtyping.
-- Note to self: I still do not have the complete type system in my head.
-- It is something that keeps changing... When we have the complete type system,
-- we should write this module from scratch.
-- Note this is not a function checking that the type itself is well-formed.
-- we are just lifting a similar check on types.
-- typeHasDyn :: Locations -> TypeSpecifier -> SemanticMonad Bool
-- typeHasDyn loc ty
--  = either
      -- There is a user defined type.
--      (userDefHasDyn loc)
      -- No user defined, either it has it or not.
--      return
--  $ hasDynOrDep ty

-- userDefHasDyn :: Locations -> Identifier -> SemanticMonad Bool
-- userDefHasDyn loc ident =
--  getGlobalTy loc ident >>= stypedefHasDyn
--  where
    --
--    stypedefHasDyn :: SemanTypeDef SemanticAnns -> SemanticMonad Bool
--    stypedefHasDyn (Struct _ident fsdef _mods) =
--      or <$> mapM (typeHasDyn loc . fieldTypeSpecifier) fsdef
--    stypedefHasDyn (Enum _ident enumVs _mods) =
--      or . concat <$> mapM ( mapM (typeHasDyn loc) . assocData) enumVs
--    stypedefHasDyn (Class _cKind _ident sMembers _mods) =
--      or <$> mapM clsMemberHasDyn sMembers
    --
--    clsMemberHasDyn :: SemanClassMember SemanticAnns  -> SemanticMonad Bool
--    -- The only one struct carrying Dynamic Value are class fields.
--    clsMemberHasDyn (ClassField fldDef _anns) = typeHasDyn loc (fieldTypeSpecifier fldDef)
--    clsMemberHasDyn (ClassMethod _ident _retTy _blk _anns) = return False
--    clsMemberHasDyn (ClassProcedure _ident _params _blk _anns) = return False -- It is not a value, it takes one
--    clsMemberHasDyn (ClassViewer _ident _params _retTy _blk _ann) = return False



getExpType :: SAST.Expression SemanticAnns -> SemanticMonad TypeSpecifier
getExpType
  = maybe (throwError $ annotateError internalErrorSeman EUnboxingStmtExpr) return
  . getResultingType . ty_ann . getAnnotation

runTypeChecking
  :: ExpressionState
  -> SemanticMonad a
  -> (Either SemanticErrors a , ExpressionState)
runTypeChecking initSt = flip ST.runState initSt . runExceptT

-- | Function checking that constant expressions are correct.
-- When checking a constant we have an expected type and, optionally,
-- an explicit type attached to the constant definition.
checkConstant :: Locations -> TypeSpecifier -> Const -> SemanticMonad ()
checkConstant loc expected_type (I ti (Just type_c)) =
  -- |type_c| is correct
  checkTypeDefinition loc type_c >>
  -- | Check that the explicit type matches the expected type
  sameOrErr loc expected_type type_c >>
  -- | Check that the constant is in the range of the type
  checkIntConstant loc type_c ti
checkConstant loc expected_type (I ti Nothing) =
  -- | Check that the constant is in the range of the type
  checkIntConstant loc expected_type ti
checkConstant loc expected_type (B {}) =
  sameOrErr loc expected_type Bool
checkConstant loc expected_type (C {}) =
  sameOrErr loc expected_type Char

checkIntConstant :: Locations -> TypeSpecifier -> TInteger -> SemanticMonad ()
checkIntConstant loc tyI ti@(TInteger i _) =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I ti (Just tyI)))

buildSize :: ConstExpression SemanticAnns -> SemanticMonad Size
buildSize (KC (I ti _) _) = return (SAST.K ti)
buildSize (KV ident _) = return (SAST.V ident)
buildSize (KC c _) = throwError $ annotateError internalErrorSeman $ ENotIntConst c