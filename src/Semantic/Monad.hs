{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | Semantic Monad.
-- Here lives the monad we use to handle semantic pass effects.
-- It should be something like Exceptions + State

module Semantic.Monad where

import           Data.Map                   as M

-- AST Info
import           AST
import           SemanAST                   as SAST
import Annotations
-- import           Utils.AST
import           Utils.AST (groundTyEq)
import           Utils.SemanAST

import qualified Parsing                    as Parser (Annotation (..))
import           Semantic.Errors
import           Semantic.Types
import           Utils.TypeSpecifier

-- Monads
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Control.Monad.State.Strict as ST

type Locations = Parser.Annotation

internalErrorSeman :: Locations
internalErrorSeman = Parser.Internal

data SAnns a = SemAnn
  { -- | Location on source code
    location :: Locations
    -- | Type after type checking
  , ty_ann   :: a}

instance Annotated SAnns where
  getAnnotation = ty_ann

-- | Semantic elements
-- we have three different semantic elements:
data SemanticElems
  = ETy TypeSpecifier -- ^ Expressions with their types
  | STy -- ^ Statements with no types
  | GTy (GEntry Locations) -- ^ Global elements

getTySpec :: SemanticElems -> Maybe TypeSpecifier
getTySpec (ETy ty) = Just ty
getTySpec _        = Nothing

getGEntry :: SemanticElems -> Maybe (GEntry Locations)
getGEntry (GTy a) = Just a
getGEntry _       = Nothing

buildExpAnn :: Locations -> TypeSpecifier -> SAnns SemanticElems
buildExpAnn loc ty = SemAnn loc (ETy ty)

buildGlobalAnn :: Locations -> SemGlobal -> SAnns SemanticElems
buildGlobalAnn loc = SemAnn loc . GTy . GGlob

buildGlobal :: Locations -> GEntry Locations -> SAnns SemanticElems
buildGlobal loc = SemAnn loc . GTy

buildGlobalTy :: Locations -> SemanTypeDef Locations -> SAnns SemanticElems
buildGlobalTy loc = SemAnn loc . GTy . GType

buildStmtAnn :: Locations -> SAnns SemanticElems
buildStmtAnn = flip SemAnn STy

-- | Expression Semantic Annotations
type SemanticAnns = SAnns SemanticElems

forgetSemAnn :: SAnns a -> Locations
forgetSemAnn = location

-- -- | Global Definitions
-- type GlobalsSemantic = SAnns (GEntry ())

-- globType :: Locations -> SemGlobal -> GlobalsSemantic
-- globType loc = SemAnn loc . GGlob

type SemanticErrors = AnnotatedErrors Locations

----------------------------------------
-- | Global env
-- It has global definitions
type GlobalEnv = Map Identifier (GEntry SemanticAnns)
-- | Local env
-- variables to their type
type LocalEnv = Map Identifier TypeSpecifier
-- | Read Only Environment.
type ROEnv = Map Identifier TypeSpecifier
-- This may seem a bad decision, but each envornment represent something
-- different.
-- TODO We can use empty types to disable envirnoments and make Haskell do part
-- of our work.

-- | Environment required to type expression packed into just one type.
data ExpressionState
 = ExprST
 { global :: GlobalEnv
 , local  :: LocalEnv
 , ro     :: ROEnv
 }

initialExpressionSt :: ExpressionState
initialExpressionSt = ExprST empty empty empty

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
  (throwError $ annotateError loc (ENoTyFound tid loc))
  -- if so, return its type
  (\case {
      GType tydef -> return tydef;
        _         -> throwError $ annotateError loc (EGlobalNoType tid)
      }) . M.lookup tid

-- | From a global name get enum variations
getGlobalEnumTy :: Locations -> Identifier -> SemanticMonad [EnumVariant]
getGlobalEnumTy loc tid  = getGlobalTy loc tid  >>= \case
  Enum _ fs _mods -> return fs
  ty              -> throwError $ annotateError loc $ EMismatchIdNotEnum tid (fmap (fmap forgetSemAnn) ty)

getFunctionTy :: Locations -> Identifier -> SemanticMonad ([Parameter],TypeSpecifier)
getFunctionTy loc iden =
  catchError (getGlobalGEnTy loc iden ) (\_ -> throwError $ annotateError loc (ENotAFun iden))
  >>= \case
  GFun args retty -> return (args, retty)
  ge -> throwError $ annotateError loc (ENotFoundFun iden (fmap forgetSemAnn ge))



-- | Adding new *local* variables.
-- Computation is executed in a new local environment.
addTempVars :: Locations -> [(Identifier, TypeSpecifier)] -> SemanticMonad a -> SemanticMonad a
addTempVars loc newVars ma  =
  localScope (addVariables newVars >> ma)
  where
    addVariables = mapM_ (uncurry (insertLocalVar loc))

-- | Insert varialbe in local scope.

insertLocalVar :: Locations -> Identifier -> TypeSpecifier -> SemanticMonad ()
insertLocalVar loc ident ty =
  isDefined ident
  >>= \b -> if b
  then -- | if there is throw error
  throwError $ annotateError loc $ EVarDefined ident
  else -- | If there is no variable named |ident|
  modify (\s -> s{local = M.insert ident ty (local s)})

insertGlobalTy :: Locations -> SemanTypeDef SemanticAnns -> SemanticMonad ()
insertGlobalTy loc tydef =
  insertGlobal type_name (GType tydef) (annotateError loc $ EUsedTypeName type_name)
 where
   type_name = identifierType tydef

insertGlobalFun :: Locations -> Identifier -> [Parameter] -> TypeSpecifier -> SemanticMonad ()
insertGlobalFun loc ident ps rettype =
  insertGlobal ident (GFun ps rettype) (annotateError loc $ EUsedFunName ident)

insertGlobal :: Identifier -> GEntry SemanticAnns -> SemanticErrors -> SemanticMonad ()
insertGlobal ident entry err =
  isDefined ident >>= \b ->
  if b then throwError err
  else modify (\s -> s{global = M.insert ident entry (global s)})

insertLocalVariables :: Locations -> [(Identifier , TypeSpecifier)] -> SemanticMonad ()
insertLocalVariables loc = mapM_ (uncurry (insertLocalVar loc))

-- | Get the Type of a local (already) defined variable. If it is not defined throw an error.
getLocalVarTy :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getLocalVarTy loc ident =
  -- | Get local variables map and check if |ident| is a member of that map
  maybe
   (throwError $ annotateError loc (ENotNamedVar ident))
  -- ^ if |ident| is not a member throw error |ENotNamedVar|
   return
  -- ^ if |ident| is a member return its type
  . M.lookup ident
  =<< gets local

-- | Get the Type of a global defined variable. If it is not defined throw an error.
getGlobalVarTyLhs :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getGlobalVarTyLhs loc ident =
  maybe (throwError $ annotateError loc (ENotNamedGlobal ident))
  (\case{
      GGlob glb ->
      if lhsGlobal glb then return (getTySemGlobal glb)
      else throwError (annotateError loc (EGlobalNotLHS ident))
      ;
      _ -> throwError (annotateError loc (EGlobalOtherType ident));
        })
  . M.lookup ident =<< gets global
  where
    -- TODO [Q20]
    lhsGlobal (SVolatile {}) = True
    lhsGlobal (SStatic {})   = True
    lhsGlobal (SShared {})   = True
    lhsGlobal (SConst {})    = False

-- | Get the Type of a defined  readonlye variable. If it is not defined throw an error.
getROVarTy :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getROVarTy loc ident =
  (M.lookup ident <$> gets ro)
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedVar ident))) return
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

-- | Get the Type of a defined entity variable. If it is not defined throw an error.
getGlobalGEnTy :: Locations -> Identifier -> SemanticMonad (GEntry SemanticAnns)
getGlobalGEnTy loc ident =
  gets global
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedGlobal ident))) return . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy,getRHSVarTy:: Locations -> Identifier -> SemanticMonad TypeSpecifier
getLHSVarTy loc ident =
  catchError
    (getLocalVarTy loc ident)
    (\case{
        AnnError (ENotNamedVar _) _loc -> getGlobalVarTyLhs loc ident;
        l                              -> throwError l;
          })
getRHSVarTy loc ident =
  -- | Try first local environment
  catchError
    (getLocalVarTy loc ident)
  -- | If it is not defined there, check ro environment
    (\case {
        ENotNamedVar _ ->
        catchError (getROVarTy loc ident)
        (\case {
            ENotNamedVar _ -> getGlobalGEnTy loc ident >>=
              (\case{
                  GGlob sG -> return (getTySemGlobal sG);
                  _ -> throwError $ annotateError loc (ENotNamedVar ident);
                  });
            _              -> throwError $ annotateError loc ERHSCatch;
               } . semError)
        ;
        _              -> throwError $ annotateError loc ERHSCatch
           } . semError)

-- | Lookups |idenfitier| in local scope first (I assuming this is the most
-- frequent case) and then the global scope.
-- Note that it looks for every identifier and returns whatever it is.
-- It is kinda weird, because we should know what to expect.
-- Returns the type of |identifier| in case it is defined.

isDefinedIn :: Identifier -> M.Map Identifier a -> Bool
isDefinedIn = M.member

isDefined :: Identifier -> SemanticMonad Bool
isDefined ident = get >>= return . (\st ->
                             (isDefinedIn ident (global st))
                             || (isDefinedIn ident (local st))
                             || (isDefinedIn ident (ro st))
                          )

-------------
-- Type |Type| helpers!
-- | Checks if two type are the same numeric type.
sameNumTy :: TypeSpecifier -> TypeSpecifier -> Bool
sameNumTy a b = groundTyEq a b && numTy a

sameOrErr :: Locations -> TypeSpecifier -> TypeSpecifier -> SemanticMonad TypeSpecifier
sameOrErr loc t1 t2 =
  if groundTyEq t1 t2
  then return t1
  else throwError $ annotateError loc $ EMismatch t1 t2

mustByTy :: TypeSpecifier -> SAST.Expression SemanticAnns -> SemanticMonad (SAST.Expression SemanticAnns)
mustByTy ty exp = getExpType exp >>= sameOrErr loc ty >> return exp
  where
    ann_exp = getAnnotation exp
    loc = location ann_exp

blockRetTy :: TypeSpecifier -> SAST.BlockRet SemanticAnns -> SemanticMonad ()
blockRetTy ty (BlockRet bd (ReturnStmt me ann)) =
  maybe (throwError $ annotateError internalErrorSeman EUnboxingBlockRet)(void . sameOrErr (location ann) ty) (getTySpec $ ty_ann ann)

getIntConst :: Locations -> Const -> SemanticMonad Integer
getIntConst _ (I _ i) = return i
getIntConst loc e     = throwError $ annotateError loc $ ENotIntConst e

-- Helper function failing if a given |TypeSpecifier| is not *simple* |simpleType|.
simpleTyorFail :: Locations -> TypeSpecifier -> SemanticMonad ()
simpleTyorFail pann ty = unless (simpleType ty) (throwError (annotateError pann (EExpectedSimple ty)))

-- | Function checking that a TypeSpecifier is well-defined.
-- This is not the same as defining a type, but it is similar.
-- Some types can be found in the wild, but user defined types are just check
-- they exist (they were defined preivously).
-- Note that we do not change the |TypeSpecifier| in any way, that's why this
-- function return |()|.

checkTypeDefinition :: Locations -> TypeSpecifier -> SemanticMonad ()
checkTypeDefinition loc (DefinedType identTy) =
  -- Check that the type was defined
  void (getGlobalTy loc identTy)
  -- we assume that only well-formed types are added to globals.
checkTypeDefinition loc (Vector ty c)         =
  -- Doc: https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Arrays .
  -- Only arrays of simple types.
  simpleTyorFail loc ty >>
  -- Numeric contast
  unless (numConstExpression c) (throwError (annotateError loc (EVectorConst c))) >>
  --
  checkTypeDefinition loc ty
checkTypeDefinition loc (MsgQueue ty _)       = checkTypeDefinition loc ty
checkTypeDefinition loc (Pool ty _)           = checkTypeDefinition loc ty
-- Dynamic Subtyping
checkTypeDefinition loc (Option tyd@(DynamicSubtype ty)) = checkTypeDefinition loc tyd
checkTypeDefinition loc (Option ty) = throwError $ annotateError loc $ EOptionDyn ty
checkTypeDefinition loc (Reference ty)        =
  -- Unless we are referencing a reference we are good
  unless (referenceType ty) (throwError (annotateError loc (EReferenceTy ty))) >>
  checkTypeDefinition loc ty
checkTypeDefinition loc (DynamicSubtype ty)   =
  simpleTyorFail loc ty >>
  checkTypeDefinition loc ty
-- This is explicit just in case
checkTypeDefinition _ UInt8                   = return ()
checkTypeDefinition _ UInt16                  = return ()
checkTypeDefinition _ UInt32                  = return ()
checkTypeDefinition _ UInt64                  = return ()
checkTypeDefinition _ Int8                    = return ()
checkTypeDefinition _ Int16                   = return ()
checkTypeDefinition _ Int32                   = return ()
checkTypeDefinition _ Int64                   = return ()
checkTypeDefinition _ Char                    = return ()
checkTypeDefinition _ Bool                    = return ()
checkTypeDefinition _ Unit                    = return ()

getExpType :: SAST.Expression SemanticAnns -> SemanticMonad TypeSpecifier
getExpType
  = maybe (throwError $ annotateError internalErrorSeman EUnboxingStmtExpr) return
  . getTySpec . ty_ann . getAnnotation

runTypeChecking
  :: ExpressionState
  -> SemanticMonad a
  -> (Either SemanticErrors a , ExpressionState)
runTypeChecking initSt = flip ST.runState initSt . runExceptT
