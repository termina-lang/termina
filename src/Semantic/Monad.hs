-- | Semantic Monad.
-- Here lives the monad we use to handle semantic pass effects.
-- It should be something like Exceptions + State

module Semantic.Monad where

import           Data.Map                   as M

import           AST
import qualified Parsing                    as Parser (Annotation (..))
import           Semantic.Errors
import           Semantic.Types
import           Utils.TypeSpecifier

-- Monads
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Control.Monad.State.Strict as ST

type Locations = Parser.Annotation

data SemanticAnns = SemAnn
  { -- | Location on source code
    parse  :: Locations
    -- | Type after type checking
  , ty_ann :: TypeSpecifier}

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

-- | Environment required to type expression packed into just one type.
data ExpressionState
 = ExprST
 { global :: GlobalEnv
 , local  :: LocalEnv
 , ro     :: ROEnv
 }

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
getGlobalTy :: Identifier -> Locations -> SemanticMonad (TypeDef SemanticAnns)
getGlobalTy tid loc = gets global >>=
  maybe
  -- if there is no varialbe name |tid|
  (throwError $ annotateError loc (ENoTyFound tid loc))
  -- if so, return its type
  (\case {
      GType tydef -> return tydef;
        _         -> throwError $ annotateError loc (EGlobalNoType tid)
      }) . M.lookup tid

-- | From a global name get enum variations
getGlobalEnumTy :: Identifier -> Locations -> SemanticMonad [EnumVariant]
getGlobalEnumTy tid loc = getGlobalTy tid loc >>= \case
  Enum _ fs _mods _anns -> return fs
  ty                    -> throwError $ annotateError loc $ EMismatchIdNotEnum tid (fmap parse ty)


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

-- | Get the Type of a (already) defined variable. If it is not defined throw an error.
getLocalVarTy :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getLocalVarTy loc ident =
  -- | Get local variables map and check if |ident| is a member of that map
  (maybe
   (throwError $ annotateError loc (ENotNamedVar ident))
  -- ^ if |ident| is not a member throw error |ENotNamedVar|
   return)
  -- ^ if |ident| is a member return its type
  . M.lookup ident
  =<< gets local


-- | Get the Type of a defined  readonlye variable. If it is not defined throw an error.
getROVarTy :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getROVarTy loc ident =
  (M.lookup ident <$> (gets ro))
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedVar ident))) return
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

-- | Get the Type of a defined entity variable. If it is not defined throw an error.
getGlobalGEnTy :: Locations -> Identifier -> SemanticMonad (GEntry SemanticAnns)
getGlobalGEnTy loc ident =
  (gets global)
  -- | Get local variables map and check if |ident| is a member of that map
  >>= maybe (throwError (annotateError loc (ENotNamedGlobal ident))) return . M.lookup ident
  -- ^ if |ident| is not a member throw error |ENotNamedVar| or return its type

getLHSVarTy,getRHSVarTy :: Locations -> Identifier -> SemanticMonad TypeSpecifier
getLHSVarTy = getLocalVarTy
getRHSVarTy loc ident =
  -- | Try first local environment
  catchError
    (getLocalVarTy loc ident)
  -- | If it is not defined there, check ro environment
    (\case {
        ENotNamedVar _ -> getROVarTy loc ident;
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

-- lookupVar :: Locations -> Identifier -> SemanticMonad (Maybe (Either TypeSpecifier (GEntry SemanticAnns)))
-- lookupVar loc ident =
--   catchError
--     (Left <$> (getRHSVarTy loc ident))
--     (\case {
--         ENotNamedVar _ ->
--             catchError
--                 (Right <$> (getGlobalGEnTy loc ident))
--                 (\case {
--                     ENotNamedVar _ -> return Nothing;
--                     _ -> throwError $ annotateError loc ELookupVar
--                        } . semError) ;
--         _              -> throwError $ annotateError loc ELookupVar
--            } . semError)
-- This function is kinda weird, we should know if we want variables or functions,etc.
-- And |lookupVar| makes no distintion between different constructs.

-------------
-- Type |Type| helpers!
-- | Checks if two type are the same numeric type.
sameNumTy :: TypeSpecifier -> TypeSpecifier -> Bool
sameNumTy a b = sameTy a b && numTy a

-- Same Type. Should we solve type aliases?
-- [Q1]
sameTy :: TypeSpecifier -> TypeSpecifier -> Bool
sameTy = groundTyEq

sameOrErr :: Locations -> TypeSpecifier -> TypeSpecifier -> SemanticMonad TypeSpecifier
sameOrErr loc t1 t2 =
  if sameTy t1 t2
  then return t1
  else throwError $ annotateError loc $ EMismatch t1 t2

getIntConst :: Locations -> Const -> SemanticMonad Integer
getIntConst _ (I _ i) = return i
getIntConst loc e       = throwError $ annotateError loc $ ENotIntConst e
