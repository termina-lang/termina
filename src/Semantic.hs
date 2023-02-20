{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
-- | Semantic Analysis. Type checking

module Semantic where

import           AST

import           Data.List                  (sortOn)

-- import Control.Monad.State as ST
import           Data.Map                   as M

import           Control.Monad.Except       (MonadError (..))
-- import Control.Monad.Error.Class (withError)
import           Control.Monad.Identity
import           Control.Monad.State.Strict as ST

type SemAnn = ()

-- Variables can have two types
-- - Simple types, uint,int, etc

type Type = TypeSpecifier SemAnn

-- Know we have another category of ints. Those known at compiler time.

----------------------------------------
-- Task type information
data SemTask' a = STask
  { taskArgs :: [Parameter a]
  , taskRet  :: TypeSpecifier a
  , taskAnns :: [ a ]
  }

type SemTask = SemTask' SemAnn

-- type AnnTaskEnv = Map Identifier (SemTask SemAnn)
----------------------------------------

data SemFunction' a = SFunction
  { funArgs :: [Parameter a]
  , funRet  :: TypeSpecifier a
  , funAnns :: [ a ]
  }

type SemFunction = SemFunction' SemAnn

-- type AnnFunEnv = Map Identifier (SemFunction SemAnn)
----------------------------------------

data SemHandler' a = SemHandler
  { handlerArgs :: [Parameter a]
  , handlerRet  :: TypeSpecifier a
  , handlerAnns :: [ a ]
  }

type SemHandler = SemHandler' SemAnn

-- type AnnHandlerEnv = Map Identifier (SemHandler SemAnn)
----------------------------------------

data SemGlobal' a
  = SVolatile (TypeSpecifier a) [ a ]
  | SStatic (TypeSpecifier a) [ a ]
  | SProtected (TypeSpecifier a) [ a ]
  | SConst (TypeSpecifier a) [ a ]

type SemGlobal = SemGlobal' SemAnn

getTySemGlobal :: SemGlobal -> Type
getTySemGlobal (SVolatile ty _)  = ty
getTySemGlobal (SStatic ty _)    = ty
getTySemGlobal (SProtected ty _) = ty
getTySemGlobal (SConst ty _)     = ty

--

data GEntry' a
  = GFun (SemFunction' a)
  | GTask (SemTask' a)
  | GHand (SemHandler' a)
  | GGlob (SemGlobal' a)
  | GType (TypeDef a)

type GEntry = GEntry' SemAnn

----------------------------------------
data Errors
  -- | Expected /similar/ types?
  = EMismatch Type Type
  | ECasteable Type Type
  -- | Expected Numeric Types
  | ENumTs [Type]
  -- | Reference of a global type?
  | EReferenceGlobal Identifier
  -- | Not Variable found
  | ENotVar
  -- | Not Function found
  | ENotFoundFun Identifier GEntry
  -- | Type Id not found
  | ENoTyFound Identifier
  -- | Not a function
  | ENotAFun Identifier
  -- | Parameter and argument type mismatch
  | EParam Type Type
  -- | Wrong number of params
  | EFunParams
  -- | Type Identifier is not Union/Struct
  | ETyNotStruct Identifier
  -- | Record missing fields
  | EFieldMissing [Identifier]
  -- | Record extra fields
  | EFieldExtra [Identifier]
  -- | Expecting a Vecotor got
  | EVector Type
  -- | Pattern Matching Missing cases
  | EPMMissingOption0
  | EPMMissingOption1
  -- | Global Object but not a type
  | EGlobalNoType Identifier
  -- | Vectors with dynamic length
  | EVectorConst (Expression SemAnn)
  -- | Not an integer const
  | ENotIntConst (Const SemAnn)

withError :: MonadError e m => (e -> e) -> m a -> m a
withError = flip catchError . (throwError .)

----------------------------------------
-- Global env: global definitions variables, tasks, functions, .., and types
type GlobalEnv = Map Identifier GEntry
-- Local env: variables to their type
type LocalEnv = Map Identifier Type

data StateAnalyzer
 = StateAnalyzer
 { global :: GlobalEnv
 , local  :: LocalEnv
 }

class (MonadState StateAnalyzer m, MonadError Errors m) => SemMonad m where

getNameTy :: SemMonad m => Identifier -> m (TypeDef SemAnn)
getNameTy tid = gets global >>=
  maybe (throwError (ENoTyFound tid)) (\case {
                                          GType tydef -> return tydef;
                                          _ -> throwError (EGlobalNoType tid)
                                             }) . M.lookup tid

-- | Boxed types represent overloaded types.
-- For example, literal ints, what type should we give them? It dependens on the
-- context, so we leave them unspecified.
data BoxedType' a
  = NoBoxed (TypeSpecifier a)
  | Numeric Int

type BoxedType = BoxedType' SemAnn

-------------
-- First approach: Load everything, type-check one at a time.

-- | Checks if two type are the same numeric type.
sameNumTy :: Type -> Type -> Bool
sameNumTy a b = sameTy a b && numTy a

-- Same Type. Should we solve type aliases?
-- [Q1]
sameTy :: Type -> Type -> Bool
sameTy = groundTyEq
-- To solve type synonyms we need an environment, but if they have the same name
-- they are the same ty :shrug:

(=?=) :: MonadError Errors m => Type -> Type -> m Type
t1 =?= t2 = if sameTy t1 t2 then return t1 else throwError (EMismatch t1 t2)

getIntConst :: MonadError Errors m => Const a -> m Int
getIntConst (I _ i) = return i
getIntConst e = throwError (ENotIntConst e)


boolTy :: Type -> Bool
boolTy Bool = True
boolTy _    = False

numTy :: Type -> Bool
numTy UInt8  = True
numTy UInt16 = True
numTy UInt32 = True
numTy UInt64 = True
numTy Int8   = True
numTy Int16  = True
numTy Int32  = True
numTy Int64  = True
numTy _      = False

-- Lots of questions here.
typeOfOps :: MonadError Errors m => Op -> Type -> Type -> m Type
-- Alg ops Same numeric type
typeOfOps Multiplication tyl tyr =
  if sameNumTy tyl tyr
  then return tyl
  else throwError (EMismatch tyl tyr)
typeOfOps Division tyl tyr = if sameNumTy tyl tyr then return tyl else throwError (EMismatch tyl tyr)
typeOfOps Addition tyl tyr = if sameNumTy tyl tyr then return tyl else throwError (EMismatch tyl tyr)
typeOfOps Substraction tyl tyr = if sameNumTy tyl tyr then return tyl else throwError (EMismatch tyl tyr)
-- shifts both numeric but may not be the same
-- Q2
typeOfOps BitwiseLeftShift tyl tyr = if numTy tyl && numTy tyr then return tyl else throwError (ENumTs [tyl,tyr])
typeOfOps BitwiseRightShift tyl tyr = if numTy tyl && numTy tyr then return tyl else throwError (ENumTs [tyl,tyr])
-- >, =>, <, <= : some numeric type.
typeOfOps RelationalLT tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
typeOfOps RelationalLTE tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
typeOfOps RelationalGT tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
typeOfOps RelationalGTE tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
-- Equiality: TODO I think we said structural equality, but not sure.
typeOfOps RelationalEqual tyl tyr = if sameTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
typeOfOps RelationalNotEqual tyl tyr = if sameTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
-- Bitwise. I guess this is like C so nums?
typeOfOps BitwiseAnd tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
typeOfOps BitwiseOr tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
typeOfOps BitwiseXor tyl tyr = if sameNumTy tyl tyr then return Bool else throwError (EMismatch tyl tyr)
-- Logical And/Or bool
typeOfOps LogicalAnd tyl tyr = if boolTy tyl && boolTy tyr then return tyl else throwError (EMismatch tyl tyr)
typeOfOps LogicalOr tyl tyr = if boolTy tyl && boolTy tyr then return tyl else throwError (EMismatch tyl tyr)


checkParamTy :: SemMonad m => Type -> Expression SemAnn -> m ()
checkParamTy pTy aTy = expressionType aTy >>= \texp ->
    withError (const (EParam pTy texp)) (void (pTy =?= texp))

paramTy :: SemMonad m => [Parameter SemAnn] -> [Expression SemAnn] -> m ()
paramTy [] [] = return ()
paramTy (p : ps) (a : as) = checkParamTy (paramTypeSpecifier p) a >> paramTy ps as
paramTy _ _ = throwError EFunParams

casteableTys :: Type -> TypeSpecifier SemAnn -> Bool
casteableTys _ _ = _casteable

expressionType :: SemMonad m => Expression SemAnn -> m Type
expressionType (Variable a) =
  -- Check if it is a local variable
  gets local >>= \locals ->
  case M.lookup a locals of
    Just t -> return t
    Nothing -> gets global >>= \glbs ->
      case M.lookup a glbs of
        Just (GGlob gvars) -> return (getTySemGlobal gvars)
        _                  -> throwError ENotVar
expressionType (Constant c) =
  case c of
    B b -> return Bool
    I tyI i ->
      -- Q8
      if numTy tyI then return tyI else throwError (ENumTs [tyI])
    C c -> return Char
    S str -> _string_type_q7

expressionType (Casting e nty) = expressionType e >>= \ety ->
  if casteableTys ety nty then return nty else throwError (ECasteable ety nty)
expressionType (BinOp op le re) = do
  tyle <- expressionType le
  tyre <- expressionType re
  typeOfOps op tyle tyre
expressionType (ReferenceExpression e) = Reference <$> expressionType e
-- Function call?
expressionType (FunctionExpression fun_name args) =
   gets global >>= \glbs ->
    case M.lookup fun_name gbls of
      Just (GFun (SFunction pTy retTy _anns)) ->
        paramTy pTy args >> return retTy
      Just ge -> throwError (ENotFoundFun funName ge)
      Nothing ->  throwError (ENotAFun funName)
expressionType (FieldValuesAssignmentsExpression id_ty fs) =
  getNameTy id_ty >>= \case {
   Struct _ ty_fs _ann ->
       checkFieldValues ty_fs fs >> return (DefinedType id_ty) ;
   Union _ ty_fs _ann ->
       checkFieldValues ty_fs fs >> return (DefinedType id_ty) ;
   _ -> throwError (ETyNotStruct id_ty);
  }
expressionType (VectorIndexExpression vec_exp index_exp) =
  expressionType vec_exp >>= \case {
    Vector ty_elems _vexp ->
        expressionType index_exp >>= \ity ->
        if numTy ity then return ty_elems
        else throwError (ENumTs [ity])
    ;
    ty -> throwError (EVector ty);
                                   }
-- Q4
expressionType (VectorInitExpression iexp lexp@(Constant c)) = do
  init_ty <- expressionType iexp
  len_ty <- expressionType lexp
  if numTy len_ty
  then Vector init_ty . K <$> getIntConst c
-- getIntConst c >>= return . Vector init_ty . K
  else throwError (ENumTs [len_ty])
expressionType (VectorInitExpression _ lexp) = throwError (EVectorConst lexp)
-- [Q5]
expressionType (MatchExpression e cs) =
  expressionType e >>= \case {
  -- Base Types PM
   Option pty     -> pmOption pty cs;
   ;
  -- User defined PM
   DefinedType id -> _Q5;
   }

pmOption :: SemMonad m => Type -> [MatchCase SemAnn] -> m Type
pmOption ty [cn, cs] = _OptionPatternMatching_Q6
pmOption _ []        = throwError EPMMissingOption0
pmOption _ [_]       = throwError EPMMissingOption1

checkFieldValue :: SemMonad m => FieldDefinition SemAnn -> FieldValueAssignment SemAnn  -> m ()
checkFieldValue (FieldDefinition fid fty _) (FieldValueAssignment faid faexp) =
  if fid == faid
  then expressionType faexp >>= void . (fty =?=)
  else throwError (EFieldMissing [fid])

checkFieldValues :: SemMonad m => [FieldDefinition SemAnn] -> [FieldValueAssignment SemAnn] -> m ()
checkFieldValues fds fas = checkSortedFields sorted_fds sorted_fas
  where
    sorted_fds = sortOn fieldIdentifier fds
    sorted_fas = sortOn fieldAssigIdentifier fas
    checkSortedFields [] [] = return ()
    checkSortedFields [] es = throwError (EFieldExtra (fmap fieldAssigIdentifier es))
    checkSortedFields ms [] = throwError (EFieldMissing (fmap fieldIdentifier ms))
    checkSortedFields (d:ds) (a:as) =
      checkFieldValue d a >> checkSortedFields ds as

-- semTask
--   :: Identifier -> [Parameter a] -> TypeSpecifier a -> BlockRet a
--   -> State (GlobalEnv a) (Maybe (SemTask a))
-- semTask ident args tyret block = _


-- createEnv :: AnnotatedProgram a -> State (GlobalEnv a) (AnnotatedProgram a)
-- createEnv _ = _
