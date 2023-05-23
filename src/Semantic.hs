{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

-- | Semantic Analysis Module i.e. Type checking

module Semantic where
import           AST

import           Data.List                  (foldl', sort, sortOn)

-- import Control.Monad.State as ST
import           Data.Map                   as M

import           Control.Arrow

import           Control.Monad.Except       (MonadError (..))
-- import Control.Monad.Error.Class (withError)
import           Control.Monad.Identity
import           Control.Monad.State.Strict as ST

type SemAnn = ()

-- How we interpret types
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
  deriving Show

type SemTask = SemTask' SemAnn

-- type AnnTaskEnv = Map Identifier (SemTask SemAnn)
----------------------------------------

data SemFunction' a = SFunction
  { funArgs :: [Parameter a]
  , funRet  :: TypeSpecifier a
  , funAnns :: [ a ]
  }
  deriving Show

type SemFunction = SemFunction' SemAnn

-- type AnnFunEnv = Map Identifier (SemFunction SemAnn)
----------------------------------------

data SemHandler' a = SemHandler
  { handlerArgs :: [Parameter a]
  , handlerRet  :: TypeSpecifier a
  , handlerAnns :: [ a ]
  }
  deriving Show

type SemHandler = SemHandler' SemAnn

-- type AnnHandlerEnv = Map Identifier (SemHandler SemAnn)
----------------------------------------

data SemGlobal' a
  = SVolatile (TypeSpecifier a) [ a ]
  | SStatic (TypeSpecifier a) [ a ]
  | SShared (TypeSpecifier a) [ a ]
  | SConst (TypeSpecifier a) [ a ]
  deriving Show

type SemGlobal = SemGlobal' SemAnn

getTySemGlobal :: SemGlobal -> Type
getTySemGlobal (SVolatile ty _)  = ty
getTySemGlobal (SStatic ty _)    = ty
getTySemGlobal (SShared ty _) = ty
getTySemGlobal (SConst ty _)     = ty

--

data GEntry' a
  = GFun (SemFunction' a)
  | GTask (SemTask' a)
  | GHand (SemHandler' a)
  | GGlob (SemGlobal' a)
  | GType (TypeDef a)
  deriving Show

type GEntry = GEntry' SemAnn

----------------------------------------
-- Error Handling
----------------------------------------
data Errors
  -- | Expected /similar/ types?
  = EMismatch Type Type
  | EMismatchIdNotEnum Identifier (TypeDef SemAnn)
  | ECasteable Type Type
  -- | Expected Numeric Types
  | ENumTs [Type]
  -- | Reference of a global type?
  | EReferenceGlobal Identifier
  -- | Not Variable found
  | ENotVar
  | ENotNamedVar Identifier
  -- No shadow binding
  | EVarDefined Identifier
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
  | EPMMissingOption0 -- Missing None
  | EPMMissingOption1 -- Missing Some
  | EPMMoreOptions -- More cases??
  | EPMMoreOptionsVariables -- More Variable in enum Some(a,b,c,d..)
  -- | Global Object but not a type
  | EGlobalNoType Identifier
  -- | Vectors with dynamic length
  | EVectorConst (Expression SemAnn)
  -- | Not an integer const
  | ENotIntConst (Const SemAnn)
  -- | PM Enum errors
  | EMCMissingEnum Identifier
  | EMCMoreArgs [Identifier]
  | EMCMissingArgs [TypeSpecifier SemAnn]
  | EMCEmpty
  -- | ForLoop
  | EBadRange
  deriving Show

withError :: MonadError e m => (e -> e) -> m a -> m a
withError = flip catchError . (throwError .)
----------------------------------------

----------------------------------------
-- Global env: global definitions variables, tasks, functions, .., and types
type GlobalEnv = Map Identifier GEntry
-- Local env: variables to their type
type LocalEnv = Map Identifier Type
type ROEnv = Map Identifier Type

data StateAnalyzer
 = StateAnalyzer
 { global :: GlobalEnv
 , local  :: LocalEnv
 , ro :: ROEnv
 }

-- | Shortcut: Our Semantic monad is just State + Errors
class (MonadState StateAnalyzer m, MonadError Errors m) => SemMonad m where

-- | Get global definition
getNameTy :: SemMonad m => Identifier -> m (TypeDef SemAnn)
getNameTy tid = gets global >>=
  maybe (throwError (ENoTyFound tid)) (\case {
                                          GType tydef -> return tydef;
                                          _ -> throwError (EGlobalNoType tid)
                                             }) . M.lookup tid

-- | From a global name get enum variations
getEnumTy :: SemMonad m => Identifier -> m [EnumVariant SemAnn]
getEnumTy tid = getNameTy tid >>= \case
  Enum _ fs _a -> return fs
  ty           -> throwError (EMismatchIdNotEnum tid ty)

-- | Execute computation |comp| in a temporal state |tempState| wihtout
-- modifying current state.
withInState :: MonadState s m => s -> m a -> m a
withInState tempState comp = localScope (put tempState >> comp)

-- Execute comp but bracktracking the state
-- Useful for blocks semantic analysis
localScope :: MonadState s m => m a -> m a
localScope comp = do
  prevst <- get
  res <- comp
  put prevst
  return res


-- | Adding new local varialbes
addTempVars :: SemMonad m => [(Identifier, Type)] -> m a -> m a
addTempVars newVars ma =
  localScope (addVariables newVars >> ma)
  where
    addVariables = mapM_ (uncurry insertVar)

-- Add variables to the local environment.
-- I assume shadow biding. If we do not want that we should be more strict.
addVars :: MonadState StateAnalyzer m => [(Identifier, Type)] -> m ()
addVars newVars = modify (\s -> s{local = Data.List.foldl' (\m (k,v) -> M.insert k v m) (local s) newVars})

-- | Insert varialbe in local scope
insertVar :: SemMonad m => Identifier -> Type -> m ()
insertVar id ty = lookupVar id
  >>= maybe
  -- | If there is no variable named |id|
  (modify (\s -> s{local = M.insert id ty (local s)}))
  -- | if there is throw error
  (const (throwError (EVarDefined id)))

-- | Get the Type of a (already) defined variable. If it is not defined throw an error.
getDefinedVarTy :: SemMonad m => Identifier -> m Type
getDefinedVarTy id =
  (M.lookup id <$> (gets local))
  -- | Get local variables map and check if |id| is a member of that map
  >>= maybe (throwError (ENotNamedVar id)) return
  -- | if |id| is not a member throw error |ENotNamedVar| or return its type


-- | Lookups |idenfitier| in local scope first (I assuming this is the most
-- frequent case) and then the global scope.
-- Returns the type of |identifier| in case it is defined.
lookupVar :: SemMonad m => Identifier -> m (Maybe (Either Type GEntry))
lookupVar id =
 gets (\s -> (local s, global s)) >>= \(locals, globals) ->
  case M.lookup id locals of
    Just ty -> return (Just (Left ty))
    Nothing -> maybe (return Nothing) (return . Just . Right) (M.lookup id globals)

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

getIntConst :: MonadError Errors m => Const SemAnn -> m Int
getIntConst (I _ i) = return i
getIntConst e       = throwError (ENotIntConst e)


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

--
casteableTys :: Type -> TypeSpecifier SemAnn -> Bool
casteableTys UInt8 UInt16  = True
casteableTys UInt16 UInt32 = True
casteableTys UInt32 UInt64 = True
casteableTys Int8 Int16    = True
casteableTys Int16 Int32   = True
casteableTys Int32 Int64   = True
-- Last option being the same.
-- This is a trivial casting :muscle:
casteableTys a b           = sameTy a b

expressionType :: SemMonad m => Expression SemAnn -> m Type
expressionType (Variable a) =
  -- | Check if it is a local variable. And its typing rule
  {-
\[
\begin{array}{c}
\inferce[evarG]
{ (x, \tau) \in G}
% ----------------------------------------
{G, L, RO \vdash x : \tau}\\
\inferce[evarL]
{ (x, \tau) \in L}
% ----------------------------------------
{G, L, RO \vdash x : \tau}\\
\inferce[evarRO]
{ (x, \tau) \in RO}
% ----------------------------------------
{G, L, RO \vdash x : \tau}
\end{array}
\]
-}
  gets local >>= \locals ->
  case M.lookup a locals of
    Just t -> return t
    Nothing -> gets global >>= \glbs ->
      case M.lookup a glbs of
        Just (GGlob gvars) -> return (getTySemGlobal gvars)
        _                  -> throwError ENotVar
expressionType (Constant c) =
  -- | Constants
  {-
\[
\begin{array}{c}
\inference[econstTrue]
% ----------------------------------------
{ \_ \vdash true : \mathsf{Bool}}\\
\inference[econstFalse]
% ----------------------------------------
{ \_ \vdash false : \mathsf{Bool}}\\
\inference[econstIntX]
{n \in \mathsf{IntX}}
% ----------------------------------------
{\_ \vdash n : \mathsf{IntX}}\\
\inference[econstC]
% ----------------------------------------
{ \_ \vdash c : \mathsf{Char}}
\end{array}
\]
-}
  case c of
    B b -> return Bool
    I tyI i ->
      -- DONE Q8
      if numTy tyI then return tyI else throwError (ENumTs [tyI])
    C c -> return Char

expressionType (Casting e nty) =
  -- | Casting Expressions.
  {-
\[
\inference[eCast]
{E \vdash e : \tau \\ \tau \subseteq \theta}
% ----------------------------------------
{ E \vdash e as \theta : \theta}
\]
-}
  expressionType e >>= \ety ->
  if casteableTys ety nty -- ety \subseteq nty
  then return nty else throwError (ECasteable ety nty)
expressionType (BinOp op le re) = do
  -- | Binary operation typings
  {-
\[
\inference[eBOps]{
  E \vdash e_1 : \theta_1 \\
  E \vdash e_2 : \theta_2 \\
  Type(\oplus)(\theta_1,\theta_2) = \tau \\
}
% ----------------------------------------
{ E \vdash e_1 \oplus e_2 : \tau}
\]
-}
  tyle <- expressionType le
  tyre <- expressionType re
  typeOfOps op tyle tyre
expressionType (ReferenceExpression e) =
  -- | Reference Expression
  -- TODO [Q15]
  Reference <$> expressionType e
-- Function call?
expressionType (FunctionExpression fun_name args) =
  -- | Function Expression.  A tradicional function call
  {-
\[
\inference[eFuncCall]{
  (f, \texttt{Func}(\mathsf{Args}, \tau)) \in G \\
  (a_1, \tau_1) \in G \cup L \cup RO \\
  \ldots \\
  (a_n, \tau_n) \in G \cup L \cup RO \\
  (\tau_1, \ldots, \tau_n) \in Args \\
}
% ----------------------------------------
{ G,L,RO \vdash f(a_1, \ldots, a_n) : \tau}
\]
-}
   gets global >>= \glbs ->
    case M.lookup fun_name glbs of
      Just (GFun (SFunction pTy retTy _anns)) ->
        paramTy pTy args >> return retTy
      Just ge -> throwError (ENotFoundFun fun_name ge)
      Nothing ->  throwError (ENotAFun fun_name)
expressionType (FieldValuesAssignmentsExpression id_ty fs) =
  -- | Field Type
  {-
\[
\begin{array}{c}
\inference[eStruct]
{
  (\mathsf{RcTy}, \texttt{Struct}(\mathsf{Fields})) \in G \\
  (r_i, \tau_i) \in \mathsf{Fields}, i \in [1, \ldots, n] \\
  \mathsf{Fields} \setminus \{r_1, \ldots, r_n \} = \emptyset \\
  G , L , RO \vdash e_i : \tau_i,  i \in [1, \ldots, n]
}
% ----------------------------------------
{ G, L , RO \vdash  \mathsf{RcTy} \{ r_1 = e_1 , \ldots , r_n = e_n \} : \mathsf{RcTy}}\\
\inference[eUnion]
{
  (\mathsf{UnionTy}, \texttt{Union}(\mathsf{Fields})) \in G \\
  (r_i, \tau_i) \in \mathsf{Fields}, i \in [1, \ldots, n] \\
  \mathsf{Fields} \setminus \{r_1, \ldots, r_n \} = \emptyset \\
  G , L , RO \vdash e_i : \tau_i,  i \in [1, \ldots, n]
}
% ----------------------------------------
{ G, L , RO \vdash  \mathsf{UnionTy} \{ r_1 = e_1 , \ldots , r_n = e_n \} : \mathsf{UnionTy}}\\
\end{array}
\]
-}
  getNameTy id_ty >>= \case {
   Struct _ ty_fs _ann ->
       checkFieldValues ty_fs fs >> return (DefinedType id_ty) ;
   Union _ ty_fs _ann ->
       checkFieldValues ty_fs fs >> return (DefinedType id_ty) ;
   _ -> throwError (ETyNotStruct id_ty);
  }
expressionType (VectorIndexExpression vec_exp index_exp) =
  -- | VectorIndex.
  {-
\[
\inference[eVectorIndex]
{ G, L, RO \vdash v : \mathsf{Vector}(\tau) \\
  G, L, RO \vdash e : \theta \\
  \mathsf{Index}(\theta)
}
% ----------------------------------------
{ G, L, RO \vdash v[e] : \tau}
\]
-}
  expressionType vec_exp >>= \case {
    Vector ty_elems _vexp ->
        expressionType index_exp >>= \ity ->
        if numTy ity then return ty_elems
        else throwError (ENumTs [ity])
    ;
    ty -> throwError (EVector ty);
                                   }
-- IDEA Q4
expressionType (VectorInitExpression iexp lexp@(Constant c)) = do
-- | Vector Initialization
{-
\inference[eVecInit]
{
G, L, RO \vdash e : \tau \\
c \in Ty \\ TODO Q16
}
% ----------------------------------------
{G , L , RO \vdash [ e ; c : Ty ] : \mathsf{Vector}(\tau, c)}
-}
  init_ty <- expressionType iexp
  len_ty <- expressionType lexp
  if numTy len_ty
  then Vector init_ty . K <$> getIntConst c
  else throwError (ENumTs [len_ty])
expressionType (VectorInitExpression _ lexp) = throwError (EVectorConst lexp)
-- DONE [Q5]
-- TODO [Q17]
expressionType (MatchExpression e cs) =
  -- | Pattern Matching.
  {-
\[
\begin{array}{c}
\inference[OptionMatch]
{ G, L, RO |- e : Option(\theta) \\
  G, L, RO |- e_2 : \tau \\
  G, L \cup \{(x, \theta)\}, RO |- e_1 : \tau \\
}
% ----------------------------------------
{ G,L,RO |- match(e){case Some(x) => e_1; case None => e_2 } : \tau}\\
\inference[GenMatch]
{ G,L,RO |- e : \mathsf{Enum}(\overline{cs}) \\
  G,L \cup \{(\overline{c_i} \cdot \overline{\tau_i})\},RO |- e_i : \tau (C_i, \overline{\tau_i}) \in \overline{cs}\\
  \overline{cs} \setminus \{ C_1, \ldots, C_n \} = \emptyset
}
{ G,L,RO |- match(e){case C_1(\overline{c_1}) => e_1; ... ; case C_n(\overline{c_n}) => e_n } : \tau}
\end{array}
\]
-}
  expressionType e >>= \case {
  -- Base Types PM
   Option pty     -> pmOption pty cs;
   ;
  -- User defined PM only Enums
   DefinedType id -> getEnumTy id >>= pmEnums cs
   }

-- Zipping list of same length
zipSameLength ::  ([b] -> e) -> ([a] -> e) -> (a -> b -> c) -> [a] -> [b] -> Either e [c]
zipSameLength ea eb f as bs = zipSameLength' ea eb f as bs []
  where
    -- Tail recursive version
    zipSameLength' :: ([b] -> e) -> ([a] -> e) -> (a -> b -> c) -> [a] -> [b] -> [c] -> Either e [c]
    zipSameLength' _ _ _ [] [] acc = Right acc
    zipSameLength' ea eb f (a : as) (b : bs) acc = zipSameLength' ea eb f as bs (f a b : acc)
    zipSameLength' ea _ _ [] bs _ = Left (ea bs)
    zipSameLength' _ eb _ as [] _ = Left (eb as)
--

-- Not assuming anything. Ordering matchs just in case.
pmEnums :: SemMonad m => [MatchCase SemAnn] -> [EnumVariant SemAnn] -> m Type
pmEnums mc evs = join $ checkSame <$> (zipWithM pmEnumsS sorted_mc sorted_evs)
  where
    checkSame :: SemMonad m => [Type] -> m Type
    checkSame []     = throwError EMCEmpty
    checkSame (t:ts) = foldM (=?=) t ts
    sorted_mc :: [MatchCase SemAnn]
    sorted_mc = sortOn matchIdentifier mc
    sorted_evs :: [EnumVariant SemAnn]
    sorted_evs = sortOn variantIdentifier evs
    pmEnumsS :: SemMonad m => MatchCase SemAnn -> EnumVariant SemAnn -> m Type
    pmEnumsS mc ev =
      if matchIdentifier mc == variantIdentifier ev
      then
        either throwError
        (\scope -> addTempVars
          scope
          (retblockType (matchBody mc)))
        (zipSameLength EMCMissingArgs EMCMoreArgs (,) (matchBVars mc) (assocData ev))
      else throwError (EMCMissingEnum (variantIdentifier ev))


pmOption :: SemMonad m => Type -> [MatchCase SemAnn] -> m Type
pmOption ty [cs1, cs2] =
  if matchIdentifier cs1 == "None" then pmOption' ty cs1 cs2
  else pmOption' ty cs2 cs1
  where
    noneConditions cs = matchIdentifier cs == "None" && Prelude.null(matchBVars cs)
    someConditions cs = matchIdentifier cs == "Some" && length(matchBVars cs) == 1
    -- I am sure this could be improved
    pmOption' :: SemMonad m => Type -> MatchCase SemAnn -> MatchCase SemAnn -> m Type
    pmOption' ty csnone cssome =
     if noneConditions csnone
     then if someConditions cssome
          then do
            tyNone <- retblockType (matchBody csnone)
            tySome <- case matchBVars cssome of
              [v] -> addTempVars [(v,ty)] (retblockType (matchBody cssome))
              _   -> throwError EPMMoreOptionsVariables
            tyNone =?= tySome
          else throwError EPMMissingOption1
     else throwError EPMMissingOption0
pmOption _ []        = throwError EPMMissingOption0
pmOption _ [_]       = throwError EPMMissingOption1
pmOption _ _ = throwError EPMMoreOptions

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

retblockType :: SemMonad m => BlockRet SemAnn -> m Type
retblockType (BlockRet bbody (mret, _anns)) =
  -- Blocks should have their own scope.
  blockType bbody
  >> maybe (return Unit) expressionType mret

blockType :: SemMonad m => Block SemAnn -> m ()
blockType = mapM_ statementTySimple

-- | Type checking statements. We should do something about Break
statementTySimple :: SemMonad m => Statement SemAnn -> m ()
-- Declaration semantic analysis
statementTySimple (Declaration lhs_id lhs_type Nothing _anns) =
  addVars[(lhs_id, lhs_type)]
statementTySimple (Declaration lhs_id lhs_type (Just expr)  _anns) =
  ((lhs_type =?=) =<<) (expressionType expr) >>
  addVars [(lhs_id,lhs_type)]
statementTySimple (AssignmentStmt lhs_id rhs_expr _anns) =
  void $ join $ (=?=) <$> getDefinedVarTy lhs_id <*> expressionType rhs_expr
statementTySimple (IfElseStmt cond_expr tt_branch elifs otherwise_branch _anns) =
  let (cs, bds) = unzip (Prelude.map (\c -> (elseIfCond c, elseIfBody c)) elifs) in
  mapM_ (join . ((Bool =?=) <$>) . expressionType) (cond_expr : cs) >>
  mapM_ (localScope . blockType) ([tt_branch] ++ bds ++ [otherwise_branch])
-- Here we could implement some abstract interpretation analysis
statementTySimple (ForLoopStmt it_id from_expr to_expr body_stmt _ann) = do
  from_ty <- expressionType from_expr
  to_ty <- expressionType to_expr
  if sameNumTy from_ty to_ty
  then addTempVars [(it_id,from_ty)] (blockType body_stmt)
  else throwError EBadRange
statementTySimple (SingleExpStmt expr _anns) = void $ expressionType expr
-- Break is not handled right now, but may do something. Stack of breaking points.
statementTySimple (Break _anns) = return () -- See Q11

----------------------------------------
-- Programs Semantic Analyzer
-- For now all are kinda the same thing but eventually they should not :shrug:
----------------------------------------
-- Task Semantic Analyzer
taskSemAnalyzer :: SemMonad m
  => [Parameter SemAnn] -> TypeSpecifier SemAnn
  -> BlockRet SemAnn -> [SemAnn] -> m Type
taskSemAnalyzer taskPs taskRetType taskBody _anns =
  addTempVars -- | Add
    (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) taskPs)
    -- | list of variables
    ((taskRetType =?=) =<< (retblockType taskBody))
    -- | analyze the body and check the returining type

-- Function Semantic Analyzer
functionSemAnalyzer :: SemMonad m
 => [Parameter SemAnn] -> (Maybe (TypeSpecifier SemAnn)) -> (BlockRet SemAnn) -> [ SemAnn ] -> m Type
functionSemAnalyzer funPS mbfunRetTy funBody _anns =
  addTempVars -- | Add scoped variables
  (fmap (\ p -> (paramIdentifier p , paramTypeSpecifier p)) funPS)
  $ -- within the scope + parameters, check body type
  case mbfunRetTy of
    -- [Q12] Do we accept procedures?
    Nothing    -> _VoidTy
    Just retTy -> (retTy =?=) =<< (retblockType funBody)

-- Handler Semantic Analyzer (Template)
handlerSemAnalyzer :: SemMonad m =>
  [Parameter SemAnn] -> (TypeSpecifier SemAnn) -> (BlockRet SemAnn) -> [ SemAnn ] -> m Type
handlerSemAnalyzer hanPS hanType hanBody _anns =
  addTempVars (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) hanPS)
  $
  (hanType =?=) =<< (retblockType hanBody)

-- Keeping only type information
globalCheck :: SemMonad m => Global SemAnn -> m SemGlobal
globalCheck (Volatile ident ty addr anns) = return (SVolatile ty anns)
-- [Q13]
globalCheck (Static ident ty (Just (Constant _Address)) anns) = _returnStatic
globalCheck (Static _ _ _ _) = _Static
--
globalCheck (Shared ident ty mexpr anns) =
  case mexpr of
    Just expr -> do
      eTy  <- expressionType expr
      _ <- ty =?= eTy
      return (SShared ty anns)
    Nothing -> _SharedNoExpr
-- TODO [Q14]
globalCheck (Const ident ty expr anns) = do
      eTy  <- expressionType expr
      _ <- ty =?= eTy
      return (SConst ty anns)
