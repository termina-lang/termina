-- | DSL to compute Use/Defs of Termina expressions

module DataFlow.Computation where

import AST.Core (Identifier)
import Parser.Parsing (Annotation)

import DataFlow.Errors

import qualified Control.Monad.State as ST
import Control.Monad.Except as E
import Control.Monad
import Control.Monad.Trans

import qualified Data.Set as S
import qualified Data.Map.Strict as M

type VarSet = S.Set Identifier

data UDSt
  = UDSt { notUsed :: VarSet
         , defV :: VarSet
         , usedSet :: VarSet
         , usedOnlyOnce :: VarSet
         }

emptyUDSt :: UDSt
emptyUDSt = UDSt S.empty S.empty S.empty S.empty

union :: UDSt -> UDSt -> UDSt
union (UDSt n1 d1 us1 uoo1) (UDSt n2 d2 us2 uoo2)
 = UDSt (S.union n1 n2) (S.union d1 d2) (S.union us1 us2) (S.union uoo1 uoo2)

type UDM e = ExceptT e (ST.State UDSt)

emptyOO :: UDM e ()
emptyOO = get >>= \st -> put (st{usedOnlyOnce = S.empty})

unionToState :: UDSt -> UDM e ()
unionToState st = get >>= put . union st

runComputation :: UDM e a -> (Either e a , UDSt )
runComputation = flip ST.runState emptyUDSt  . runExceptT


continueWith,unionS :: VarSet -> VarSet -> VarSet -> UDM e ()
continueWith oo uses defs = lift $ ST.put $ UDSt S.empty defs uses oo
unionS oo uses defs =
  get >>= \st -> put st{ usedOnlyOnce = S.union oo (usedOnlyOnce st)
                       , usedSet = S.union uses (usedSet st)
                       , defV = S.union defs (defV st)}

get :: UDM e UDSt
get = lift ST.get

put :: UDSt -> UDM e ()
put = lift . ST.put

gets :: (UDSt -> b) ->  UDM e b
gets = lift . ST.gets

withState :: (UDSt -> UDSt) -> UDM e a -> UDM e a
withState f = (modify f >>)

getOnlyOnce :: UDM e VarSet
getOnlyOnce = gets usedOnlyOnce

runEncapsulated :: UDM e a -> UDM e a
runEncapsulated m =
  get >>= \st -> m >>= \res -> lift (ST.put st) >> return res

-- This should be a little bit more efficient

runEncapsEOO :: [UDM e a] -> UDM e [a]
runEncapsEOO ms =
  get >>= \st ->
  mapM (withState (const emptyUDSt)) ms >>=
  (put st >>) . return

runEncaps :: [UDM e a] -> UDM e [a]
runEncaps ms =
  get >>= \st ->
  mapM (withState (const st)) ms >>= \res ->
  put st >> return res

modify :: (UDSt -> UDSt) -> UDM e ()
modify = lift . ST.modify

unsafeAdd :: Identifier -> VarSet -> VarSet
unsafeAdd = S.insert

unionUsed :: VarSet -> UDM e ()
unionUsed vset =
  modify (\st -> st{usedSet = S.union vset (usedSet st)})

removeUsedOO,removeUsed :: Identifier -> UDSt -> UDSt
removeUsedOO s st = st{usedOnlyOnce = S.delete s (usedOnlyOnce st)}
removeUsed s st = st{usedSet = S.delete s (usedSet st)}

safeAdd :: Identifier -> VarSet -> UDM Errors VarSet
safeAdd ident vset =
  unless (S.size vset < maxBound) (throwError SetMaxBound)
  >> return (unsafeAdd ident vset)

safeAddUse,safeAddUseOnlyOnce,safeAddDef :: Identifier -> UDM Errors ()
safeAddUse ident =
  gets usedSet >>= safeAdd ident >>= modify . (\s uS -> uS{usedSet = s})
safeAddDef ident =
  gets defV >>= safeAdd ident >>= modify . (\s uS -> uS{defV = s})
safeAddUseOnlyOnce  ident =
  gets usedOnlyOnce >>= safeAdd ident >>= modify . (\s uS -> uS{usedOnlyOnce = s})

addUseOnlyOnce :: Identifier -> UDM Errors ()
addUseOnlyOnce ident = get >>= \udst
  -> when (S.member ident (usedSet udst)) (throwError (UsingTwice ident))
  >> safeAddUseOnlyOnce ident

-- This function eventually produces an error.
-- We collect all unused variables and produce an Error.
notUsedVar :: Identifier -> UDM Errors ()
notUsedVar = throwError . NotUsed

-- If we define a variable that was not used, then error.
defVariable,defVariableOO :: Identifier -> UDM Errors ()
defVariable ident =
  gets usedSet >>=
  \i ->
    if S.member ident i
    then
        -- Variable is used, strong assumtions on use!
        -- return () -- we can remove it... but maybe it is more expensive.
        modify (removeUsed ident)
    else
        -- Variable |ident| is not used in the rest of the code :(
        notUsedVar ident
defVariableOO ident =
  gets usedOnlyOnce >>=
  \i ->
    if S.member ident i
    then
        modify (removeUsedOO ident)
    else
        notUsedVar ident
-----

annotateError :: Annotation -> UDM Errors a -> UDM AnnotatedErrors a
annotateError an = withExceptT (annError an)
