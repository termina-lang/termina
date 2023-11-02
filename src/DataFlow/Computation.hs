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

type UDM e = ExceptT e (ST.State UDSt)

continueWith :: VarSet -> VarSet -> VarSet -> UDM e ()
continueWith oo uses defs = lift $ ST.put $ UDSt S.empty defs uses oo

get :: UDM e UDSt
get = lift ST.get

gets :: (UDSt -> b) ->  UDM e b
gets = lift . ST.gets

withState :: (UDSt -> UDSt) -> UDM e a -> UDM e a
withState f = (modify f >>)

getOnlyOnce :: UDM e VarSet
getOnlyOnce = gets usedOnlyOnce

runEncapsulated :: UDM e a -> UDM e a
runEncapsulated m =
  get >>= \st -> m >>= \res -> lift (ST.put st) >> return res

runEncaps :: [UDM e a] -> UDM e [a]
runEncaps ms =
  get >>= \st ->
  mapM (withState (const st)) ms >>= \res ->
  lift (ST.put st) >> return res

modify :: (UDSt -> UDSt) -> UDM e ()
modify = lift . ST.modify

unsafeAdd :: Identifier -> VarSet -> VarSet
unsafeAdd = S.insert

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
defVariable :: Identifier -> UDM Errors ()
defVariable ident =
  gets usedSet >>=
  \i ->
    if S.member ident i
    then
        -- Variable is used, strong assumtions on use!
        return () -- we can remove it... but maybe it is more expensive.
    else
        -- Variable |ident| is not used in the rest of the code :(
        notUsedVar ident
-----

annotateError :: Annotation -> UDM Errors a -> UDM AnnotatedErrors a
annotateError an = withExceptT (annError an)
