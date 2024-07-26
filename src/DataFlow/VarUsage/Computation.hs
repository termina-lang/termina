-- | DSL to compute Use/Defs of Termina expressions

module DataFlow.VarUsage.Computation where

import AST.Core (Identifier,Parameter(..),TypeSpecifier(..))
import Parser.Parsing (Annotation)

import DataFlow.VarUsage.Errors
import DataFlow.VarUsage.Types

import qualified Control.Monad.State as ST
import Control.Monad.Except as E

-- Warning: Both implmentations have a limit on the number of elements they can
-- contain. They do not fail if the limit is rechead.
-- Sets
import qualified Data.Set as S
-- Maps
import qualified Data.Map.Strict as M
import Utils.Annotations

type VarSet = S.Set Identifier

getVars :: VarSet -> [Identifier]
getVars = S.toList

-- Variables could be defined, allocated or used.
-- Normal variables go from |Defined| to used |Used|


-- We already know that each used variable is defined from previous pass.
-- We just check that every defined variable is used plus the special ones goes
-- throw the special process.

type OOVarSt = M.Map Identifier MVars

-- Internal state.
data UDSt
  = UDSt
  -- A set of used variables
    { usedSet :: VarSet
  -- A map indicating the state of special variables.
    , usedOption :: OOVarSt
    , usedDyn :: VarSet
    }

emptyUDSt :: UDSt
emptyUDSt
  = UDSt S.empty M.empty S.empty

emptyButUsed :: UDSt -> UDSt
emptyButUsed st = st{usedOption = M.empty, usedDyn = S.empty}

type UDM e = ExceptT e (ST.State UDSt)

----------------------------------------
-- Manipulating the state inside monad |UDM|
get :: UDM e UDSt
get = lift ST.get

put :: UDSt -> UDM e ()
put = lift . ST.put

putOOMap :: OOVarSt -> UDM e ()
putOOMap = modify . (\s st -> st{usedOption = s})

putUSet,putDynSet :: VarSet -> UDM e ()
putUSet = modify . (\s st -> st{usedSet = s})
putDynSet = modify . (\s st -> st{usedDyn = s})

gets :: (UDSt -> b) ->  UDM e b
gets = lift . ST.gets

modify :: (UDSt -> UDSt) -> UDM e ()
modify = lift . ST.modify

withState :: (UDSt -> UDSt) -> UDM e a -> UDM e a
withState f = (modify f >>)

-- Encapsulation mechanisms.
-- Useful to run computations in isolated environments.

runEncapsulated :: UDM e a -> UDM e a
runEncapsulated m =
  get >>= \st -> m >>= \res -> lift (ST.put st) >> return res

runEncapsEOO :: [UDM e a] -> UDM e [a]
runEncapsEOO ms =
  get >>= \st ->
  mapM (withState emptyButUsed) ms >>=
-- mapM (withState (const emptyUDSt)) ms >>=

-- mapM (withState (const emptyUDSt)) ms >>=

-- mapM (withState (const emptyUDSt)) ms >>=

-- mapM (withState (const emptyUDSt)) ms >>=
  
-- mapM (withState (const emptyUDSt)) ms >>=

-- mapM (withState (const emptyUDSt)) ms >>=

-- mapM (withState (const emptyUDSt)) ms >>=

-- mapM (withState (const emptyUDSt)) ms >>=
  (put st >>) . return

-- Run computations encapusulates with same first state.
runEncaps :: [UDM e a] -> UDM e [a]
runEncaps ms = do
  st <- get
  res <- mapM (withState (const st)) ms
  put st
  return res
----------------------------------------

emptyOO :: UDM e ()
emptyOO = get >>= \st -> put (st{usedOption = M.empty})

continueWith,unionS :: OOVarSt -> VarSet -> VarSet -> UDM e ()
continueWith oo uses dyns
  = put $ UDSt uses oo dyns
unionS oo uses dyns
  = get
  >>= \st
  -> put
    -- M.union is left biased. Meaning that it takes priority over the other map
    -- when key collision. It is what we want tho.
     st{ usedOption = M.union oo (usedOption st)
       , usedSet = S.union uses (usedSet st)
       , usedDyn = S.union dyns (usedDyn st)
       }

getOnlyOnce :: UDM e OOVarSt
getOnlyOnce = gets usedOption

unsafeAdd :: Identifier -> VarSet -> VarSet
unsafeAdd = S.insert

unionUsed :: VarSet -> UDM e ()
unionUsed vset =
  modify (\st -> st{usedSet = S.union vset (usedSet st)})

removeUsedOO,removeUsed :: Identifier -> UDSt -> UDSt
removeUsedOO s st = st{usedOption = M.delete s (usedOption st)}
removeUsed s st = st{usedSet = S.delete s (usedSet st)}

-- safeAdd :: Identifier -> VarSet -> UDM Errors VarSet
-- safeAdd ident vset =
--   unless (M.size vset < maxBound) (throwError SetMaxBound)
--   >> return (unsafeAdd ident vset)

----------------------------------------
-- This function checks we have not reached the limit of the data structure.
safeAddUse :: Identifier -> UDM Error ()
-- Add Variable to use set
safeAddUse ident
  = do
    usedSet <- gets usedSet
    unless (S.size usedSet < maxBound) (throwError SetMaxBound)
    putUSet $ unsafeAdd ident usedSet

safeAddUseOnlyOnce :: Identifier -> MVars -> UDM Error ()
safeAddUseOnlyOnce ident mv
  = do
    ooMap <- gets usedOption
    unless (M.size ooMap < maxBound) (throwError MapMaxBound)
    putOOMap $
      case mv of
        -- We can delete it, because previous stage guarantees no variable
        -- shadowing.
        Defined -> M.delete ident ooMap
        -- Everything else just inserts.
        _ -> M.insert ident mv ooMap

-- DynVar manipulation
defDynVar, useDynVar :: Identifier -> UDM Error ()
defDynVar ident
  = gets usedDyn
  >>= \dynSet ->
    if S.member ident dynSet
    then do
      putDynSet $ S.delete ident dynSet
    else
      throwError (NotUsedOO ident)
useDynVar ident
  = gets usedDyn
  >>= \dynSet ->
  if S.member ident dynSet
  then
    throwError (UsingTwice ident)
  else
    unless (S.size dynSet < maxBound) (throwError SetMaxBound)
    >> putDynSet (unsafeAdd ident dynSet)
----------------------------------------

addUseOnlyOnce :: Identifier -> UDM Error ()
addUseOnlyOnce ident
  = do
    ooMap <- gets usedOption
    when (M.member ident ooMap) (throwError (UsingTwice ident))
    safeAddUseOnlyOnce ident Used

allocOO :: Identifier -> UDM Error ()
allocOO ident
  =
  maybe
    (throwError (AllocNotUsed ident))
    (\case{
        -- We use it in the future
        Used -> safeAddUseOnlyOnce ident Allocated;
        -- Re-allocation
        Allocated -> throwError (AllocTwice ident);
        -- Define in the future? This case shouldn't happen
        Defined -> throwError (AllocRedef ident);
        }) . M.lookup ident =<< gets usedOption

defVariableOO :: Identifier -> UDM Error ()
defVariableOO ident =
  maybe
    (throwError (NotUsed ident))
    (\case{
        -- Allocated after defined.
        Allocated ->  safeAddUseOnlyOnce ident Defined;
        -- Skipped allocation
        Used -> throwError (DefinedNotAlloc ident);
        -- Defined;Defined not allowed,
        Defined -> throwError (DefinedTwice ident);
        }) . M.lookup ident =<< gets usedOption

-- If we define a variable that was not used, then error.
defVariable :: Identifier -> UDM Error ()
defVariable ident =
  gets usedSet >>=
  \i ->
    case head ident of
      -- If the argument starts with an underscore, then the parameter must be ignored and not used.
      '_' -> when (S.member ident i) $ throwError (UsedIgnoredVariable ident)
      _ ->
        if S.member ident i
        then
            -- Variable is used, strong assumtions on use!
            -- return () -- we can remove it... but maybe it is more expensive.
            modify (removeUsed ident)
        else
            -- Variable |ident| is not used in the rest of the code :(
            throwError (NotUsed ident)

-- Procedures can receive /dyn/ variables as arguments.
-- Dyn variables have a special Use, through free or stuff.
-- So we need to analyze each argument to decide if it is normal variable or
-- dyn.
defArgumentsProc :: Parameter -> UDM Error ()
defArgumentsProc ps
  = (case paramTypeSpecifier ps of
        DynamicSubtype _ -> defDynVar
        _ -> defVariable)
    (paramIdentifier ps)

----------------------------------------
-- Run computation and get its result.
runComputation :: UDM e a -> (Either e a , UDSt )
runComputation = flip ST.runState emptyUDSt  . runExceptT
----------------------------------------
