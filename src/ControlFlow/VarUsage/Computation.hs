-- | DSL to compute Use/Defs of Termina expressions

module ControlFlow.VarUsage.Computation (
  UDM, UDSt(..), VarMap, VarSet, OptionBoxMap,
  runEncapsWithEmptyVars, runMultipleEncapsWithEmptyVars, unionUsed, unifyState,
  unifyStates, defVariableOptionBox, defBox, defVariable, safeUseVariable,
  initializeOptionBox, moveOptionBox, safeMoveBox, allocOptionBox,
  defArgumentsProc, runComputation, emptyUDSt
) where

import ControlFlow.BasicBlocks.AST 

import ControlFlow.VarUsage.Errors
import ControlFlow.VarUsage.Types

import Control.Monad
import qualified Control.Monad.State as ST
import Control.Monad.Except as E

-- Warning: Both implmentations have a limit on the number of elements they can
-- contain. They do not fail if the limit is rechead.
-- Sets
-- Maps
import qualified Data.Map as M
import Utils.Annotations ( Location(Internal), annotateError )
import qualified Data.Set as S

-- | Map of variables to the last location where were used/moved.
type VarMap = M.Map Identifier Location
type VarSet = S.Set Identifier

-- | Map of option-box variables to their current state.
type OptionBoxMap = M.Map Identifier MVars

-- Internal state.
data UDSt = UDSt { 
    -- | Map with the current state of the option-box variables
    optionBoxesMap :: OptionBoxMap,
    -- | Set of used variables
    usedVarSet :: VarSet,
    -- | Map of moved boxes. It maps each box variable to the location where it
    -- was moved.
    movedBoxes :: VarMap
  }

emptyUDSt :: UDSt
emptyUDSt
  = UDSt M.empty S.empty M.empty

-- | Monad to compute the use/defs of variables.
type UDM e = ExceptT e (ST.State UDSt)

putOptionBoxesMap :: OptionBoxMap -> UDM e ()
putOptionBoxesMap = ST.modify . (\s st -> st {optionBoxesMap = s})

putUsedVarSet :: VarSet -> UDM e ()
putUsedVarSet = ST.modify . (\s st -> st {usedVarSet = s})

putMovedBoxMap :: VarMap -> UDM e ()
putMovedBoxMap =ST.modify . (\s st -> st {movedBoxes = s})

withState :: (UDSt -> UDSt) -> UDM e a -> UDM e a
withState f = (ST.modify f >>)

-- Encapsulation mechanisms.
-- Useful to run computations in isolated environments.
runEncapsWithEmptyVars :: UDM e a -> UDM e a
runEncapsWithEmptyVars m = do
  st <- ST.get
  res <- withState (const $ st {usedVarSet = S.empty}) m
  ST.put st
  return res

-- Run computations encapusulates with same first state.
runMultipleEncapsWithEmptyVars :: [UDM e a] -> UDM e [a]
runMultipleEncapsWithEmptyVars ms = do
  st <- ST.get
  res <- mapM (withState (const $ st {usedVarSet = S.empty})) ms
  ST.put st
  return res

unifyState :: (OptionBoxMap, VarMap, VarSet) -> UDM e ()
unifyState (optionBoxes, boxes, regular)
  = ST.modify 
    (\st ->
    -- M.union is left biased. Meaning that it takes priority over the other map
    -- when key collision. It is what we want tho.
     st { 
        optionBoxesMap = M.union optionBoxes (optionBoxesMap st),
        movedBoxes = M.union boxes (movedBoxes st),
        usedVarSet = S.union regular (usedVarSet st)
      })

unifyStates :: UDSt -> UDSt -> UDM e UDSt
unifyStates prev curr
  = return $ 
  UDSt {
    optionBoxesMap = M.union (optionBoxesMap curr) (optionBoxesMap prev),
    movedBoxes = M.union (movedBoxes curr) (movedBoxes prev),
    usedVarSet = S.union (usedVarSet curr) (usedVarSet prev)
  }

unsafeAddMap :: Identifier -> Location -> VarMap -> VarMap
unsafeAddMap = M.insert

unsafeAddSet :: Identifier -> VarSet -> VarSet
unsafeAddSet = S.insert

unionUsed :: OptionBoxMap -> VarSet -> UDM e ()
unionUsed optionBoxes regular =
  ST.modify (\st -> st {
    optionBoxesMap = M.union optionBoxes (optionBoxesMap st),
    usedVarSet = S.union regular (usedVarSet st)})

removeUsed :: Identifier -> UDSt -> UDSt
removeUsed s st = st {usedVarSet = S.delete s (usedVarSet st)}

----------------------------------------
-- This function checks we have not reached the limit of the data structure.
safeUseVariable :: Identifier -> UDM VarUsageError ()
-- Add Variable to use set
safeUseVariable ident 
  = do
    usedVarSet' <- ST.gets usedVarSet
    unless (S.size usedVarSet' < maxBound) (throwError $ annotateError Internal ESetMaxBound)
    putUsedVarSet $ unsafeAddSet ident usedVarSet'

safeMoveBox :: Identifier -> Location -> UDM VarUsageError ()
safeMoveBox ident loc
  = ST.gets movedBoxes
  >>= \boxSet ->
    case M.lookup ident boxSet of
      Just prevLoc -> throwError $ annotateError prevLoc (EBoxMovedTwice ident loc)
      Nothing -> do
        unless (M.size boxSet < maxBound) (throwError $ annotateError Internal ESetMaxBound)
        putMovedBoxMap (unsafeAddMap ident loc boxSet)
        safeUseVariable ident

safeUpdateOptionBox :: Identifier -> MVars -> UDM VarUsageError ()
safeUpdateOptionBox ident mv
  = do
    ooMap <- ST.gets optionBoxesMap
    unless (M.size ooMap < maxBound) (throwError $ annotateError Internal EMapMaxBound)
    putOptionBoxesMap $
      case mv of
        -- We can delete it, because previous stage guarantees no variable
        -- shadowing.
        Defined _ -> M.delete ident ooMap
        -- Everything else just inserts.
        _ -> M.insert ident mv ooMap

-- | Box variable manipulation
defBox :: Identifier -> Location -> UDM VarUsageError ()
defBox ident loc
  = ST.gets movedBoxes
  >>= \boxSet ->
    if M.member ident boxSet
    then do
      putMovedBoxMap $ M.delete ident boxSet
    else
      throwError $ annotateError loc (EBoxNotMoved ident)

moveOptionBox :: Identifier -> Location -> UDM VarUsageError ()
moveOptionBox ident loc
  = do
    optionBoxMap <- ST.gets optionBoxesMap
    case M.lookup ident optionBoxMap of
      Just (Allocated _) -> safeUpdateOptionBox ident (Moved loc) >> safeUseVariable ident
      Just (Moved prevLoc) -> throwError $ annotateError loc (EOptionBoxMovedTwice ident prevLoc)
      Just (Defined _) -> throwError $ annotateError Internal EVarRedefinition;
      Nothing -> safeUpdateOptionBox ident (Moved loc) >> safeUseVariable ident

initializeOptionBox :: Identifier -> Location -> UDM VarUsageError ()
initializeOptionBox ident loc
  = do
    optionBoxMap <- ST.gets optionBoxesMap
    case M.lookup ident optionBoxMap of
      Just (Moved prevLoc) -> throwError $ annotateError loc (EMovedWithoutAlloc ident prevLoc)
      -- Define in the future? This case shouldn't happen
      Just (Defined _) -> throwError $ annotateError Internal EVarRedefinition;
      _ -> return ()
      
allocOptionBox :: Identifier -> Location -> UDM VarUsageError ()
allocOptionBox ident loc
  =
  maybe
    (throwError $ annotateError loc (EAllocNotMoved ident))
    (\case{
        -- We use it in the future
        Moved _ -> safeUpdateOptionBox ident (Allocated loc) >> safeUseVariable ident;
        -- Re-allocation
        Allocated prevLoc -> throwError $ annotateError prevLoc (EAllocTwice ident loc);
        -- Define in the future? This case shouldn't happen
        Defined _ -> throwError $ annotateError Internal EVarRedefinition;
      }) . M.lookup ident =<< ST.gets optionBoxesMap

defVariableOptionBox :: Identifier -> Location -> UDM VarUsageError ()
defVariableOptionBox ident loc =
  maybe
    (throwError $ annotateError loc (ENotUsed ident))
    (\case{
        -- Allocated after defined.
        Allocated _ -> safeUpdateOptionBox ident (Defined loc);
        -- Skipped allocation
        Moved prevLoc -> throwError $ annotateError loc (EMovedWithoutAlloc ident prevLoc);
        -- Defined;Defined not allowed,
        Defined _ -> throwError $ annotateError Internal EDefinedTwice 
        }) . M.lookup ident =<< ST.gets optionBoxesMap

-- If we define a variable that was not used, then error.
defVariable :: Identifier -> Location -> UDM VarUsageError ()
defVariable ident loc =
  ST.gets usedVarSet >>=
  \i ->
    case head ident of
      -- If the argument starts with an underscore, then the parameter must be ignored and not used.
      '_' -> when (S.member ident i) (throwError $ annotateError loc (EUsedIgnoredParameter ident))
      _ -> if S.member ident i
           then ST.modify (removeUsed ident)
           else throwError $ annotateError loc (ENotUsed ident)

-- Procedures can receive /box/ variables as arguments.
-- Box variables have a special Use, through free or stuff.
-- So we need to analyze each argument to decide if it is normal variable or
-- box.
defArgumentsProc :: Parameter -> Location -> UDM VarUsageError ()
defArgumentsProc ps loc
  = (case paramType ps of
        TBoxSubtype _ -> flip defBox loc
        _ -> flip defVariable loc)
    (paramIdentifier ps)

----------------------------------------
-- Run computation and get its result.
runComputation :: UDM e a -> (Either e a , UDSt )
runComputation = flip ST.runState emptyUDSt  . runExceptT
----------------------------------------
