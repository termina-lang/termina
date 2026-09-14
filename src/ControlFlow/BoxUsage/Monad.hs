-- | State of the box linearity check: which boxes have been moved, which
-- option-boxes are allocated, and which names have been mentioned.

module ControlFlow.BoxUsage.Monad (
  BoxUsageM, BoxUsageSt(..), VarMap, VarSet, OptionBoxMap,
  runEncapsWithEmptyVars, runMultipleEncapsWithEmptyVars, unionUsed, unifyState,
  unifyStates, defVariableOptionBox, defBox, safeUseVariable,
  initializeOptionBox, moveOptionBox, safeMoveBox, allocOptionBox,
  defArgumentsProc, runBoxUsage, emptyBoxUsageSt
) where

import ControlFlow.BasicBlocks.AST 

import ControlFlow.BoxUsage.Errors
import ControlFlow.BoxUsage.Types

import Control.Monad
import qualified Control.Monad.State as ST
import Control.Monad.Except as E

-- Warning: Both implmentations have a limit on the number of elements they can
-- contain. They do not fail if the limit is rechead.
-- Sets
-- Maps
import qualified Data.Map.Strict as M
import Utils.Annotations ( Location(Internal), annotateError )
import qualified Data.Set as S

-- | Map of variables to the last location where were used/moved.
type VarMap = M.Map Identifier Location
type VarSet = S.Set Identifier

-- | Map of option-box variables to their current state.
type OptionBoxMap = M.Map Identifier MVars

-- Internal state.
data BoxUsageSt = BoxUsageSt { 
    -- | Map with the current state of the option-box variables
    optionBoxesMap :: OptionBoxMap,
    -- | Set of used variables
    usedVarSet :: VarSet,
    -- | Map of moved boxes. It maps each box variable to the location where it
    -- was moved.
    movedBoxes :: VarMap
  }

emptyBoxUsageSt :: BoxUsageSt
emptyBoxUsageSt
  = BoxUsageSt M.empty S.empty M.empty

-- | Monad to compute the use/defs of variables.
type BoxUsageM e = ExceptT e (ST.State BoxUsageSt)

putOptionBoxesMap :: OptionBoxMap -> BoxUsageM e ()
putOptionBoxesMap = ST.modify . (\s st -> st {optionBoxesMap = s})

putUsedVarSet :: VarSet -> BoxUsageM e ()
putUsedVarSet = ST.modify . (\s st -> st {usedVarSet = s})

putMovedBoxMap :: VarMap -> BoxUsageM e ()
putMovedBoxMap =ST.modify . (\s st -> st {movedBoxes = s})

withState :: (BoxUsageSt -> BoxUsageSt) -> BoxUsageM e a -> BoxUsageM e a
withState f = (ST.modify f >>)

-- Encapsulation mechanisms.
-- Useful to run computations in isolated environments.
runEncapsWithEmptyVars :: BoxUsageM e a -> BoxUsageM e a
runEncapsWithEmptyVars m = do
  st <- ST.get
  res <- withState (const $ st {usedVarSet = S.empty}) m
  ST.put st
  return res

-- Run computations encapusulates with same first state.
runMultipleEncapsWithEmptyVars :: [BoxUsageM e a] -> BoxUsageM e [a]
runMultipleEncapsWithEmptyVars ms = do
  st <- ST.get
  res <- mapM (withState (const $ st {usedVarSet = S.empty})) ms
  ST.put st
  return res

unifyState :: (OptionBoxMap, VarMap, VarSet) -> BoxUsageM e ()
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

unifyStates :: BoxUsageSt -> BoxUsageSt -> BoxUsageM e BoxUsageSt
unifyStates prev curr
  = return $ 
  BoxUsageSt {
    optionBoxesMap = M.union (optionBoxesMap curr) (optionBoxesMap prev),
    movedBoxes = M.union (movedBoxes curr) (movedBoxes prev),
    usedVarSet = S.union (usedVarSet curr) (usedVarSet prev)
  }

unsafeAddMap :: Identifier -> Location -> VarMap -> VarMap
unsafeAddMap = M.insert

unsafeAddSet :: Identifier -> VarSet -> VarSet
unsafeAddSet = S.insert

unionUsed :: OptionBoxMap -> VarSet -> BoxUsageM e ()
unionUsed optionBoxes regular =
  ST.modify (\st -> st {
    optionBoxesMap = M.union optionBoxes (optionBoxesMap st),
    usedVarSet = S.union regular (usedVarSet st)})

----------------------------------------
-- This function checks we have not reached the limit of the data structure.
safeUseVariable :: Identifier -> BoxUsageM BoxUsageError ()
-- Add Variable to use set
safeUseVariable ident 
  = do
    usedVarSet' <- ST.gets usedVarSet
    unless (S.size usedVarSet' < maxBound) (throwError $ annotateError Internal ESetMaxBound)
    putUsedVarSet $ unsafeAddSet ident usedVarSet'

safeMoveBox :: Identifier -> Location -> BoxUsageM BoxUsageError ()
safeMoveBox ident loc
  = ST.gets movedBoxes
  >>= \boxSet ->
    case M.lookup ident boxSet of
      Just prevLoc -> throwError $ annotateError prevLoc (EBoxMovedTwice ident loc)
      Nothing -> do
        unless (M.size boxSet < maxBound) (throwError $ annotateError Internal ESetMaxBound)
        putMovedBoxMap (unsafeAddMap ident loc boxSet)
        safeUseVariable ident

safeUpdateOptionBox :: Identifier -> MVars -> BoxUsageM BoxUsageError ()
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
defBox :: Identifier -> Location -> BoxUsageM BoxUsageError ()
defBox ident loc
  = ST.gets movedBoxes
  >>= \boxSet ->
    if M.member ident boxSet
    then do
      putMovedBoxMap $ M.delete ident boxSet
    else
      throwError $ annotateError loc (EBoxNotMoved ident)

moveOptionBox :: Identifier -> Location -> BoxUsageM BoxUsageError ()
moveOptionBox ident loc
  = do
    optionBoxMap <- ST.gets optionBoxesMap
    case M.lookup ident optionBoxMap of
      Just (Allocated _) -> safeUpdateOptionBox ident (Moved loc) >> safeUseVariable ident
      Just (Moved prevLoc) -> throwError $ annotateError loc (EOptionBoxMovedTwice ident prevLoc)
      Just (Defined _) -> throwError $ annotateError Internal EVarRedefinition;
      Nothing -> safeUpdateOptionBox ident (Moved loc) >> safeUseVariable ident

initializeOptionBox :: Identifier -> Location -> BoxUsageM BoxUsageError ()
initializeOptionBox ident loc
  = do
    optionBoxMap <- ST.gets optionBoxesMap
    case M.lookup ident optionBoxMap of
      Just (Moved prevLoc) -> throwError $ annotateError loc (EMovedWithoutAlloc ident prevLoc)
      -- Define in the future? This case shouldn't happen
      Just (Defined _) -> throwError $ annotateError Internal EVarRedefinition;
      _ -> return ()
      
allocOptionBox :: Identifier -> Location -> BoxUsageM BoxUsageError ()
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

-- | An option-box that nobody mentions is reported as an unused variable by
-- the forward pass, so here it is simply not tracked.
defVariableOptionBox :: Identifier -> Location -> BoxUsageM BoxUsageError ()
defVariableOptionBox ident loc =
  maybe
    (return ())
    (\case{
        -- Allocated after defined.
        Allocated _ -> safeUpdateOptionBox ident (Defined loc);
        -- Skipped allocation
        Moved prevLoc -> throwError $ annotateError loc (EMovedWithoutAlloc ident prevLoc);
        -- Defined;Defined not allowed,
        Defined _ -> throwError $ annotateError Internal EDefinedTwice 
        }) . M.lookup ident =<< ST.gets optionBoxesMap

-- Procedures can receive /box/ variables as arguments.
-- Box variables have a special use, through free or stuff.
-- So we need to analyze each argument to decide if it is normal variable or
-- box.
defArgumentsProc :: Parameter a -> Location -> BoxUsageM BoxUsageError ()
defArgumentsProc ps loc
  = case paramType ps of
      TBoxSubtype _ -> defBox (paramIdentifier ps) loc
      -- | A plain parameter is a variable, which the forward pass owns
      _ -> return ()

----------------------------------------
-- Run computation and get its result.
runBoxUsage :: BoxUsageM e a -> (Either e a, BoxUsageSt)
runBoxUsage = flip ST.runState emptyBoxUsageSt . runExceptT
----------------------------------------
