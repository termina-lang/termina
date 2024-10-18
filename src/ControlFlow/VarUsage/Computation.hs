-- | DSL to compute Use/Defs of Termina expressions

module ControlFlow.VarUsage.Computation (
  UDM, UDSt(..), VarMap, OptionBoxMap,
  runEncapsulated, runEncaps, unionUsed, unifyState,
  defVariableOptionBox, defBox, defVariable, safeUseVariable,
  moveOptionBox, safeMoveBox, allocOptionBox, defArgumentsProc,
  runComputation
) where

import Core.AST (Identifier,Parameter(..),TerminaType(..))

import ControlFlow.VarUsage.Errors.Errors
import ControlFlow.VarUsage.Types

import qualified Control.Monad.State as ST
import Control.Monad.Except as E

-- Warning: Both implmentations have a limit on the number of elements they can
-- contain. They do not fail if the limit is rechead.
-- Sets
-- Maps
import qualified Data.Map.Strict as M
import Utils.Annotations ( Location(Internal), annotateError )

-- | Map of variables to the last location where were used/moved.
type VarMap = M.Map Identifier Location

-- | Map of option-box variables to their current state.
type OptionBoxMap = M.Map Identifier MVars

-- Internal state.
data UDSt = UDSt { 
    -- | Set of used variables
    usedVarMap :: VarMap,
    -- | Map with the current state of the option-box variables
    optionBoxesMap :: OptionBoxMap,
    -- | Map of moved boxes. It maps each box variable to the location where it
    -- was moved.
    movedBoxes :: VarMap
  }

emptyUDSt :: UDSt
emptyUDSt
  = UDSt M.empty M.empty M.empty

-- | Monad to compute the use/defs of variables.
type UDM e = ExceptT e (ST.State UDSt)

putOptionBoxesMap :: OptionBoxMap -> UDM e ()
putOptionBoxesMap = ST.modify . (\s st -> st {optionBoxesMap = s})

putUsedVarMap, putMovedBoxSet :: VarMap -> UDM e ()
putUsedVarMap = ST.modify . (\s st -> st {usedVarMap = s})
putMovedBoxSet =ST.modify . (\s st -> st {movedBoxes = s})

withState :: (UDSt -> UDSt) -> UDM e a -> UDM e a
withState f = (ST.modify f >>)

-- Encapsulation mechanisms.
-- Useful to run computations in isolated environments.

runEncapsulated :: UDM e a -> UDM e a
runEncapsulated m =
  ST.get >>= \st -> m >>= \res -> ST.put st >> return res

-- Run computations encapusulates with same first state.
runEncaps :: [UDM e a] -> UDM e [a]
runEncaps ms = do
  st <- ST.get
  res <- mapM (withState (const st)) ms
  ST.put st
  return res

unifyState :: (OptionBoxMap, VarMap, VarMap) -> UDM e ()
unifyState (oo, boxes, regular)
  = ST.get
  >>= \st
  -> ST.put
    -- M.union is left biased. Meaning that it takes priority over the other map
    -- when key collision. It is what we want tho.
     st { 
        optionBoxesMap = M.union oo (optionBoxesMap st),
        movedBoxes = M.union boxes (movedBoxes st),
        usedVarMap = M.union regular (usedVarMap st)
      }

unsafeAdd :: Identifier -> Location -> VarMap -> VarMap
unsafeAdd = M.insert

unionUsed :: VarMap -> UDM e ()
unionUsed vset =
  ST.modify (\st -> st{usedVarMap = M.union vset (usedVarMap st)})

removeUsed :: Identifier -> UDSt -> UDSt
removeUsed s st = st{usedVarMap = M.delete s (usedVarMap st)}

----------------------------------------
-- This function checks we have not reached the limit of the data structure.
safeUseVariable :: Identifier -> Location -> UDM Error ()
-- Add Variable to use set
safeUseVariable ident loc
  = do
    usedVarMap' <- ST.gets usedVarMap
    unless (M.size usedVarMap' < maxBound) (throwError ESetMaxBound)
    putUsedVarMap $ unsafeAdd ident loc usedVarMap'

safeMoveBox :: Identifier -> Location -> UDM Error ()
safeMoveBox ident loc
  = ST.gets movedBoxes
  >>= \boxSet ->
    case M.lookup ident boxSet of
      Just prevLoc -> throwError (EBoxMovedTwice ident prevLoc)
      Nothing -> do
        unless (M.size boxSet < maxBound) (throwError ESetMaxBound)
        putMovedBoxSet (unsafeAdd ident loc boxSet)

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
      putMovedBoxSet $ M.delete ident boxSet
    else
      throwError $ annotateError loc (EBoxNotMoved ident)

moveOptionBox :: Identifier -> Location -> UDM VarUsageError ()
moveOptionBox ident loc
  = do
    optionBoxMap <- ST.gets optionBoxesMap
    case M.lookup ident optionBoxMap of
      Just (Allocated _) -> safeUpdateOptionBox ident (Moved loc)
      Just s -> throwError $ annotateError loc (EOptionBoxMovedTwice ident (getLocation s))
      Nothing -> safeUpdateOptionBox ident (Moved loc)

allocOptionBox :: Identifier -> Location -> UDM VarUsageError ()
allocOptionBox ident loc
  =
  maybe
    (throwError $ annotateError loc (EAllocNotMoved ident))
    (\case{
        -- We use it in the future
        Moved _ -> safeUpdateOptionBox ident (Allocated loc);
        -- Re-allocation
        Allocated prevLoc -> throwError $ annotateError loc (EAllocTwice ident prevLoc);
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
        Defined prevLoc -> throwError $ annotateError loc (EDefinedTwice ident prevLoc);
        }) . M.lookup ident =<< ST.gets optionBoxesMap

-- If we define a variable that was not used, then error.
defVariable :: Identifier -> Location -> UDM VarUsageError ()
defVariable ident loc =
  ST.gets usedVarMap >>=
  \i ->
    case head ident of
      -- If the argument starts with an underscore, then the parameter must be ignored and not used.
      '_' -> 
        case M.lookup ident i of
          Nothing -> return ()
          Just useLoc -> throwError $ annotateError useLoc (EUsedIgnoredParameter ident)
      _ ->
        case M.lookup ident i of
          Just _ -> ST.modify (removeUsed ident)
          Nothing -> throwError $ annotateError loc (ENotUsed ident)

-- Procedures can receive /box/ variables as arguments.
-- Box variables have a special Use, through free or stuff.
-- So we need to analyze each argument to decide if it is normal variable or
-- box.
defArgumentsProc :: Parameter -> Location -> UDM VarUsageError ()
defArgumentsProc ps loc
  = (case paramTerminaType ps of
        BoxSubtype _ -> flip defBox loc
        _ -> flip defVariable loc)
    (paramIdentifier ps)

----------------------------------------
-- Run computation and get its result.
runComputation :: UDM e a -> (Either e a , UDSt )
runComputation = flip ST.runState emptyUDSt  . runExceptT
----------------------------------------
