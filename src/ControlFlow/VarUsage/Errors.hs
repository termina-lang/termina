-- | Module With Errors

module ControlFlow.VarUsage.Errors where

import Core.AST (Identifier)

import ControlFlow.VarUsage.Types

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Utils.Annotations

data Error
  = ESetMaxBound -- ^ The set has reached its maximum bound (Internal)  
  | EMapMaxBound -- ^ The map has reached its maximum bound (Internal)
  | EUnboxingObjectType -- ^ Error when trying to unbox an object type (Internal)
  | EUnboxingMatchType -- ^ Error when trying to get the type of a match case (Internal)
  | EDefiningBox -- ^ Error when trying to declare variable of box type (Internal)
  | EBadAllocArg -- ^ Bad argument for alloc (Internal)
  | EBadFreeArg -- ^ Bad argument for free (Internal)
  | EBadSendArg -- ^ Bad argument for send (Internal)
  | UsedIgnoredVariable Identifier -- ^ Using a variable that is ignored (VE-001)
  | NotUsed Identifier -- ^ Variable is not used (VE-002)
  | NotUsedOO Identifier -- ^ Option-box variable is not used (VE-003)
  | UsingTwice Identifier -- ^ Option-box variable is used twice (VE-004)
  | DifferentOnlyOnce [M.Map Identifier MVars]
  | DifferentBoxesSets [S.Set Identifier]
  | DifferentOnlyOnceMatch
  | DifferentBoxesSetsMatch
  | ForMoreOOpt
  | ForMoreOBox [Identifier]
  | InternalOptionMissMatch
  -- Special Variable errors
  | AllocNotUsed Identifier
  | AllocTwice Identifier
  | AllocRedef Identifier -- This is impossible
  | DefinedNotAlloc Identifier
  | DefinedTwice Identifier
  deriving Show

type VarUsageError = AnnotatedError Error Location
