-- | Module With Errors

module DataFlow.VarUsage.Errors where

import AST.Core (Identifier)

import DataFlow.VarUsage.Types

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Utils.Annotations

data Error
  = SetMaxBound
  | MapMaxBound
  | ImpossibleError
  | ImpossibleErrorMatchGetType
  | UsedIgnoredVariable Identifier
  | NotUsed Identifier
  | NotUsedOO Identifier
  | UsingTwice Identifier
  | DifferentOnlyOnce [M.Map Identifier MVars]
  | DifferentBoxesSets [S.Set Identifier]
  | DifferentOnlyOnceMatch
  | DifferentBoxesSetsMatch
  | ForMoreOOpt
  | ForMoreOBox [Identifier]
  | InternalOptionMissMatch
  | ImpossibleErrorBadAllocArg
  | ImpossibleErrorBadFreeArg
  | ImpossibleErrorBadSendArg
  | ImpossibleErrorBadReceiveArg
  -- Special Variable errors
  | AllocNotUsed Identifier
  | AllocTwice Identifier
  | AllocRedef Identifier -- This is impossible
  | DefinedNotAlloc Identifier
  | DefinedTwice Identifier
  -- Box
  | DefiningBox Identifier
  -- vv| NotUsedBox Identifier
  deriving Show

type VarUsageError = AnnotatedError Error Location
