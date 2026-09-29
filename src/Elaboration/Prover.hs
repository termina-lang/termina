-- | What a prover is and how the provers of a build are combined.
--
-- A prover is given a scope and its obligations and answers with the ones it
-- discharges, each with the reason. The provers run independently: an
-- obligation is discharged when any of them discharges it, and the first one
-- in the list gives the reason.
module Elaboration.Prover (
    Evidence(..)
  , Prover(..)
  , discharge
) where

import Elaboration.Obligations

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Why an obligation holds.
data Evidence = Evidence
  {
    evidenceProver :: String
  , evidenceReason :: String
  } deriving Show

data Prover = Prover
  {
    proverName :: String
  , prove :: Scope -> [Obligation] -> M.Map ObligationId Evidence
  }

-- | The obligations of a scope and the ones the provers discharge. Two
-- operations that share a name cannot be told apart, so neither of them is
-- handed to the provers and both keep their checks.
discharge :: [Prover] -> Scope -> ([Obligation], M.Map ObligationId Evidence)
discharge provers scope = (obligations, M.restrictKeys proved (S.fromList (map obligationId distinct)))

  where

    obligations = scopeObligations scope

    occurrences = M.fromListWith (+) [(obligationId o, 1 :: Int) | o <- obligations]

    distinct = filter ((== Just 1) . (`M.lookup` occurrences) . obligationId) obligations

    proved = M.unions [prove p scope distinct | p <- provers]
