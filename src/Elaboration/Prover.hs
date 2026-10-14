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

-- | The obligations of a scope and the ones the provers discharge.
discharge :: [Prover] -> Scope -> ([Obligation], M.Map ObligationId Evidence)
discharge provers scope = (obligations, M.unions [prove p scope obligations | p <- provers])

  where

    obligations = scopeObligations scope
