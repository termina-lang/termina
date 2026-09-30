-- | The prover of the values: a check holds when every value its operands may
-- take at that point is one for which it does, which is what the value
-- analysis works out while it walks the body. The prover only reads what the
-- analysis found for the module.
module Elaboration.Prover.Value (valueProver) where

import Elaboration.Obligations
import Elaboration.Prover

import qualified Data.Map.Strict as M
import qualified Data.Set as S

valueProver :: M.Map ObligationId Evidence -> Prover
valueProver evidence = Prover "value" proveScope

  where

    proveScope _ obligations =
      M.restrictKeys evidence (S.fromList (map obligationId obligations))
