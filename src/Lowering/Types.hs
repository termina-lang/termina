module Lowering.Types (
    LoweringMonad
) where

import Control.Monad.Except
import Lowering.Errors

-- | This type represents the monad used to generate basic blocks.
type LoweringMonad = Except LoweringError
