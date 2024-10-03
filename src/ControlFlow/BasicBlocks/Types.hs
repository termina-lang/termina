module ControlFlow.BasicBlocks.Types (
    BBGenerator
) where

import Control.Monad.Except
import ControlFlow.BasicBlocks.Errors

-- | This type represents the monad used to generate basic blocks.
type BBGenerator = Except BBGeneratorError
