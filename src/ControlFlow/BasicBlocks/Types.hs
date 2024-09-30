module ControlFlow.BasicBlocks.Types (
    BBGenerator,
    BBGeneratorError(..)
) where

import Control.Monad.Except

-- | This type represents the possible errors that can occur during the generation of basic blocks.
-- In its current form, the only possible error is an internal error, which is used to signal that
-- an unexpected situation has occurred.
newtype BBGeneratorError = InternalError String
    deriving (Show)

-- | This type represents the monad used to generate basic blocks.
type BBGenerator = Except BBGeneratorError