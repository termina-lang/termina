module ControlFlow.BasicBlocks.Errors where

-- | This type represents the possible errors that can occur during the generation of basic blocks.
-- In its current form, the only possible error is an internal error, which is used to signal that
-- an unexpected situation has occurred.
newtype BBGeneratorError = InternalError String
    deriving (Show)
