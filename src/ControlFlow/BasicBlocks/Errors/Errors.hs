module ControlFlow.BasicBlocks.Errors.Errors where
import Utils.Annotations

-- | This type represents the possible errors that can occur during the generation of basic blocks.
-- In its current form, the only possible error is an internal error, which is used to signal that
-- an unexpected situation has occurred.
newtype BBGeneratorError = InternalError String
    deriving (Show)

data ExitCheckError =
    BBInvalidCheckState
    | BBInvalidReturn
    | BBInvalidContinue
    | BBInvalidSend
    | BBBlockShallExit
    | BBIfBlockShallExit
    | BBMatchBlockShallExit
    | BBIfBlockShallNotExit
    | BBMatchBlockShallNotExit
    | BBBlockIfBlockMissingElseExit
    deriving (Show)

type BBPathsCheckError = AnnotatedError ExitCheckError Location