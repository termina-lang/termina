module ControlFlow.BasicBlocks.Errors.Errors where
import Utils.Annotations

-- | This type represents the possible errors that can occur during the generation of basic blocks.
-- In its current form, the only possible error is an internal error, which is used to signal that
-- an unexpected situation has occurred.
newtype BBGeneratorError = InternalError String
    deriving (Show)

data ExitCheckError =
    BBInvalidCheckState -- ^ Invalid check state (Internal)
    | BBInvalidReturn -- ^ Invalid return statement (EE-001)
    | BBInvalidContinue -- ^ Invalid continue statement (EE-002)
    | BBInvalidSend -- ^ Invalid send statement (EE-003)
    | BBBlockShallExit -- ^ Missing return statement (EE-004)
    | BBActionShallExit -- ^ Missing exit point on an action (EE-005)
    | BBActionInvalidContinue -- ^ Invalid continue statement on an action (EE-006)
    | BBActionInvalidSend -- ^ Invalid send statement on an action (EE-007)
    | BBActionIfBlockShallExit -- ^ Missing continue statement on an action if block (EE-008)
    | BBActionMatchBlockShallExit -- ^ Missing continue statement on an action match block (EE-009)
    | BBActionIfBlockShallNotExit -- ^ If block shall not exit (EE-010)
    | BBActionMatchBlockShallNotExit -- ^ Match block shall not exit (EE-011)
    | BBActionIfBlockMissingElseExit -- ^ Missing else exit on an action if block (EE-012)
    deriving (Show)

type BBPathsCheckError = AnnotatedError ExitCheckError Location