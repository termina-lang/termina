module ControlFlow.BasicBlocks.Checks.ExitPaths.Errors where
import Utils.Annotations
import Core.AST

data ExitCheckError =
    EEInvalidCheckState -- ^ Invalid check state (Internal)
    | EEInvalidReturn -- ^ Invalid return statement (EE-001)
    | EEInvalidContinue -- ^ Invalid continue statement (EE-002)
    | EEInvalidSend -- ^ Invalid send statement (EE-003)
    | EEBlockShallExit -- ^ Missing return statement (EE-004)
    | EEActionShallExit -- ^ Missing exit point on an action (EE-005)
    | EEActionInvalidContinue -- ^ Invalid continue statement on an action (EE-006)
    | EEActionInvalidSend -- ^ Invalid send statement on an action (EE-007)
    | EEActionIfBlockShallExit -- ^ Missing continue statement on an action if block (EE-008)
    | EEActionMatchBlockShallExit -- ^ Missing continue statement on an action match block (EE-009)
    | EEActionIfBlockShallNotExit -- ^ If block shall not exit (EE-010)
    | EEActionMatchBlockShallNotExit -- ^ Match block shall not exit (EE-011)
    | EEActionIfBlockMissingElseExit -- ^ Missing else exit on an action if block (EE-012)
    deriving (Show)

type PathsCheckError = AnnotatedError ExitCheckError TLocation

data TaskBoxAllocationError = 
    TBAEInvalidFree Identifier -- ^ Freeing a box into a different port (TBAE-001)
    | TBAEBoxSendBoxDifferentAllocators Identifier -- ^ Sending boxes from different allocators (TBAE-002)

type TBACheckError = AnnotatedError TaskBoxAllocationError TLocation