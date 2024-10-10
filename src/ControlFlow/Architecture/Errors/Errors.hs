module ControlFlow.Architecture.Errors.Errors (
    ArchitectureError, Error(..)
) where

import ControlFlow.BasicBlocks.AST
import Utils.Annotations

data Error = 
    EUnboxingObject -- ^ Error when trying to unbox an object (Internal)
    | EUnboxingExpression -- ^ Error when trying to unbox an expression (Internal)
    | EUnboxingPort -- ^ Error when trying to unbox a port (Internal)
    | EUnboxingBox -- ^ Error when trying to unbox a box (Internal)
    | EUnboxingOptionBox -- ^ Error when trying to unbox an option box (Internal)
    | EUnboxingMatchCase -- ^ Error when trying to unbox a match case (Internal)
    | EUnboxingClassField -- ^ Error when trying to unbox a class field (Internal)
    | EUnboxingChannelSource -- ^ Error when trying to unbox a channel source (Internal)
    | EUnboxingTaskSource -- ^ Error when trying to unbox a task source (Internal)
    | EUnboxingHandlerSource -- ^ Error when trying to unbox a handler source (Internal)
    | EUnboxingResourceSource -- ^ Error when trying to unbox a resource source (Internal)
    | EUnboxingProcedureCall -- ^ Error when trying to unbox a procedure call (Internal)
    | EUnsupportedEmitterClass Identifier  -- ^ Unsupported emitter class (Internal)
    | EDuplicatedEmitterConnection Identifier Location -- ^ Duplicated emitter connection (AE-001)
    | EDuplicatedChannelConnection Identifier Location -- ^ Duplicated channel connection (AE-002)
    | EDisconnectedChannel Identifier -- ^ Disconnected channel (AE-003)
    | EDisconnectedResource Identifier -- ^ Disconnected resource (AE-004)
    -- | Mismatched box source in task output port (AE-005)
    | EMismatchedBoxSourceTaskOutPort 
        Identifier -- ^ Task identifier
        Identifier -- ^ Port identifier 
        Identifier -- ^ Offending box source
        (Identifier, Location) -- ^ Previous box souce
    -- | Mismatched box source in handler output port (AE-006)
    | EMismatchedBoxSourceHandlerOutPort 
        Identifier -- ^ Task identifier
        Identifier -- ^ Port identifier 
        Identifier -- ^ Offending box source
        (Identifier, Location) -- ^ Previous box souce 
    -- | Mismatched box source in channel inbound connections (AE-007)
    | EMismatchedBoxSourceChannelInbound
        Identifier -- ^ Channel identifier
        (Identifier, Identifier) -- ^ Offending box source
        (Identifier, Identifier, Location) -- ^ Previous box source
    | EMismatchedBoxSourceProcedureCall
        Identifier -- ^ Resource identifier
        Identifier -- ^ Procedure identifier
        Integer -- ^ Argument number
        Identifier -- ^ Offending box source
        (Identifier, Location) -- ^ Previous box source
    deriving Show

type ArchitectureError = AnnotatedError Error Location