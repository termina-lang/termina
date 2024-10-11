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
    | EUnboxingFree -- ^ Error when trying to unbox a free (Internal)
    | EDuplicatedEmitterConnection Identifier Location -- ^ Duplicated emitter connection (AE-001)
    | EDuplicatedChannelConnection Identifier Location -- ^ Duplicated channel connection (AE-002)
    | EChannelWithoutSources Identifier -- ^ Channel without sources (AE-003)
    | EChannelWithoutTargets Identifier -- ^ Channel without targets (AE-004)
    | EDisconnectedResource Identifier -- ^ Disconnected resource (AE-005)
    | EDisconnectedPool Identifier -- ^ Disconnected pool (AE-006)
    | EMismatchedBoxSource Identifier Identifier [Location] -- ^ Mismatched box source (AE-007)
    deriving Show

type ArchitectureError = AnnotatedError Error Location