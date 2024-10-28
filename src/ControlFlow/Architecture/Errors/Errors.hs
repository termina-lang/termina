module ControlFlow.Architecture.Errors.Errors (
    ArchitectureError, Error(..)
) where

import ControlFlow.BasicBlocks.AST
import Utils.Annotations

data Error = 
    EUnboxingObject -- ^ Error when trying to unbox an object (Internal)
    | EUnboxingExpression -- ^ Error when trying to unbox an expression (Internal)
    | EUnboxingPort -- ^ Error when trying to unbox a port (Internal)
    | EUnboxingBox -- ^ Error when trying to unbox a box (Internal)
    | EUnboxingOptionBox -- ^ Error when trying to unbox an option box (Internal)
    | EUnboxingMatchCase -- ^ Error when trying to unbox a match case (Internal)
    | EUnboxingClassField -- ^ Error when trying to unbox a class field (Internal)
    | EUnboxingChannel -- ^ Error when trying to unbox a channel (Internal)
    | EUnboxingTask -- ^ Error when trying to unbox a task (Internal)
    | EUnboxingHandler -- ^ Error when trying to unbox a handler (Internal)
    | EUnboxingResource -- ^ Error when trying to unbox a resource (Internal)
    | EUnboxingPool -- ^ Error when trying to unbox a pool (Internal)
    | EUnboxingProcedureCall -- ^ Error when trying to unbox a procedure call (Internal)
    | EUnsupportedEmitterClass -- ^ Unsupported emitter class (Internal)
    | EUnboxingFree -- ^ Error when trying to unbox a free (Internal)
    | EDuplicatedEmitterConnection Identifier Location -- ^ Duplicated emitter connection (AE-001)
    | EDuplicatedChannelConnection Identifier Location -- ^ Duplicated channel connection (AE-002)
    | EMismatchedBoxSource Identifier Identifier [Location] -- ^ Mismatched box source (AE-003)
    deriving Show

type ArchitectureError = AnnotatedError Error Location