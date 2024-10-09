module ControlFlow.Architecture.Errors.Errors (
    ProgramError, Error(..)
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
    | EDuplicatedEmitterConnection Identifier Location -- ^ Duplicated emitter connection (AE-001)
    | EDuplicatedChannelConnection Identifier Location -- ^ Duplicated channel connection (AE-002)
    | EUnsupportedEmitterClass Identifier  -- ^ Unsupported emitter class (AE-003)
    deriving Show

type ProgramError = AnnotatedError Error Location