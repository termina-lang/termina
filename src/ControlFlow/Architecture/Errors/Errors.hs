module ControlFlow.Architecture.Errors.Errors (
    ProgramError, Error(..)
) where

import ControlFlow.BasicBlocks.AST
import Utils.Annotations

data Error = 
    -- | Error when the task is not defined
    DuplicatedEmitterConnection Identifier Location
    | DuplicatedChannelConnection Identifier Location
    | UnsupportedEmitterClass Identifier
    | InputOutputPortMismatch Identifier Identifier
    deriving Show

type ProgramError = AnnotatedError Error Location