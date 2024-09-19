module DataFlow.Architecture.Errors.Errors (
    ProgramError, Error(..)
) where

import AST.Seman
import Utils.Annotations

data Error a = 
    -- | Error when the task is not defined
    DuplicatedEmitterConnection Identifier a
    | DuplicatedChannelConnection Identifier a
    | UnsupportedEmitterClass Identifier
    deriving Show

type ProgramError = AnnotatedError (Error Location) Location