module DataFlow.Program.Errors.Errors where

import AST.Seman

data ProgramErrors = 
    -- | Error when the task is not defined
    DuplicatedEmitterConnection Identifier
    | DuplicatedChannelConnection Identifier
    | UnsupportedEmitterClass Identifier