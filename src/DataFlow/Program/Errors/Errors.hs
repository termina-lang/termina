module DataFlow.Program.Errors.Errors (
    ProgramError, annnotateError, Error(..)
) where

import AST.Seman
import Parser.Parsing

data Error a = 
    -- | Error when the task is not defined
    DuplicatedEmitterConnection Identifier a
    | DuplicatedChannelConnection Identifier a
    | UnsupportedEmitterClass Identifier
    deriving Show

data AnnotatedError a = 
    AnnotatedError {
        location :: a , error :: Error a }
  deriving Show

annnotateError :: a -> Error a -> AnnotatedError a
annnotateError = AnnotatedError

type ProgramError = AnnotatedError Annotation