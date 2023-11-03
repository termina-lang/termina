-- | Module With Errors

module DataFlow.Errors where

import AST.Core (Identifier)

import Parser.Parsing (Annotation)

data Errors
  = SetMaxBound
  | ImpossibleError
  | NotUsed Identifier
  | UsingTwice Identifier
  | DifferentOnlyOnce
  | DifferentOnlyOnceMatch
  | ForMoreOO
  | InternalOptionMissMatch

data AnnotatedErrors
  = AnErrors {location :: Annotation , err :: Errors}

annError :: Annotation -> Errors -> AnnotatedErrors
annError = AnErrors
