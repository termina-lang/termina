-- | Module With Errors

module DataFlow.Errors where

import AST.Core (Identifier)

import Parser.Parsing (Annotation)

import Semantic.Monad (SemanticAnns)

data Errors
  = SetMaxBound
  | ImpossibleError
  | ImpossibleErrorMatchGetType
  | NotUsed Identifier
  | NotUsedOO Identifier
  | UsingTwice Identifier
  | DifferentOnlyOnce
  | DifferentOnlyOnceMatch
  | ForMoreOO
  | InternalOptionMissMatch
  | ImpossibleErrorBadAllocArg
  deriving Show

data AnnotatedErrors
  = AnErrors {location :: Annotation , err :: Errors}
  deriving Show

annError :: Annotation -> Errors -> AnnotatedErrors
annError = AnErrors
