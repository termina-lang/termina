-- | Module With Errors

module DataFlow.Errors where

import AST.Core (Identifier)

import Parser.Parsing (Annotation)

import Semantic.Monad (SemanticAnns)

data Errors
  = SetMaxBound
  | MapMaxBound
  | ImpossibleError
  | ImpossibleErrorMatchGetType
  | NotUsed Identifier
  | NotUsedOO Identifier
  | UsingTwice Identifier
  | DifferentOnlyOnce
  | DifferentDynsSets
  | DifferentOnlyOnceMatch
  | DifferentDynsSetsMatch
  | ForMoreOOpt
  | ForMoreODyn
  | InternalOptionMissMatch
  | ImpossibleErrorBadAllocArg
  -- Special Variable errors
  | AllocNotUsed Identifier
  | AllocTwice Identifier
  | AllocRedef Identifier -- This is impossible
  | DefinedNotAlloc Identifier
  | DefinedTwice Identifier
  -- Dyn
  | DefiningDyn Identifier
  -- vv| NotUsedDyn Identifier
  deriving Show

data AnnotatedErrors
  = AnErrors {location :: Annotation , err :: Errors}
  deriving Show

annError :: Annotation -> Errors -> AnnotatedErrors
annError = AnErrors
