{-# Language KindSignatures #-}
{-# Language FlexibleInstances #-}
-- | Module describing a family of types equipped with annotations

module Utils.Annotations where

import Data.Kind
import Control.Monad.Except
import Text.Parsec.Pos

type QualifiedName = FilePath

data Location =
  Position QualifiedName !SourcePos !SourcePos -- ^ Source code start and end position
  | Builtin -- ^ Builtin position for elements that are not in the source code
  | Internal
  -- ^ Internal error position. Used for debugging, internals shoulnd't happen
  deriving Show

data LocatedElement a = LocatedElement {
    -- | LocatedElement element
    element :: a,
     -- | TFixedLocation on source code
    location :: Location
} deriving Show

class Located a where
  getLocation :: a -> Location
  updateLocation :: a -> Location -> a

instance Located (LocatedElement a) where
  getLocation = location
  updateLocation le loc = le { location = loc }

instance Located Location where
  getLocation = id
  updateLocation _ = id

locate :: Location -> a -> LocatedElement a
locate = flip LocatedElement

class Annotated (d :: Type -> Type) where
  getAnnotation :: d a -> a
  updateAnnotation :: d a -> a -> d a

data AnnotatedError a b =
    AnnotatedError a b
  deriving Show

instance Annotated (AnnotatedError a) where
  getAnnotation (AnnotatedError _err ann) = ann

  updateAnnotation (AnnotatedError err _) = AnnotatedError err
  
getError :: AnnotatedError a b -> a
getError (AnnotatedError err _ann) = err

annotateError :: b -> a -> AnnotatedError a b
annotateError = flip AnnotatedError

withLocation :: Functor m => b -> ExceptT e m a -> ExceptT (AnnotatedError e b) m a
withLocation ann = withExceptT (annotateError ann)

sameSource :: Location -> Location -> Bool
sameSource Builtin Builtin = True
sameSource Internal Internal = True
sameSource (Position f1 _ _) (Position f2 _ _) = f1 == f2
sameSource _ _ = False