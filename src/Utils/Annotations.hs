{-# Language KindSignatures #-}
-- | Module describing a family of types equipped with annotations

module Utils.Annotations where

import Data.Kind
import Control.Monad.Except
import Text.Parsec.Pos

data TLocation =
  Position SourcePos SourcePos -- ^ Source code start and end position
  | Builtin -- ^ Builtin position for elements that are not in the source code
  | Internal
  -- ^ Internal error position. Used for debugging, internals shoulnd't happen
  deriving Show

data Located a = Located {
    -- | Located element
    element :: a,
     -- | TLocation on source code
    location :: TLocation
} deriving Show

locate :: TLocation -> a -> Located a
locate = flip Located

class Annotated (d :: Type -> Type) where
  getAnnotation :: d a -> a

data AnnotatedError a b = 
    AnnotatedError a b
  deriving Show

getError :: AnnotatedError a b -> a
getError (AnnotatedError err _ann) = err

annotateError :: b -> a -> AnnotatedError a b
annotateError = flip AnnotatedError

withLocation :: Functor m => b -> ExceptT e m a -> ExceptT (AnnotatedError e b) m a
withLocation ann = withExceptT (annotateError ann)