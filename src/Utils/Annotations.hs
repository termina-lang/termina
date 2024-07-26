{-# Language KindSignatures #-}
-- | Module describing a family of types equipped with annotations

module Utils.Annotations where

import Data.Kind
import Control.Monad.Except

class Annotated (d :: Type -> Type) where
  getAnnotation :: d a -> a

data AnnotatedError a b = 
    AnnotatedError a b
  deriving Show

getError :: AnnotatedError a b -> a
getError (AnnotatedError err _ann) = err

annotateError :: b -> a -> AnnotatedError a b
annotateError = flip AnnotatedError

withAnnotation :: Functor m => b -> ExceptT e m a -> ExceptT (AnnotatedError e b) m a
withAnnotation ann = withExceptT (annotateError ann)