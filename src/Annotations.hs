{-# Language KindSignatures #-}
-- | Module describing a family of types equipped with annotations

module Annotations where

import Data.Kind

class Annotated (d :: Type -> Type) where
  getAnnotation :: d a -> a

class HAnnotated (d :: ( Type -> Type ) -> Type -> Type) where
  getHAnnotation :: d exp a -> a

class HHAnnotated (d :: ( Type -> Type ) -> (Type -> Type) -> Type -> Type) where
  getHHAnnotation :: d e lr a -> a
