{-# Language KindSignatures #-}
-- | Module describing a family of types equipped with annotations

module Annotations where

class Annotated (d :: * -> *) where
  getAnnotation :: d a -> a

class HAnnotated (d :: ( * -> * ) -> * -> *) where
  getHAnnotation :: d exp a -> a

class HHAnnotated (d :: ( * -> * ) -> (* -> *) -> * -> *) where
  getHHAnnotation :: d e lr a -> a
