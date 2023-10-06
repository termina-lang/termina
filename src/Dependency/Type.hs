-- | Module Computing Dependency of elements

module Dependency.Type where

import AST.Core
import Annotations

-- We use sets (implemented as trees)
import Data.Set as S

newtype Dep a
  = Dep {unDep :: (a , S.Set Identifier) }

getDependencies :: Annotated d => d (Dep a) -> S.Set Identifier
getDependencies = snd . unDep . getAnnotation
