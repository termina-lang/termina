-- | Semantic Analysis of modules.

module Modules.Typing where

-- From ordering (and parsing) the project we have
-- Project: |Map ModuleName (... , PAST.TerminaProgram Annotation)|
-- We want to have now: |Map Module Name SAST.TerminaProgram ..|

import Modules.Modules
import qualified AST.Parser as PAST
import qualified AST.Seman as SAST

typeProject :: Monad m =>
  M.Map ModuleName ([ModuleName], PAST.TerminaProgram Annotation)
  -> m (M.Map ModuleName ([ModuleName], SAST.TerminaProgram Annotation))
typeProject = _
