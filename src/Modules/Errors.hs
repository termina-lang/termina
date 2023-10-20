-- | Module of Module errors

module Modules.Errors where

import AST.Core
-- import Semantic.Errors (Errors)
import Semantic.Monad (SemanticErrors)


import Modules.Modules

data Errors
  = EModuleNotParsed ModuleName
  | EDependencyNotTyped ModuleName
  | ENameCollition ModuleName Identifier
  | ELiftTypeCheckError SemanticErrors
  deriving Show
