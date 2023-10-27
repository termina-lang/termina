-- | Module of Module errors

module Modules.Errors where

import AST.Core
import Semantic.Errors (SemanticErrors)

import Modules.Modules

data Errors
  = EModuleNotParsed ModuleName
  | EDependencyNotTyped ModuleName
  | ENameCollition ModuleName Identifier
  | ELiftTypeCheckError SemanticErrors
  deriving Show
