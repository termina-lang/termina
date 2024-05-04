-- | Module of Module errors

module Modules.Errors where

import AST.Core
import Semantic.Errors (SemanticErrors)

import Modules.Modules
import qualified Data.Text.Lazy as TL

data Errors
  = EModuleNotParsed ModuleName
  | EDependencyNotTyped ModuleName
  | ENameCollition ModuleName Identifier
  | ELiftTypeCheckError SemanticErrors TL.Text
  deriving Show
