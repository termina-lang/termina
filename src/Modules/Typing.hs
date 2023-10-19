-- | Semantic Analysis of modules.

module Modules.Typing where

-- From ordering (and parsing) the project we have
-- Project: |Map ModuleName (... , PAST.TerminaProgram Annotation)|
-- We want to have now: |Map Module Name SAST.TerminaProgram ..|

import Modules.Modules
import qualified AST.Parser as PAST
import Parser.Parsing (Annotation)
import qualified AST.Seman as SAST
import Semantic.Monad (SemanticAnns)

import Control.Monad.Fail

import qualified Data.Map.Strict as M

data ModuleAST a = MData
  { moduleDeps :: [ModuleName]
  , moduleAST :: a
  }

type ParserProject = M.Map ModuleName (ModuleAST (PAST.TerminaProgram Annotation))
type SemanProject =  M.Map ModuleName (ModuleAST (SAST.TerminaProgram SemanticAnns))

-- type TypeProject = Except MErrors

typeProject :: MonadFail m
  -- We have a project just parsed
  => ParserProject
  -- a list indicating the order in which they need to be loaded.
  -> [ModuleName]
  -> m SemanProject
typeProject parserMap = flip typeProject' M.empty
  where
    typeProject' [] typedP = return typedP
    typeProject' (m:ms) tP =
      case M.lookup m parserMap of
        Nothing -> fail ("Module ("++ show m ++") in list but not parsed")
        Just mPData ->
          let deps = moduleDeps mPData in
          let parsedButNotTyped = moduleAST mPData in
          undefined
