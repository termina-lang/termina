{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Command.Types where

import qualified Data.Map as M

import qualified Parser.AST as PAST
import qualified Semantic.AST as SAST
import qualified ControlFlow.BasicBlocks.AST as CFAST
import qualified EFP.Schedulability.TransPath.AST as TPAST

import qualified Parser.Types as PTYPES
import qualified Semantic.Types as STYPES
import qualified EFP.Schedulability.TransPath.Types as TTYPES

import Modules.Modules
import Utils.Annotations

newtype ParsingData = ParsingData {
  parsedAST :: PAST.AnnotatedProgram PTYPES.ParserAnn
} deriving (Show)

newtype SemanticData = SemanticData {
  typedAST :: SAST.AnnotatedProgram STYPES.SemanticAnn
} deriving (Show)

newtype BasicBlocksData = BasicBlockData {
  basicBlocksAST :: CFAST.AnnotatedProgram STYPES.SemanticAnn
} deriving (Show)

newtype TransPathData = TransPathData {
  transPathAST :: [TPAST.TransactionalWCEPath TTYPES.ParserAnn]
} deriving (Show)

type ParsedModule = TerminaModuleData ParsingData
type TypedModule = TerminaModuleData SemanticData
type BasicBlocksModule = TerminaModuleData BasicBlocksData
type TransPathModule = TerminaModuleData TransPathData
type ParsedProject = M.Map QualifiedName ParsedModule
type TypedProject = M.Map QualifiedName TypedModule
type BasicBlocksProject = M.Map QualifiedName BasicBlocksModule
type TransPathProject = M.Map QualifiedName TransPathModule

type ProjectDependencies = M.Map QualifiedName [ModuleDependency]