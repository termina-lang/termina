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

import Modules.Modules
import Utils.Annotations
import qualified EFP.Schedulability.WCET.AST as WTAST
import qualified EFP.Schedulability.Core.Types as SCHEDTYPES
import qualified EFP.Schedulability.RT.AST as RTPAST

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
  transPathAST :: [TPAST.TransactionalWCEPath SCHEDTYPES.ParserAnn]
} deriving (Show)

newtype WCETData = WCETData {
  wcetAST :: [WTAST.WCETPlatformAssignment SCHEDTYPES.ParserAnn]
} deriving (Show)

newtype RTData = RTData {
  rtAST :: [RTPAST.RTElement SCHEDTYPES.ParserAnn]
} deriving (Show)

type ParsedModule = TerminaModuleData ParsingData
type TypedModule = TerminaModuleData SemanticData
type BasicBlocksModule = TerminaModuleData BasicBlocksData
type TransPathModule = TerminaModuleData TransPathData
type WCETModule = TerminaModuleData WCETData
type RTModule = TerminaModuleData RTData

type ParsedProject = M.Map QualifiedName ParsedModule
type TypedProject = M.Map QualifiedName TypedModule
type BasicBlocksProject = M.Map QualifiedName BasicBlocksModule
type TransPathProject = M.Map QualifiedName TransPathModule
type WCETProject = M.Map QualifiedName WCETModule

type ProjectDependencies = M.Map QualifiedName [ModuleDependency]