module Command.Types where

import qualified Data.Map.Strict as M

import qualified Parser.AST as PAST
import qualified Semantic.AST as SAST
import qualified ControlFlow.AST as CFAST

import Semantic.Types
import Parser.Types

import Modules.Modules

newtype ParsingData = ParsingData {
  parsedAST :: PAST.AnnotatedProgram ParserAnn
} deriving (Show)

newtype SemanticData = SemanticData {
  typedAST :: SAST.AnnotatedProgram SemanticAnn
} deriving (Show)

newtype BasicBlocksData = BasicBlockData {
  basicBlocksAST :: CFAST.AnnotatedProgram SemanticAnn
} deriving (Show)

type ParsedModule = TerminaModuleData ParsingData
type TypedModule = TerminaModuleData SemanticData
type BasicBlocksModule = TerminaModuleData BasicBlocksData
type ParsedProject = M.Map QualifiedName ParsedModule
type TypedProject = M.Map QualifiedName TypedModule
type BasicBlocksProject = M.Map QualifiedName BasicBlocksModule
type ProjectDependencies = M.Map QualifiedName [QualifiedName]
