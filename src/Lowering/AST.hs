-- | The lowered AST: the bodies of the semantic AST as sequences of basic
-- blocks, where each operation on a port is a block of its own. It holds the
-- expressions and the objects of the semantic AST.
module Lowering.AST (
  module Lowering.AST,
  module Core.AST,
  module BasicBlocks,
  Object(..),
  Expression(..),
  FieldDefinition,
  InterfaceMember,
  EnumVariant,
  Parameter,
  Modifier,
  Const, TerminaType,
  semanticTree
) where

import Core.AST
import BasicBlocks
import Semantic.AST (
  Expression(..),
  Object(..),
  FieldDefinition,
  InterfaceMember,
  EnumVariant,
  Parameter,
  Expression,
  Object,
  Modifier,
  Const, TerminaType,
  semanticTree)
import Utils.Annotations (QualifiedName)

type MatchCase = MatchCase' TerminaType Expression Object
type DefaultCase = DefaultCase' TerminaType Expression Object
type CondIf = CondIf' TerminaType Expression Object
type CondElse = CondElse' TerminaType Expression Object
type CondElseIf = CondElseIf' TerminaType Expression Object
type Statement = Statement' TerminaType Expression Object
type BasicBlock = BasicBlock' TerminaType Expression Object
type Block = Block' TerminaType Expression Object

type AnnASTElement = AnnASTElement' TerminaType Expression Block
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TerminaType Expression

type TypeDef = TypeDef' TerminaType Expression Block

type ClassMember = ClassMember' TerminaType Block

type AnnotatedProgram a = [AnnASTElement' TerminaType Expression Block a]

type ModuleImport = ModuleImport' QualifiedName
type TerminaModule = TerminaModule' TerminaType Expression Block QualifiedName
