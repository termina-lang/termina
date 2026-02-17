{-# LANGUAGE DeriveFunctor  #-}

module ControlFlow.BasicBlocks.AST (
  module ControlFlow.BasicBlocks.AST,
  module Core.AST,
  Object(..),
  Expression(..),
  FieldDefinition,
  InterfaceMember,
  EnumVariant,
  Parameter,
  Modifier,
  Const, TerminaType
) where

import Core.AST
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
  Const, TerminaType)
import Utils.Annotations


data MatchCase a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [Identifier]
  , matchBody       :: Block a
  , matchAnnotation :: a
  } deriving (Show, Functor)

data DefaultCase a = DefaultCase
  (Block a)
  a
  deriving (Show, Functor)

data CondIf a = CondIf
  {
    condIfCond       :: Expression a
  , condIfBody       :: Block a
  , condIfAnnotation :: a
  } deriving (Show, Functor)

data CondElse a = CondElse
  {
    condElseBody       :: Block a
  , condElseAnnotation :: a
  } deriving (Show, Functor)

data CondElseIf a = CondElseIf
  {
    condElseIfCond       :: Expression a
  , condElseIfBody       :: Block a
  , condElseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    (TerminaType a) -- ^ type of the variable
    (Expression a) -- ^ initialization expression
    a
  | AssignmentStmt
    (Object a) -- ^ left hand side of the assignment
    (Expression a) -- ^ assignment expression
    a
  | SingleExpStmt
    (Expression a) -- ^ expression
    a
  deriving (Show, Functor)

data BasicBlock a =
    -- | If-else-if basic block
    IfElseBlock 
        (CondIf a) -- ^ if condition and body
        [CondElseIf a] -- ^ list of else if blocks
        (Maybe (CondElse a)) a -- ^ statements in the else block
    -- | For-loop basic block
    | ForLoopBlock 
        Identifier -- ^ name of the iterator variable
        (TerminaType a) -- ^ type of iterator variable
        (Expression a) -- ^ initial value of the iterator
        (Expression a) -- ^ final value of the iterator
        (Maybe (Expression a)) -- ^ break condition (optional)
        (Block a) a
    -- | Match basic block
    | MatchBlock (Expression a) [MatchCase a] (Maybe (DefaultCase a)) a
    -- | Send message
    | SendMessage (Object a) (Expression a) a
    -- | Invoke a resource procedure
    | ProcedureInvoke 
        (Object a) -- ^ access port
        Identifier -- ^ name of the procedure
        [Expression a] -- ^ list of arguments
        a
    | AtomicLoad 
        (Object a) -- ^ access port
        (Expression a) -- ^ expression that points to the object where the value will be stored
        a
    | AtomicStore
        (Object a) -- ^ access port
        (Expression a) -- ^ value to store
        a
    | AtomicArrayLoad
        (Object a) -- ^ access port
        (Expression a) -- ^ index expression
        (Expression a) -- ^ expression that points to the object where the value will be stored
        a
    | AtomicArrayStore
        (Object a) -- ^ access port
        (Expression a) -- ^ index expression
        (Expression a) -- ^ value to store
        a
    -- | Call to the alloc procedure of a memory allocator
    | AllocBox
        (Object a) -- port that implements the allocator interface
        (Expression a) -- ^ argument expression
        a
    -- | Call to the free procedure of a memory allocator 
    | FreeBox
        (Object a) -- port that implements the allocator interface
        (Expression a) -- ^ argument expression
        a
    -- | Regular block (list of statements)
    | RegularBlock [Statement a]
    | ReturnBlock 
        (Maybe (Expression a)) -- ^ return expression
        a
    | ContinueBlock
        (Expression a)
        a
    | RebootBlock a
    -- | System call
    | SystemCall
        (Object a) -- ^ access port
        Identifier -- ^ name of the system call
        [Expression a] -- ^ list of arguments
        a
    deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data Block a
  = Block
  {
    blockBody :: [BasicBlock a],
    blockAnnotation :: a
  }
  deriving (Show, Functor)

instance Annotated Statement where
  getAnnotation (Declaration _ _ _ _ ann) = ann
  getAnnotation (AssignmentStmt _ _ ann) = ann
  getAnnotation (SingleExpStmt _ ann) = ann

  updateAnnotation (Declaration idk ak t expr _) =
    Declaration idk ak t expr
  updateAnnotation (AssignmentStmt obj expr _) =
    AssignmentStmt obj expr
  updateAnnotation (SingleExpStmt expr _) =
    SingleExpStmt expr

type AnnASTElement = AnnASTElement' TerminaType Block Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TerminaType Expression

type TypeDef = TypeDef' TerminaType Block

type ClassMember = ClassMember' TerminaType Block

type AnnotatedProgram a = [AnnASTElement' TerminaType Block Expression a]

type ModuleImport = ModuleImport' QualifiedName
type TerminaModule = TerminaModule' TerminaType Block Expression QualifiedName 