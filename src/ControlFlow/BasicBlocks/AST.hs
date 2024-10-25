{-# LANGUAGE DeriveFunctor  #-}

module ControlFlow.BasicBlocks.AST (
  module ControlFlow.BasicBlocks.AST,
  module Core.AST,
  Object'(..),
  Expression'(..),
  Object,
  Expression,
  FieldDefinition,
  InterfaceMember,
  EnumVariant,
  Parameter,
  Modifier,
  Const
) where

import Core.AST
import Semantic.AST (
  Expression'(..), 
  Object'(..), 
  FieldDefinition, 
  InterfaceMember,
  EnumVariant,
  Parameter, 
  Expression, 
  Object, 
  Modifier, 
  Const)
import Modules.Modules


data MatchCase' ty expr obj a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [Identifier]
  , matchBody       :: Block' ty expr obj a
  , matchAnnotation :: a
  } deriving (Show,Functor)

data ElseIf' ty expr obj a = ElseIf
  {
    elseIfCond       :: expr a
  , elseIfBody       :: Block' ty expr obj a
  , elseIfAnnotation :: a
  } deriving (Show, Functor)

data Statement' ty expr obj a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    ty -- ^ type of the variable
    (expr a) -- ^ initialization expression
    a
  | AssignmentStmt
    (obj a) -- ^ left hand side of the assignment
    (expr a) -- ^ assignment expression
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  deriving (Show, Functor)

data BasicBlock' ty expr obj a =
    -- | If-else-if basic block
    IfElseBlock 
        (expr a) -- ^ conditional expression
        (Block' ty expr obj a) -- ^ basic blocks in the if block
        [ElseIf' ty expr obj a] -- ^ list of else if blocks (possibly empty)
        (Maybe (Block' ty expr obj a)) a -- ^ basic blocks in the else
    -- | For-loop basic block
    | ForLoopBlock 
        Identifier -- ^ name of the iterator variable
        ty -- ^ type of iterator variable
        (expr a) -- ^ initial value of the iterator
        (expr a) -- ^ final value of the iterator
        (Maybe (expr a)) -- ^ break condition (optional)
        (Block' ty expr obj a) a
    -- | Match basic block
    | MatchBlock (expr a) [MatchCase' ty expr obj a] a
    -- | Send message
    | SendMessage (obj a) (expr a) a
    -- | Call to a resource procedure
    | ProcedureCall 
        (obj a) -- ^ access port
        Identifier -- ^ name of the procedure
        [expr a] -- ^ list of arguments
        a
    | AtomicLoad 
        (obj a) -- ^ access port
        (expr a) -- ^ expression that points to the object where the value will be stored
        a
    | AtomicStore
        (obj a) -- ^ access port
        (expr a) -- ^ value to store
        a
    | AtomicArrayLoad
        (obj a) -- ^ access port
        (expr a) -- ^ index expression
        (expr a) -- ^ expression that points to the object where the value will be stored
        a
    | AtomicArrayStore
        (obj a) -- ^ access port
        (expr a) -- ^ index expression
        (expr a) -- ^ value to store
        a
    -- | Call to the alloc procedure of a memory allocator
    | AllocBox
        (obj a) -- port that implements the allocator interface
        (expr a) -- ^ argument expression
        a
    -- | Call to the free procedure of a memory allocator 
    | FreeBox
        (obj a) -- port that implements the allocator interface
        (expr a) -- ^ argument expression
        a
    -- | Regular block (list of statements)
    | RegularBlock [Statement' ty expr obj a]
    | ReturnBlock 
        (Maybe (expr a)) -- ^ return expression
        a
    | ContinueBlock
        (expr a)
        a
    deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data Block' ty expr obj a
  = Block
  {
    blockBody :: [BasicBlock' ty expr obj a],
    blockAnnotation :: a
  }
  deriving (Show, Functor)

type BasicBlock = BasicBlock' TerminaType Expression Object

type Block = Block' TerminaType Expression Object
type AnnASTElement = AnnASTElement' TerminaType Block Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' TerminaType Expression

type TypeDef = TypeDef' TerminaType Block

type ClassMember = ClassMember' TerminaType Block

type MatchCase = MatchCase' TerminaType Expression Object
type ElseIf = ElseIf' TerminaType Expression Object
type Statement = Statement' TerminaType Expression Object

type AnnotatedProgram a = [AnnASTElement' TerminaType Block Expression a]

type Module = Module' QualifiedName
type TerminaModule = TerminaModule' TerminaType Block Expression QualifiedName 