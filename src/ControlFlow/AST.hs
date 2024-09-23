{-# LANGUAGE DeriveFunctor  #-}

module ControlFlow.AST where

import Core.AST
import Semantic.AST (Expression', Object)
import Modules.Modules

data Statement' expr obj a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    TypeSpecifier -- ^ type of the variable
    (expr a) -- ^ initialization expression
    a
  | AssignmentStmt
    (obj a) -- ^ name of the variable
    (expr a) -- ^ assignment expression
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  deriving (Show, Functor)

data ElseIf' expr obj a = ElseIf (expr a) [BasicBlock' expr obj a] a
    deriving (Show, Functor)

data MatchCase' expr obj a = 
    MatchCase
        Identifier
        [Identifier]
        [BasicBlock' expr obj a]
        a
    deriving (Show, Functor)

data BasicBlock' expr obj a =
    -- | If-else-if basic block
    IfElseBlock 
        (expr a) -- ^ conditional expression
        [BasicBlock' expr obj a] -- ^ basic blocks in the if block
        [ElseIf' expr obj a] -- ^ list of else if blocks (possibly empty)
        (Maybe [BasicBlock' expr obj a]) a -- ^ basic blocks in the else
    -- | For-loop basic block
    | ForLoopBlock 
        Identifier -- ^ name of the iterator variable
        TypeSpecifier -- ^ type of iterator variable
        (expr a) -- ^ initial value of the iterator
        (expr a) -- ^ final value of the iterator
        (Maybe (expr a)) -- ^ break condition (optional)
        [BasicBlock' expr obj a] a
    -- | Match basic block
    | MatchBlock (expr a) [MatchCase' expr obj a] a
    -- | Send message
    | SendMessage Identifier (expr a) a
    -- | Call to a resource procedure
    | ProcedureCall 
        Identifier -- ^ name of the access port
        Identifier -- ^ name of the procedure
        [expr a] -- ^ list of arguments
        a
    | AtomicLoad 
        Identifier -- ^ name of the access port
        (expr a) -- ^ expression that points to the object where the value will be stored
        a
    | AtomicStore
        Identifier -- ^ name of the access port
        (expr a) -- ^ value to store
        a
    | AtomicArrayLoad
        Identifier -- ^ name of the access port
        (expr a) -- ^ index expression
        (expr a) -- ^ expression that points to the object where the value will be stored
        a
    | AtomicArrayStore
        Identifier -- ^ name of the access port
        (expr a) -- ^ index expression
        (expr a) -- ^ value to store
        a
    -- | Call to the alloc procedure of a memory allocator
    | AllocBox
        Identifier -- ^ name of the port that implements the allocator interface
        (expr a) -- ^ argument expression
        a
    -- | Call to the free procedure of a memory allocator 
    | FreeBox
        Identifier -- ^ name of the port that implements the allocator interface
        (expr a) -- ^ argument expression
        a
    -- | Regular block (list of statements)
    | RegularBlock [Statement' expr obj a]
    deriving (Show, Functor)

-- | |BlockRet| represent a body block with its return statement
data BlockRet' expr obj a
  = BlockRet
  {
    blockBody :: [BasicBlock' expr obj a]
  , blockRet  :: ReturnStmt' expr a
  }
  deriving (Show, Functor)

-- type OptionVariant a = OptionVariant' (Expression a)
type Expression = Expression' Object

type BasicBlock = BasicBlock' Expression Object

type ReturnStmt = ReturnStmt' Expression
type BlockRet = BlockRet' Expression Object
type AnnASTElement = AnnASTElement' BlockRet Expression
type FieldAssignment = FieldAssignment' Expression
type Global = Global' Expression

type TypeDef = TypeDef' BlockRet

type ClassMember = ClassMember' BlockRet

type MatchCase = MatchCase' Expression Object
type ElseIf = ElseIf' Expression Object
type Statement = Statement' Expression Object

type AnnotatedProgram a = [AnnASTElement' BlockRet Expression a]

type Module = Module' QualifiedName
type TerminaModule = TerminaModule' BlockRet Expression QualifiedName 