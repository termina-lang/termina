{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The shape of a body as a sequence of basic blocks, parametric in the
-- types, the expressions and the objects it holds, as the core AST is. Each
-- stage that works on basic blocks instantiates it with its own: the lowered
-- AST with the expressions of the semantic AST, and the elaborated AST with the
-- expressions whose run-time checks are decided.
module BasicBlocks where

import Core.AST (Identifier, AccessKind)
import Utils.Annotations

data MatchCase' ty expr obj a = MatchCase
  {
    matchIdentifier :: Identifier
  , matchBVars      :: [(Identifier, a)] -- ^ variables the variant binds, each with its own position
  , matchBody       :: Block' ty expr obj a
  , matchAnnotation :: a
  } deriving (Functor)

data DefaultCase' ty expr obj a = DefaultCase
  (Block' ty expr obj a)
  a
  deriving (Functor)

data CondIf' ty expr obj a = CondIf
  {
    condIfCond       :: expr a
  , condIfBody       :: Block' ty expr obj a
  , condIfAnnotation :: a
  } deriving (Functor)

data CondElse' ty expr obj a = CondElse
  {
    condElseBody       :: Block' ty expr obj a
  , condElseAnnotation :: a
  } deriving (Functor)

data CondElseIf' ty expr obj a = CondElseIf
  {
    condElseIfCond       :: expr a
  , condElseIfBody       :: Block' ty expr obj a
  , condElseIfAnnotation :: a
  } deriving (Functor)

data Statement' ty expr obj a =
  -- | Declaration statement
  Declaration
    Identifier -- ^ name of the variable
    AccessKind -- ^ kind of declaration (mutable "var" or immutable "let")
    (ty a) -- ^ type of the variable
    (Maybe (expr a)) -- ^ optional initialization expression
    a
  | AssignmentStmt
    (obj a) -- ^ left hand side of the assignment
    (expr a) -- ^ assignment expression
    a
  | SingleExpStmt
    (expr a) -- ^ expression
    a
  deriving (Functor)

data BasicBlock' ty expr obj a =
    -- | If-else-if basic block
    IfElseBlock
        (CondIf' ty expr obj a) -- ^ if condition and body
        [CondElseIf' ty expr obj a] -- ^ list of else if blocks
        (Maybe (CondElse' ty expr obj a)) a -- ^ statements in the else block
    -- | For-loop basic block
    | ForLoopBlock
        Identifier -- ^ name of the iterator variable
        (ty a) -- ^ type of iterator variable
        (expr a) -- ^ initial value of the iterator
        (expr a) -- ^ final value of the iterator
        (Maybe (expr a)) -- ^ break condition (optional)
        (Block' ty expr obj a) a
    -- | Match basic block
    | MatchBlock (expr a) [MatchCase' ty expr obj a] (Maybe (DefaultCase' ty expr obj a)) a
    -- | Send message
    | SendMessage (obj a) (expr a) a
    -- | Invoke a resource procedure
    | ProcedureInvoke
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
    -- | Call to the alloc procedure of a memory allocator
    | AllocBox
        (obj a) -- port that implements the allocator interface
        (expr a) -- ^ argument expression
        a
    -- | Call to the free procedure of a memory allocator
    | FreeBox
        (obj a) -- port that implements the allocator interface
        (expr a) -- ^ argument expression
        a
    -- | Regular block (list of statements)
    | RegularBlock [Statement' ty expr obj a]
    | ReturnBlock
        (Maybe (expr a)) -- ^ return expression
        a
    | ContinueBlock
        (expr a)
        a
    | RebootBlock a
    -- | System call
    | SystemCall
        (obj a) -- ^ access port
        Identifier -- ^ name of the system call
        [expr a] -- ^ list of arguments
        a
    deriving (Functor)

-- | |BlockRet| represent a body block with its return statement
data Block' ty expr obj a
  = Block
  {
    blockBody :: [BasicBlock' ty expr obj a],
    blockAnnotation :: a
  }
  deriving (Functor)

deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (MatchCase' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (DefaultCase' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (CondIf' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (CondElse' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (CondElseIf' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (Statement' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (BasicBlock' ty expr obj a)
deriving instance (Show (ty a), Show (expr a), Show (obj a), Show a) => Show (Block' ty expr obj a)

instance Annotated (Statement' ty expr obj) where
  getAnnotation (Declaration _ _ _ _ ann) = ann
  getAnnotation (AssignmentStmt _ _ ann) = ann
  getAnnotation (SingleExpStmt _ ann) = ann

  updateAnnotation (Declaration idk ak t expr _) =
    Declaration idk ak t expr
  updateAnnotation (AssignmentStmt obj expr _) =
    AssignmentStmt obj expr
  updateAnnotation (SingleExpStmt expr _) =
    SingleExpStmt expr
