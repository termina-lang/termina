-- | The declarations that carry modifiers, and the modifiers that each one
-- accepts.
module Semantic.Modifiers (
    Declaration(..),
    acceptedModifiers,
    declarationName
) where

import Core.AST (Identifier)

data Declaration =
    TaskDeclaration
    | HandlerDeclaration
    | TestInstanceDeclaration
    | ResourceDeclaration
    | PoolDeclaration
    | DequeDeclaration
    | TestPoolDeclaration
    | AtomicDeclaration
    | AtomicArrayDeclaration
    | EmitterDeclaration
    | TestEmitterDeclaration
    | ChannelDeclaration
    | TestChannelDeclaration
    | ConstantDeclaration
    | FunctionDeclaration
    | ExternFunctionDeclaration
    | StructDeclaration
    | EnumDeclaration
    | InterfaceDeclaration
    | ProcedureDeclaration
    | ClassDeclaration
  deriving (Eq, Show)

-- | The modifiers that a declaration accepts, each with whether it takes a
-- value.
acceptedModifiers :: Declaration -> [(Identifier, Bool)]
acceptedModifiers TaskDeclaration = [("priority", True), ("stack_size", True)]
acceptedModifiers ResourceDeclaration = [("unprotected", False)]
acceptedModifiers StructDeclaration = [("packed", False)]
acceptedModifiers _ = []

-- | The declaration as the messages name it.
declarationName :: Declaration -> String
declarationName TaskDeclaration = "a task"
declarationName HandlerDeclaration = "a handler"
declarationName TestInstanceDeclaration = "a test instance"
declarationName ResourceDeclaration = "a resource"
declarationName PoolDeclaration = "a pool"
declarationName DequeDeclaration = "a deque"
declarationName TestPoolDeclaration = "a test pool"
declarationName AtomicDeclaration = "an atomic"
declarationName AtomicArrayDeclaration = "an atomic array"
declarationName EmitterDeclaration = "an emitter"
declarationName TestEmitterDeclaration = "a test emitter"
declarationName ChannelDeclaration = "a channel"
declarationName TestChannelDeclaration = "a test channel"
declarationName ConstantDeclaration = "a constant"
declarationName FunctionDeclaration = "a function"
declarationName ExternFunctionDeclaration = "an extern function"
declarationName StructDeclaration = "a struct"
declarationName EnumDeclaration = "an enum"
declarationName InterfaceDeclaration = "an interface"
declarationName ProcedureDeclaration = "a procedure"
declarationName ClassDeclaration = "a class"
