module DataFlow.Program where

import DataFlow.Program.Types
import Control.Monad.Except
import DataFlow.Program.Errors.Errors
import qualified Control.Monad.State.Strict as ST
import AST.Seman
import Semantic.Monad
import qualified Data.Map as M


type ProgramMonad = ExceptT ProgramErrors (ST.State TerminaProgram)

typeDefinitionCheck :: TypeDef SemanticAnns -> ProgramMonad ()
typeDefinitionCheck (Class TaskClass _ _ _ _) = return ()
typeDefinitionCheck (Class EmitterClass _ _ _ _) = return ()
typeDefinitionCheck (Class HandlerClass _ _ _ _) = return ()
typeDefinitionCheck (Class ResourceClass _ _ _ _) = return ()
typeDefinitionCheck _ = return ()

globalCheck :: Global SemanticAnns -> ProgramMonad ()
globalCheck (Const {}) = return ()
globalCheck (Emitter ident emitterCls _ _ _) = do
  case emitterCls of
    (DefinedType "Interrupt") -> ST.modify $ \tp -> 
      tp {
        emitters = M.insert ident (TPInterruptEmittter ident) (emitters tp)
      }
    (DefinedType "PeriodicTimer") -> ST.modify $ \tp -> 
      tp {
        emitters = M.insert ident (TPPeriodicTimerEmitter ident) (emitters tp)
       }
    (DefinedType "SystemInit") -> ST.modify $ \tp -> 
      tp {
        emitters = M.insert ident (TPSystemInitEmitter ident) (emitters tp)
      }
    _ -> throwError $ UnsupportedEmitterClass ident
globalCheck (Task ident taskCls initializer _ _) = do
  return ()
globalCheck (Resource ident _ _ _ _) = return ()
globalCheck (Handler ident _ _ _ _) = return ()
globalCheck (Channel ident _ _ _ _) = return ()

elementCheck :: AnnASTElement SemanticAnns -> ProgramMonad ()
elementCheck (Function {}) = return ()
elementCheck (GlobalDeclaration glb) = globalCheck glb
elementCheck (TypeDefinition typeDef _) = typeDefinitionCheck typeDef