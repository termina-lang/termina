-- | The instances of a deployment are declared in its main module, app.fin or test.fin
module ControlFlow.Architecture.Checks.Instances (runCheckInstanceModules) where

import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Errors
import Semantic.Types
import Utils.Annotations

import Control.Monad (when)
import qualified Data.Map.Strict as M

-- | The main module is the last one in the order of dependencies, since no module imports it
runCheckInstanceModules :: QualifiedName -> TerminaProgArch SemanticAnn -> Either ArchitectureError ()
runCheckInstanceModules mainModule tp = mapM_ check instances

  where

    check (ident, modName, ann) =
      when (modName /= mainModule) $
        Left (annotateError (getLocation ann) (EInstanceOutsideMainModule ident modName mainModule))

    -- | The elements of the system and of the platform have no module
    instances =
      [ (taskName t, taskModule t, getAnnotation t) | t <- M.elems (tasks tp) ]
      ++ [ (handlerName h, handlerModule h, getAnnotation h) | h <- M.elems (handlers tp) ]
      ++ [ (resourceName r, m, getAnnotation r) | r <- M.elems (resources tp), Just m <- [resourceModule r] ]
      ++ [ (e, m, a) | TPPeriodicTimerEmitter e _ m a <- M.elems (emitters tp) ]
      ++ [ (c, m, a) | TPMsgQueue c _ _ m a <- M.elems (channels tp) ]
      ++ [ (p, m, a) | TPPool p _ _ m a <- M.elems (pools tp) ]
      ++ [ (i, m, a) | TPAtomic i _ m a <- M.elems (atomics tp) ]
      ++ [ (i, m, a) | TPAtomicArray i _ _ m a <- M.elems (atomicArrays tp) ]
      ++ [ (d, m, a) | TPDeque d _ _ m a <- M.elems (deques tp) ]
      ++ [ (testInstanceName t, testInstanceModule t, getAnnotation t) | t <- M.elems (testInstances tp) ]
      ++ [ (e, m, a) | TPTestEmitter e _ m a <- M.elems (testEmitters tp) ]
      ++ [ (c, m, a) | TPMsgQueue c _ _ m a <- M.elems (testChannels tp) ]
      ++ [ (p, m, a) | TPPool p _ _ m a <- M.elems (testPools tp) ]
