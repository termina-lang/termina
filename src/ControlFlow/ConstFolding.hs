module ControlFlow.ConstFolding where

import qualified Data.Map as M
import Semantic.Types
import Control.Monad.Except
import ControlFlow.ConstFolding.Errors
import qualified Control.Monad.State as ST
import ControlFlow.Architecture.Types
import Utils.Annotations
import Control.Monad
import Command.Types
import ControlFlow.ConstFolding.Monad
import ControlFlow.ConstFolding.Transaction
import ControlFlow.ConstFolding.Simpl

loadGlobalConstEvironment :: TransFoldMonad ()
loadGlobalConstEvironment = do
  progArchitecture <- ST.gets progArch
  let glbConstants = globalConstants progArchitecture
  -- Load the global constants into the environment
  mapM_ (\(TPGlobalConstant identifier _ expr _) -> do
    constExpr <- evalConstExpression expr
    ST.modify (\s -> s { globalConstEnv = M.insert identifier constExpr (globalConstEnv s) })) (M.elems glbConstants)


runTransFolding ::
  BasicBlocksProject
  -> TerminaProgArch SemanticAnn
  -> Either ConstFoldError BasicBlocksProject
runTransFolding bbProject progArchitecture =
  let env = TransFoldSt
        {
          localConstEnv = M.empty,
          globalConstEnv = M.empty,
          currentElement = Nothing,
          progArch = progArchitecture
        } in
  case flip ST.runState env . runExceptT $
    loadGlobalConstEvironment >>
    mapM transFoldGlobalEnvironment bbProject >>
    mapM_ (\func -> unless (functionHasConstParams func) $
        transFoldFunction func) (M.elems $ functions progArchitecture) >>
    forM_ (tasks progArchitecture) (\task -> do
          taskCls <- case M.lookup (taskClass task) (taskClasses progArchitecture) of
            Just cls -> return cls
            Nothing -> throwError $ annotateError Internal (EUnknownTaskClass (taskClass task))
          switchInputScope (taskName task) $
            forM_ (classMemberFunctions taskCls) (\func -> unless (functionHasConstParams func) $
                transFoldFunction func)) >>
    forM_ (handlers progArchitecture) (\handler -> do
          handlerCls <- case M.lookup (handlerClass handler) (handlerClasses progArchitecture) of
            Just cls -> return cls
            Nothing -> throwError $ annotateError Internal (EUnknownHandlerClass (handlerClass handler))
          switchInputScope (handlerName handler) $
            mapM_ (\func -> unless (functionHasConstParams func) $
                transFoldFunction func) (classMemberFunctions handlerCls)) >>
    forM_ (resources progArchitecture) (\resource -> do
          -- We don't want to fold the system_entry resource
          -- The implementation of the procedures of this reosurce is done
          -- outside Termina
          unless (resourceName resource == "system_entry") $ do
            resourceCls <- case M.lookup (resourceClass resource) (resourceClasses progArchitecture) of
              Just cls -> return cls
              Nothing -> throwError $ annotateError Internal (EUnknownResourceClass (resourceClass resource))
            switchInputScope (resourceName resource) $
              mapM_ (\func -> unless (functionHasConstParams func) $
                  transFoldFunction func) (classMemberFunctions resourceCls)) >>
    mapM evalModuleTypeDefinitions bbProject of
      (Left err, _) -> Left err
      (Right bbProject', _) -> Right bbProject'

runConstSimpl :: BasicBlocksProject -> Either ConstFoldError BasicBlocksProject
runConstSimpl = runExcept . mapM constSimplModule