module ControlFlow.Architecture.Forwarding where

import ControlFlow.Architecture.Types

import qualified Control.Monad.State.Strict as ST
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import qualified Data.Map as M
import qualified Data.Set as S
import ControlFlow.Architecture.Utils
import Control.Monad.Except
import ControlFlow.Architecture.Errors

type ActionForwardingMonad = ExceptT ArchitectureError (ST.State ActionForwardingMap)

-- | Adds a new out port to the forwarding map
addFordwardingPort :: Identifier -> Identifier -> ActionForwardingMonad ()
addFordwardingPort from to = do
    currMap <- ST.get
    case M.lookup from currMap of
        Just prevSet -> do
            ST.modify (M.insert from (S.insert to prevSet))
        Nothing -> ST.modify (M.insert from (S.singleton to))

-- | This function is used to search for sending blocks and add the forwarding 
-- information to the map
addForwardingBlock :: 
    Identifier -- ^ Name of the currect action
    -> BasicBlock SemanticAnn -- ^ The block to be analyzed
    -> ActionForwardingMonad ()
-- | We have to add the port to the set of ports through which the action is
-- being forwarded
addForwardingBlock action (SendMessage obj _ _) = do
    outPt <- getPortName obj
    addFordwardingPort action outPt
-- | We may have sending blocks inside the if-else block
addForwardingBlock action (IfElseBlock _ ifBlock elifs elseBlocks _) = do
    mapM_ (addForwardingBlock action) (blockBody ifBlock)
    mapM_ (\(ElseIf _ blocks _) -> mapM_ (addForwardingBlock action) (blockBody blocks)) elifs
    maybe (return ()) (mapM_ (addForwardingBlock action) . blockBody) elseBlocks
-- | We may have sending blocks inside the cases of a match block
addForwardingBlock action (MatchBlock _ cases mDefaultCase _) = do
    mapM_ (\(MatchCase _ _ blocks _) -> mapM_ (addForwardingBlock action) (blockBody blocks)) cases
    case mDefaultCase of
        Just (DefaultCase blocks _) -> mapM_ (addForwardingBlock action) (blockBody blocks)
        Nothing -> return ()
-- | We don't have to do anything for the rest of the blocks
addForwardingBlock _ _ = return ()

addFordwardingAction :: ClassMember SemanticAnn -> ActionForwardingMonad ()
addFordwardingAction (ClassAction action _ _ (Block blocks _) _) =
    mapM_ (addForwardingBlock action) blocks
addFordwardingAction _ = return ()

getForwardingMap :: [ClassMember SemanticAnn] -> ActionForwardingMonad ()
getForwardingMap = mapM_ addFordwardingAction

emptyForwardingMap :: ActionForwardingMap
emptyForwardingMap = M.empty

runGetForwardingMap :: [ClassMember SemanticAnn] -> Either ArchitectureError ActionForwardingMap
runGetForwardingMap members =
  case flip ST.runState emptyForwardingMap . runExceptT $ getForwardingMap members of
    (Left err, _) -> Left err
    (Right _, st) -> Right st