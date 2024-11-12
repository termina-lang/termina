module ControlFlow.BasicBlocks.Checks.ExitPaths where

import ControlFlow.BasicBlocks.Checks.ExitPaths.Types
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Utils.Annotations
import ControlFlow.BasicBlocks.Checks.ExitPaths.Errors
import Data.Foldable
import qualified Control.Monad.State as ST
import Utils.Monad

-- | Check if a block exits.
-- A block exits if it contains a return statement or a continue statement or,
-- if it ends with a conditional block (if-else or match) and all the branches
-- exit.
doesBlockExit :: BasicBlock SemanticAnn -> Bool
doesBlockExit (ReturnBlock {}) = True
doesBlockExit (ContinueBlock {}) = True
doesBlockExit (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) =
    let
        -- | Check the exit of the if block
        ifBlocksExit = doesBlockExit (last (blockBody ifBlocks))
    in
        -- | Check the exit of the else if blocks
        -- If the if block exits, then we must check the else if blocks
        -- and the else block (if present) 
        (ifBlocksExit &&
            (let elseIfsExit = foldl' (\prevState (ElseIf _ elifBlocks _) ->
                    prevState && doesBlockExit (last (blockBody elifBlocks))) ifBlocksExit elseIfBlocks in
                (elseIfsExit && doesBlockExit (last (blockBody elseBlocks)))))
doesBlockExit (MatchBlock _ cases _) =
    foldl'
        (\prevState (MatchCase _ _ blocks _) ->
            prevState && doesBlockExit (last (blockBody blocks))) True cases
doesBlockExit _ = False

checkBlockPaths :: Location -> [BasicBlock SemanticAnn] -> BBPathsCheck ExitPathsCheckST
checkBlockPaths loc stmts = do
    step <- get
    case step of
        -- | If we are here, it means that the block must exit on this step.
        EPMustExit ->
            case stmts of
                -- | If the block list is empty and it must exit, then a return
                -- statement is missing.
                [] -> throwError $ annotateError loc EEBlockShallExit
                -- | If the last block is a return, then we must check that the
                -- rest of the blocks
                (ReturnBlock {} : xb) ->
                    -- | The rest of the blocks shall not exit
                    setExitNotAllowed >> checkBlockPaths loc xb
                _ -> throwError $ annotateError loc EEBlockShallExit
        -- | If we are here, it means that the block is not allowed to exit.
        EPExitNotAllowed ->
            case stmts of
                [] -> return step
                (ReturnBlock _ ann: _) -> throwError $ annotateError (location ann) EEInvalidReturn
                (SendMessage _ _ ann : _) -> throwError $ annotateError (location ann) EEInvalidSend
                (ContinueBlock _ ann : _) -> throwError $ annotateError (location ann) EEInvalidContinue
                (_ : xb) -> checkBlockPaths loc xb
        _ -> throwError $ annotateError Internal EEInvalidCheckState

checkActionPaths :: Location -> [BasicBlock SemanticAnn] -> BBPathsCheck ExitPathsCheckST
checkActionPaths loc stmts = do
    step <- get
    case step of
        -- | If we are here, it means that the block must exit on this step.
        EPMustExit ->
            case stmts of
                -- | If the block list is empty and it must exit, then a return
                -- statement is missing.
                [] -> throwError $ annotateError loc EEActionShallExit
                (x : xb) ->
                    case x of
                        ReturnBlock {} ->
                            -- | The rest of the blocks shall only partially
                            -- exit or not exit at all. This means that the must
                            -- be at least one path that does not exit and thus
                            -- reaches the end of the block.
                            setPartialExit >> checkActionPaths loc xb
                        ContinueBlock {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        blk@(IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) ann) ->
                            if not (doesBlockExit blk) then
                                throwError $ annotateError (location ann) EEActionIfBlockShallExit
                            else do
                                stateIfBlock <- localScope (checkActionPaths loc (reverse (blockBody ifBlocks)))
                                stateElseIfs <- foldM
                                    (\prevState (ElseIf _ elifBlocks ann') -> do
                                        elseIfState <- localScope (checkActionPaths (location ann') (reverse (blockBody elifBlocks)))
                                        return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                                elseBlockState <- localScope (checkActionPaths loc (reverse (blockBody elseBlocks)))
                                put (max elseBlockState stateElseIfs)
                                checkActionPaths loc xb
                        IfElseBlock _ _ _ _ ann ->
                            throwError $ annotateError (location ann) EEActionIfBlockMissingElseExit
                        blk@(MatchBlock _ cases ann) ->
                            if not (doesBlockExit blk) then
                                throwError $ annotateError (location ann) EEActionMatchBlockShallExit
                            else do
                                matchCaseState <- foldM
                                    (\prevState (MatchCase _ _ blocks ann') -> do
                                        matchCaseState <- localScope (checkActionPaths (location ann') (reverse (blockBody blocks)))
                                        return (max matchCaseState prevState)) step cases
                                put matchCaseState
                                checkActionPaths loc xb
                        _ -> throwError $ annotateError loc EEActionShallExit
        -- | If we are here, it means that we may exit the block or send messages BUT there
        -- must be a path that does not exit the block
        EPPartialExit ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) EEInvalidReturn
                        ContinueBlock _ ann -> throwError $ annotateError (location ann) EEActionInvalidContinue
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        blk@(IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            if doesBlockExit blk then
                                throwError $ annotateError loc EEActionIfBlockShallNotExit
                            else do
                                stateIfBlock <- localScope (setAllowedContinue >> checkActionPaths loc (reverse (blockBody ifBlocks)))
                                stateElseIfs <- foldM
                                    (\prevState (ElseIf _ elifBlocks ann') -> do
                                        elseIfState <- localScope (setAllowedContinue >> checkActionPaths (location ann') (reverse (blockBody elifBlocks)))
                                        return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                                elseBlockState <- localScope (setAllowedContinue >> checkActionPaths loc (reverse (blockBody elseBlocks)))
                                put (max elseBlockState stateElseIfs)
                                checkActionPaths loc xb
                        IfElseBlock _ ifBlocks _ _ _ ->
                            checkActionPaths loc (reverse (blockBody ifBlocks)) >>
                            checkActionPaths loc xb
                        blk@(MatchBlock _ cases _) ->
                            if doesBlockExit blk then
                                throwError $ annotateError loc EEActionMatchBlockShallNotExit
                            else do
                                matchCaseState <- foldM
                                    (\prevState (MatchCase _ _ blocks ann) -> do
                                        matchCaseState <- localScope (setAllowedContinue >> checkActionPaths (location ann) (reverse (blockBody blocks)))
                                        return (max matchCaseState prevState)) step cases
                                put matchCaseState
                                checkActionPaths loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionPaths (location ann) (reverse (blockBody loopBlocks)) >> checkActionPaths loc xb
                        _ -> setExitNotAllowed >> checkActionPaths loc xb
        EPAllowedContinue ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) EEInvalidReturn
                        ContinueBlock {} -> 
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            stateIfBlock <- localScope (checkActionPaths loc (reverse (blockBody ifBlocks)))
                            stateElseIfs <- foldM
                                (\prevState (ElseIf _ elifBlocks ann') -> do
                                    elseIfState <- localScope (checkActionPaths (location ann') (reverse (blockBody elifBlocks)))
                                    return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                            elseBlockState <- localScope (checkActionPaths loc (reverse (blockBody elseBlocks)))
                            put (max elseBlockState stateElseIfs)
                            checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks _ _ _) ->
                            checkActionPaths loc (reverse (blockBody ifBlocks)) >>
                            checkActionPaths loc xb
                        (MatchBlock _ cases _) -> do
                            matchCaseState <- foldM
                                (\prevState (MatchCase _ _ blocks ann) -> do
                                    matchCaseState <- localScope (checkActionPaths (location ann) (reverse (blockBody blocks)))
                                    return (max matchCaseState prevState)) step cases
                            put matchCaseState
                            checkActionPaths loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionPaths (location ann) (reverse (blockBody loopBlocks)) >> checkActionPaths loc xb
                        _ -> setExitNotAllowed >> checkActionPaths loc xb
        EPAllowedSend ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) EEInvalidReturn
                        ContinueBlock _ ann -> throwError $ annotateError (location ann) EEActionInvalidContinue
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            stateIfBlock <- localScope (checkActionPaths loc (reverse (blockBody ifBlocks)))
                            stateElseIfs <- foldM
                                (\prevState (ElseIf _ elifBlocks ann') -> do
                                    elseIfState <- localScope (checkActionPaths (location ann') (reverse (blockBody elifBlocks)))
                                    return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                            elseBlockState <- localScope (checkActionPaths loc (reverse (blockBody elseBlocks)))
                            put (max elseBlockState stateElseIfs)
                            checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks _ _ _) ->
                            checkActionPaths loc (reverse (blockBody ifBlocks)) >>
                            checkActionPaths loc xb
                        (MatchBlock _ cases _) -> do
                            matchCaseState <- foldM
                                (\prevState (MatchCase _ _ blocks ann) -> do
                                    matchCaseState <- localScope (checkActionPaths (location ann) (reverse (blockBody blocks)))
                                    return (max matchCaseState prevState)) step cases
                            put matchCaseState
                            checkActionPaths loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionPaths (location ann) (reverse (blockBody loopBlocks)) >> checkActionPaths loc xb
                        _ ->
                            setExitNotAllowed >> checkActionPaths loc xb
        -- | If we are here, it means that the block is not allowed to exit nor to send messages
        EPExitNotAllowed ->
            case stmts of
                [] -> return step
                (ReturnBlock _ ann: _) -> throwError $ annotateError (location ann) EEInvalidReturn
                (SendMessage _ _ ann : _) -> throwError $ annotateError (location ann) EEActionInvalidSend
                (ContinueBlock _ ann : _) -> throwError $ annotateError (location ann) EEActionInvalidContinue
                (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _ : xb) -> 
                    checkActionPaths loc (reverse (blockBody ifBlocks)) >> 
                    mapM_
                        (\(ElseIf _ elifBlocks ann') -> 
                            checkActionPaths (location ann') (reverse (blockBody elifBlocks))) elseIfBlocks >>
                    checkActionPaths loc (reverse (blockBody elseBlocks)) >>
                    checkActionPaths loc xb
                (IfElseBlock _ ifBlocks _ _ _ : xb) ->
                    checkActionPaths loc (reverse (blockBody ifBlocks)) >>
                    checkActionPaths loc xb
                (MatchBlock _ cases _ : xb) ->
                    mapM_
                        (\(MatchCase _ _ blocks ann) -> 
                            checkActionPaths (location ann) (reverse (blockBody blocks))) cases >>
                    checkActionPaths loc xb
                (ForLoopBlock _ _ _ _ _ loopBlocks ann : xb) ->
                    checkActionPaths (location ann) (reverse (blockBody loopBlocks)) >> checkActionPaths loc xb
                (_ : xb) -> checkActionPaths loc xb

checkExitPathClassMember :: ClassMember SemanticAnn -> BBPathsCheck ()
checkExitPathClassMember (ClassField {}) = return ()
checkExitPathClassMember (ClassMethod _name _retType (Block body _) ann) = 
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkExitPathClassMember (ClassProcedure _name _args (Block body _) ann) = 
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkExitPathClassMember (ClassViewer _name _args _retType (Block body _) ann) =
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkExitPathClassMember (ClassAction _name _param _retType (Block body _) ann) = 
    void $ setMustExit >> checkActionPaths (location ann) (reverse body)

checkExitPathTypeDef :: TypeDef SemanticAnn -> BBPathsCheck ()
checkExitPathTypeDef (Struct {}) = return ()
checkExitPathTypeDef (Enum {}) = return ()
checkExitPathTypeDef (Class _kind _name members _parents _ann) = 
    mapM_ checkExitPathClassMember members
checkExitPathTypeDef (Interface {}) = return ()

checkExitPathAnnASTElement :: AnnASTElement SemanticAnn -> BBPathsCheck ()
checkExitPathAnnASTElement (Function _name _args _retType (Block body _) _modifiers ann) = 
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkExitPathAnnASTElement (GlobalDeclaration {}) = return ()
checkExitPathAnnASTElement (TypeDefinition typeDef _ann) =
    checkExitPathTypeDef typeDef

checkExitPathModule :: AnnotatedProgram SemanticAnn -> BBPathsCheck ()
checkExitPathModule = mapM_ checkExitPathAnnASTElement

runCheckExitPaths :: AnnotatedProgram SemanticAnn -> Either PathsCheckError ()
runCheckExitPaths prog = case ST.runState (runExceptT . checkExitPathModule $ prog) EPMustExit of
    (Left err, _) -> Left err
    (Right _, _) -> Right ()
