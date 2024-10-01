module ControlFlow.BasicBlocks.Checks where

import ControlFlow.BasicBlocks.Types
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Control.Monad.State
import Control.Monad.Except
import Utils.Annotations
import ControlFlow.BasicBlocks.Errors.Errors
import Data.Foldable

doesBlockExit :: BasicBlock SemanticAnn -> Bool
doesBlockExit (ReturnBlock {}) = True
doesBlockExit (ContinueBlock {}) = True
doesBlockExit (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) =
    let
        ifBlocksExit = doesBlockExit (last ifBlocks)
    in
        (ifBlocksExit &&
            (let elseIfsExit = foldl' (\prevState (ElseIf _ elifBlocks _) ->
                    prevState && doesBlockExit (last elifBlocks)) ifBlocksExit elseIfBlocks in
                (elseIfsExit && doesBlockExit (last elseBlocks))))
doesBlockExit (MatchBlock _ cases _) =
    foldl'
        (\prevState (MatchCase _ _ blocks _) ->
            prevState && doesBlockExit (last blocks)) True cases
doesBlockExit _ = False

checkBlockExit :: Location -> [BasicBlock SemanticAnn] -> BBExitCheck BBExitCheckST
checkBlockExit loc stmts = do
    step <- get
    case step of
        -- | If we are here, it means that the block must exit on this step.
        BBMustExit ->
            case stmts of
                -- | If the block list is empty and it must exit, then a return
                -- statement is missing.
                [] -> throwError $ annotateError loc BBBlockShallExit
                -- | If the last block is a return, then we must check that the
                -- rest of the blocks
                (ReturnBlock {} : xb) ->
                    -- | The rest of the blocks shall not exit
                    setExitNotAllowed >> checkBlockExit loc xb
                _ -> throwError $ annotateError loc BBBlockShallExit
        -- | If we are here, it means that the block is not allowed to exit.
        BBExitNotAllowed ->
            case stmts of
                [] -> return step
                (ReturnBlock _ ann: _) -> throwError $ annotateError (location ann) BBInvalidReturn
                (SendMessage _ _ ann : _) -> throwError $ annotateError (location ann) BBInvalidSend
                (ContinueBlock _ ann : _) -> throwError $ annotateError (location ann) BBInvalidContinue
                (_ : xb) -> checkBlockExit loc xb
        _ -> throwError $ annotateError Internal BBInvalidCheckState

checkActionExit :: Location -> [BasicBlock SemanticAnn] -> BBExitCheck BBExitCheckST
checkActionExit loc stmts = do
    step <- get
    case step of
        -- | If we are here, it means that the block must exit on this step.
        BBMustExit ->
            case stmts of
                -- | If the block list is empty and it must exit, then a return
                -- statement is missing.
                [] -> throwError $ annotateError loc BBBlockShallExit
                (x : xb) ->
                    case x of
                        ReturnBlock {} ->
                            -- | The rest of the blocks shall not exit
                            setPartialExit >> checkActionExit loc xb
                        ContinueBlock {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionExit loc xb
                        blk@(IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) ann) ->
                            if not (doesBlockExit blk) then
                                throwError $ annotateError (location ann) BBIfBlockShallExit
                            else do
                                stateIfBlock <- localScope (checkActionExit loc (reverse ifBlocks))
                                stateElseIfs <- foldM
                                    (\prevState (ElseIf _ elifBlocks ann') -> do
                                        elseIfState <- localScope (checkActionExit (location ann') (reverse elifBlocks))
                                        return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                                elseBlockState <- localScope (checkActionExit loc (reverse elseBlocks))
                                put (max elseBlockState stateElseIfs)
                                checkActionExit loc xb
                        IfElseBlock _ _ _ _ ann ->
                            throwError $ annotateError (location ann) BBBlockIfBlockMissingElseExit
                        blk@(MatchBlock _ cases ann) ->
                            if not (doesBlockExit blk) then
                                throwError $ annotateError (location ann) BBMatchBlockShallExit
                            else do
                                matchCaseState <- foldM
                                    (\prevState (MatchCase _ _ blocks ann') -> do
                                        matchCaseState <- localScope (checkActionExit (location ann') (reverse blocks))
                                        return (max matchCaseState prevState)) step cases
                                put matchCaseState
                                checkActionExit loc xb
                        _ -> throwError $ annotateError loc BBBlockShallExit
        -- | If we are here, it means that we may exit the block or send messages BUT there
        -- must be a path that does not exit the block
        BBPartialExit ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) BBInvalidReturn
                        ContinueBlock _ ann -> throwError $ annotateError (location ann) BBInvalidContinue
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionExit loc xb
                        blk@(IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            if doesBlockExit blk then
                                throwError $ annotateError loc BBIfBlockShallNotExit
                            else do
                                stateIfBlock <- localScope (setAllowedContinue >> checkActionExit loc (reverse ifBlocks))
                                stateElseIfs <- foldM
                                    (\prevState (ElseIf _ elifBlocks ann') -> do
                                        elseIfState <- localScope (setAllowedContinue >> checkActionExit (location ann') (reverse elifBlocks))
                                        return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                                elseBlockState <- localScope (setAllowedContinue >> checkActionExit loc (reverse elseBlocks))
                                put (max elseBlockState stateElseIfs)
                                checkActionExit loc xb
                        IfElseBlock _ ifBlocks _ _ _ ->
                            checkActionExit loc (reverse ifBlocks) >>
                            checkActionExit loc xb
                        blk@(MatchBlock _ cases _) ->
                            if doesBlockExit blk then
                                throwError $ annotateError loc BBMatchBlockShallNotExit
                            else do
                                matchCaseState <- foldM
                                    (\prevState (MatchCase _ _ blocks ann) -> do
                                        matchCaseState <- localScope (setAllowedContinue >> checkActionExit (location ann) (reverse blocks))
                                        return (max matchCaseState prevState)) step cases
                                put matchCaseState
                                checkActionExit loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionExit (location ann) (reverse loopBlocks) >> checkActionExit loc xb
                        _ -> setExitNotAllowed >> checkActionExit loc xb
        BBAllowedContinue ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) BBInvalidReturn
                        ContinueBlock {} -> 
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionExit loc xb
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionExit loc xb
                        (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            stateIfBlock <- localScope (checkActionExit loc (reverse ifBlocks))
                            stateElseIfs <- foldM
                                (\prevState (ElseIf _ elifBlocks ann') -> do
                                    elseIfState <- localScope (checkActionExit (location ann') (reverse elifBlocks))
                                    return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                            elseBlockState <- localScope (checkActionExit loc (reverse elseBlocks))
                            put (max elseBlockState stateElseIfs)
                            checkActionExit loc xb
                        (IfElseBlock _ ifBlocks _ _ _) ->
                            checkActionExit loc (reverse ifBlocks) >>
                            checkActionExit loc xb
                        (MatchBlock _ cases _) -> do
                            matchCaseState <- foldM
                                (\prevState (MatchCase _ _ blocks ann) -> do
                                    matchCaseState <- localScope (checkActionExit (location ann) (reverse blocks))
                                    return (max matchCaseState prevState)) step cases
                            put matchCaseState
                            checkActionExit loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionExit (location ann) (reverse loopBlocks) >> checkActionExit loc xb
                        _ -> setExitNotAllowed >> checkActionExit loc xb
        BBAllowedSend ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) BBInvalidReturn
                        ContinueBlock _ ann -> throwError $ annotateError (location ann) BBInvalidContinue
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            checkActionExit loc xb
                        (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            stateIfBlock <- localScope (checkActionExit loc (reverse ifBlocks))
                            stateElseIfs <- foldM
                                (\prevState (ElseIf _ elifBlocks ann') -> do
                                    elseIfState <- localScope (checkActionExit (location ann') (reverse elifBlocks))
                                    return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                            elseBlockState <- localScope (checkActionExit loc (reverse elseBlocks))
                            put (max elseBlockState stateElseIfs)
                            checkActionExit loc xb
                        (IfElseBlock _ ifBlocks _ _ _) ->
                            checkActionExit loc (reverse ifBlocks) >>
                            checkActionExit loc xb
                        (MatchBlock _ cases _) -> do
                            matchCaseState <- foldM
                                (\prevState (MatchCase _ _ blocks ann) -> do
                                    matchCaseState <- localScope (checkActionExit (location ann) (reverse blocks))
                                    return (max matchCaseState prevState)) step cases
                            put matchCaseState
                            checkActionExit loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionExit (location ann) (reverse loopBlocks) >> checkActionExit loc xb
                        _ ->
                            setExitNotAllowed >> checkActionExit loc xb
        -- | If we are here, it means that the block is not allowed to exit nor to send messages
        BBExitNotAllowed ->
            case stmts of
                [] -> return step
                (ReturnBlock _ ann: _) -> throwError $ annotateError (location ann) BBInvalidReturn
                (SendMessage _ _ ann : _) -> throwError $ annotateError (location ann) BBInvalidSend
                (ContinueBlock _ ann : _) -> throwError $ annotateError (location ann) BBInvalidContinue
                (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _ : xb) -> 
                    checkActionExit loc (reverse ifBlocks) >> 
                    mapM_
                        (\(ElseIf _ elifBlocks ann') -> 
                            checkActionExit (location ann') (reverse elifBlocks)) elseIfBlocks >>
                    checkActionExit loc (reverse elseBlocks) >>
                    checkActionExit loc xb
                (IfElseBlock _ ifBlocks _ _ _ : xb) ->
                    checkActionExit loc (reverse ifBlocks) >>
                    checkActionExit loc xb
                (MatchBlock _ cases _ : xb) ->
                    mapM_
                        (\(MatchCase _ _ blocks ann) -> 
                            checkActionExit (location ann) (reverse blocks)) cases >>
                    checkActionExit loc xb
                (ForLoopBlock _ _ _ _ _ loopBlocks ann : xb) ->
                    checkActionExit (location ann) (reverse loopBlocks) >> checkActionExit loc xb
                (_ : xb) -> checkActionExit loc xb