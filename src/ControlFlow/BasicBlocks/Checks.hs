module ControlFlow.BasicBlocks.Checks where

import ControlFlow.BasicBlocks.Types
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Control.Monad.State
import Control.Monad.Except
import Utils.Annotations
import ControlFlow.BasicBlocks.Errors.Errors
import Data.Foldable
import qualified Control.Monad.State as ST

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

checkBlockPaths :: Location -> [BasicBlock SemanticAnn] -> BBPathsCheck BBPathsCheckST
checkBlockPaths loc stmts = do
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
                    setExitNotAllowed >> checkBlockPaths loc xb
                _ -> throwError $ annotateError loc BBBlockShallExit
        -- | If we are here, it means that the block is not allowed to exit.
        BBExitNotAllowed ->
            case stmts of
                [] -> return step
                (ReturnBlock _ ann: _) -> throwError $ annotateError (location ann) BBInvalidReturn
                (SendMessage _ _ ann : _) -> throwError $ annotateError (location ann) BBInvalidSend
                (ContinueBlock _ ann : _) -> throwError $ annotateError (location ann) BBInvalidContinue
                (_ : xb) -> checkBlockPaths loc xb
        _ -> throwError $ annotateError Internal BBInvalidCheckState

checkActionPaths :: Location -> [BasicBlock SemanticAnn] -> BBPathsCheck BBPathsCheckST
checkActionPaths loc stmts = do
    step <- get
    case step of
        -- | If we are here, it means that the block must exit on this step.
        BBMustExit ->
            case stmts of
                -- | If the block list is empty and it must exit, then a return
                -- statement is missing.
                [] -> throwError $ annotateError loc BBActionShallExit
                (x : xb) ->
                    case x of
                        ReturnBlock {} ->
                            -- | The rest of the blocks shall not exit
                            setPartialExit >> checkActionPaths loc xb
                        ContinueBlock {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        blk@(IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) ann) ->
                            if not (doesBlockExit blk) then
                                throwError $ annotateError (location ann) BBActionIfBlockShallExit
                            else do
                                stateIfBlock <- localScope (checkActionPaths loc (reverse ifBlocks))
                                stateElseIfs <- foldM
                                    (\prevState (ElseIf _ elifBlocks ann') -> do
                                        elseIfState <- localScope (checkActionPaths (location ann') (reverse elifBlocks))
                                        return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                                elseBlockState <- localScope (checkActionPaths loc (reverse elseBlocks))
                                put (max elseBlockState stateElseIfs)
                                checkActionPaths loc xb
                        IfElseBlock _ _ _ _ ann ->
                            throwError $ annotateError (location ann) BBActionIfBlockMissingElseExit
                        blk@(MatchBlock _ cases ann) ->
                            if not (doesBlockExit blk) then
                                throwError $ annotateError (location ann) BBActionMatchBlockShallExit
                            else do
                                matchCaseState <- foldM
                                    (\prevState (MatchCase _ _ blocks ann') -> do
                                        matchCaseState <- localScope (checkActionPaths (location ann') (reverse blocks))
                                        return (max matchCaseState prevState)) step cases
                                put matchCaseState
                                checkActionPaths loc xb
                        _ -> throwError $ annotateError loc BBActionShallExit
        -- | If we are here, it means that we may exit the block or send messages BUT there
        -- must be a path that does not exit the block
        BBPartialExit ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) BBInvalidReturn
                        ContinueBlock _ ann -> throwError $ annotateError (location ann) BBActionInvalidContinue
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        blk@(IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            if doesBlockExit blk then
                                throwError $ annotateError loc BBActionIfBlockShallNotExit
                            else do
                                stateIfBlock <- localScope (setAllowedContinue >> checkActionPaths loc (reverse ifBlocks))
                                stateElseIfs <- foldM
                                    (\prevState (ElseIf _ elifBlocks ann') -> do
                                        elseIfState <- localScope (setAllowedContinue >> checkActionPaths (location ann') (reverse elifBlocks))
                                        return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                                elseBlockState <- localScope (setAllowedContinue >> checkActionPaths loc (reverse elseBlocks))
                                put (max elseBlockState stateElseIfs)
                                checkActionPaths loc xb
                        IfElseBlock _ ifBlocks _ _ _ ->
                            checkActionPaths loc (reverse ifBlocks) >>
                            checkActionPaths loc xb
                        blk@(MatchBlock _ cases _) ->
                            if doesBlockExit blk then
                                throwError $ annotateError loc BBActionMatchBlockShallNotExit
                            else do
                                matchCaseState <- foldM
                                    (\prevState (MatchCase _ _ blocks ann) -> do
                                        matchCaseState <- localScope (setAllowedContinue >> checkActionPaths (location ann) (reverse blocks))
                                        return (max matchCaseState prevState)) step cases
                                put matchCaseState
                                checkActionPaths loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionPaths (location ann) (reverse loopBlocks) >> checkActionPaths loc xb
                        _ -> setExitNotAllowed >> checkActionPaths loc xb
        BBAllowedContinue ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) BBInvalidReturn
                        ContinueBlock {} -> 
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            setAllowedSend >> checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            stateIfBlock <- localScope (checkActionPaths loc (reverse ifBlocks))
                            stateElseIfs <- foldM
                                (\prevState (ElseIf _ elifBlocks ann') -> do
                                    elseIfState <- localScope (checkActionPaths (location ann') (reverse elifBlocks))
                                    return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                            elseBlockState <- localScope (checkActionPaths loc (reverse elseBlocks))
                            put (max elseBlockState stateElseIfs)
                            checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks _ _ _) ->
                            checkActionPaths loc (reverse ifBlocks) >>
                            checkActionPaths loc xb
                        (MatchBlock _ cases _) -> do
                            matchCaseState <- foldM
                                (\prevState (MatchCase _ _ blocks ann) -> do
                                    matchCaseState <- localScope (checkActionPaths (location ann) (reverse blocks))
                                    return (max matchCaseState prevState)) step cases
                            put matchCaseState
                            checkActionPaths loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionPaths (location ann) (reverse loopBlocks) >> checkActionPaths loc xb
                        _ -> setExitNotAllowed >> checkActionPaths loc xb
        BBAllowedSend ->
            case stmts of
                [] -> return step
                (x : xb) ->
                    case x of
                        ReturnBlock _ ann -> throwError $ annotateError (location ann) BBInvalidReturn
                        ContinueBlock _ ann -> throwError $ annotateError (location ann) BBActionInvalidContinue
                        SendMessage {} ->
                            -- | The rest of the blocks shall not exit
                            checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _) -> do
                            stateIfBlock <- localScope (checkActionPaths loc (reverse ifBlocks))
                            stateElseIfs <- foldM
                                (\prevState (ElseIf _ elifBlocks ann') -> do
                                    elseIfState <- localScope (checkActionPaths (location ann') (reverse elifBlocks))
                                    return (max elseIfState prevState)) stateIfBlock elseIfBlocks
                            elseBlockState <- localScope (checkActionPaths loc (reverse elseBlocks))
                            put (max elseBlockState stateElseIfs)
                            checkActionPaths loc xb
                        (IfElseBlock _ ifBlocks _ _ _) ->
                            checkActionPaths loc (reverse ifBlocks) >>
                            checkActionPaths loc xb
                        (MatchBlock _ cases _) -> do
                            matchCaseState <- foldM
                                (\prevState (MatchCase _ _ blocks ann) -> do
                                    matchCaseState <- localScope (checkActionPaths (location ann) (reverse blocks))
                                    return (max matchCaseState prevState)) step cases
                            put matchCaseState
                            checkActionPaths loc xb
                        (ForLoopBlock _ _ _ _ _ loopBlocks ann) ->
                            setExitNotAllowed >> checkActionPaths (location ann) (reverse loopBlocks) >> checkActionPaths loc xb
                        _ ->
                            setExitNotAllowed >> checkActionPaths loc xb
        -- | If we are here, it means that the block is not allowed to exit nor to send messages
        BBExitNotAllowed ->
            case stmts of
                [] -> return step
                (ReturnBlock _ ann: _) -> throwError $ annotateError (location ann) BBInvalidReturn
                (SendMessage _ _ ann : _) -> throwError $ annotateError (location ann) BBActionInvalidSend
                (ContinueBlock _ ann : _) -> throwError $ annotateError (location ann) BBActionInvalidContinue
                (IfElseBlock _ ifBlocks elseIfBlocks (Just elseBlocks) _ : xb) -> 
                    checkActionPaths loc (reverse ifBlocks) >> 
                    mapM_
                        (\(ElseIf _ elifBlocks ann') -> 
                            checkActionPaths (location ann') (reverse elifBlocks)) elseIfBlocks >>
                    checkActionPaths loc (reverse elseBlocks) >>
                    checkActionPaths loc xb
                (IfElseBlock _ ifBlocks _ _ _ : xb) ->
                    checkActionPaths loc (reverse ifBlocks) >>
                    checkActionPaths loc xb
                (MatchBlock _ cases _ : xb) ->
                    mapM_
                        (\(MatchCase _ _ blocks ann) -> 
                            checkActionPaths (location ann) (reverse blocks)) cases >>
                    checkActionPaths loc xb
                (ForLoopBlock _ _ _ _ _ loopBlocks ann : xb) ->
                    checkActionPaths (location ann) (reverse loopBlocks) >> checkActionPaths loc xb
                (_ : xb) -> checkActionPaths loc xb

checkBBPathClassMember :: ClassMember SemanticAnn -> BBPathsCheck ()
checkBBPathClassMember (ClassField {}) = return ()
checkBBPathClassMember (ClassMethod _name _retType (Block body) ann) = 
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkBBPathClassMember (ClassProcedure _name _args (Block body) ann) = 
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkBBPathClassMember (ClassViewer _name _args _retType (Block body) ann) =
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkBBPathClassMember (ClassAction _name _param _retType (Block body) ann) = 
    void $ setMustExit >> checkActionPaths (location ann) (reverse body)

checkBBPathTypeDef :: TypeDef SemanticAnn -> BBPathsCheck ()
checkBBPathTypeDef (Struct {}) = return ()
checkBBPathTypeDef (Enum {}) = return ()
checkBBPathTypeDef (Class _kind _name members _parents _ann) = 
    mapM_ checkBBPathClassMember members
checkBBPathTypeDef (Interface {}) = return ()

checkBBPathAnnASTElement :: AnnASTElement SemanticAnn -> BBPathsCheck ()
checkBBPathAnnASTElement (Function _name _args _retType (Block body) _modifiers ann) = 
    void $ setMustExit >> checkBlockPaths (location ann) (reverse body)
checkBBPathAnnASTElement (GlobalDeclaration {}) = return ()
checkBBPathAnnASTElement (TypeDefinition typeDef _ann) =
    checkBBPathTypeDef typeDef

checkBBPathModule :: AnnotatedProgram SemanticAnn -> BBPathsCheck ()
checkBBPathModule = mapM_ checkBBPathAnnASTElement

runCheckBBPaths :: AnnotatedProgram SemanticAnn -> Either BBPathsCheckError ()
runCheckBBPaths prog = case ST.runState (runExceptT . checkBBPathModule $ prog) BBMustExit of
    (Left err, _) -> Left err
    (Right _, _) -> Right ()