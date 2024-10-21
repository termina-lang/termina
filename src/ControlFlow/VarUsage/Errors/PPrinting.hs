{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ControlFlow.VarUsage.Errors.PPrinting where

import ControlFlow.VarUsage.Errors.Errors

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Parsec.Pos
import qualified Data.Map as M
import Utils.Annotations
import Utils.Errors
import ControlFlow.VarUsage.Types

ppError :: M.Map FilePath TL.Text ->
    VarUsageError -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position startPos _endPos)) =
  let fileName = sourceName startPos
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    EUsedIgnoredParameter ident ->
        let title = "\x1b[31merror [VE-001]\x1b[0m: Using a variable that is ignored"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is ignored and should not be used."))
    ENotUsed ident ->
        let title = "\x1b[31merror [VE-002]\x1b[0m: Variable is not used"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is declared but not used."))
    EBoxNotMoved ident ->
        let title = "\x1b[31merror [VE-003]\x1b[0m: Box variable is not moved"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is declared but not moved."))
    EBoxMovedTwice ident prevMove@(Position moveStart _moveEnd) ->
        let title = "\x1b[31merror [VE-004]\x1b[0m: Box variable is moved twice"
            -- | We can safely assume that the previous move is in the same file
            moveFileName = sourceName moveStart
            moveSourceLines = toModuleAST M.! moveFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is moved twice.")) >>
            printSimpleError
                moveSourceLines "The previous move was done here:" moveFileName
                prevMove Nothing
    EOptionBoxMovedTwice ident prevMove@(Position moveStart _moveEnd) ->
        let title = "\x1b[31merror [VE-005]\x1b[0m: Option-box variable is moved twice"
            -- | We can safely assume that the previous move is in the same file
            moveFileName = sourceName moveStart
            moveSourceLines = toModuleAST M.! moveFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is moved twice.")) >>
            printSimpleError
                moveSourceLines "The previous move was done here:" moveFileName
                prevMove Nothing
    EDifferentOptionBoxUse ident rval (lval, otherPos@(Position otherStart _otherEnd)) ->
        let title = "\x1b[31merror [AE-006]\x1b[0m: Option-box final state mismatch"
            -- | We can safely assume that the other position is in the same file
            otherFileName = sourceName otherStart
            otherSourceLines = toModuleAST M.! otherFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("On this branch, the variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has been \x1b[31m" <> showText rval <> "\x1b[0m.\n" <>
                    "The final state of the option-box variables in all branches must be the same.")) >>
            printSimpleError
                otherSourceLines ("However, in this branch, the option-box \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has been \x1b[31m" <> showText lval <> "\x1b[0m:") otherFileName
                otherPos Nothing
    EDifferentNewOptionBoxUse ident rval ->
        let title = "\x1b[31merror [AE-007]\x1b[0m: Option-box used in conditional branch or loop"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has been \x1b[31m" <> showText rval <>
                    "\x1b[0m in a branch that may not be executed or inside a loop.\n" <> 
                    "This shall cause the final state to be inconsistent."))
    EMissingOptionBox ident prevVal ->
        let title = "\x1b[31merror [AE-008]\x1b[0m: Option-box unused in a branch but used previously"
            -- | We can safely assume that the other position is in the same file
            otherFileName = case getLocation prevVal of
                Position otherStart _otherEnd -> sourceName otherStart
                _ -> error "EMissingUsedOptionBox: Location is not a position"
            otherSourceLines = toModuleAST M.! otherFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is not used on this branch.\n" <>
                    "The final state of the option-box variables must be the same so that the resulting state is consistent.")) >>
            printSimpleError
                otherSourceLines ("However, in this previous branch, option-box \x1b[31m" <> T.pack ident <>
                    "\x1b[0m was \x1b[31m" <> showText prevVal <> "\x1b[0m:") otherFileName
                (getLocation prevVal) Nothing
    EMissingBoxMove ident otherMove@(Position otherStart _othersEnd) ->
        let title = "\x1b[31merror [AE-009]\x1b[0m: Box variable is not always moved"
            otherFileName = sourceName otherStart
            otherSourceLines = toModuleAST M.! otherFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Box variable \x1b[31m" <> T.pack ident <> "\x1b[0m is not moved on this branch.\n"
                <> "The same box variables must be moved in all branches so that the resulting state is consistent.")) >>
            printSimpleError
                otherSourceLines ("However, in this branch, the box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has been moved:") otherFileName
                otherMove Nothing
    EBoxMoveConditionalBranch ident ->
        let title = "\x1b[31merror [AE-010]\x1b[0m: Box variable moved in conditional branch"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is moved in a branch that may not be executed."))
    EAllocNotMoved ident ->
        let title = "\x1b[31merror [AE-011]\x1b[0m: Option-box allocated but not moved"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is allocated but not moved."))
    EAllocTwice ident prevAlloc@(Position allocStart _allocEnd) ->
        let title = "\x1b[31merror [AE-012]\x1b[0m: Option-box allocated twice"
            allocFileName = sourceName allocStart
            allocSourceLines = toModuleAST M.! allocFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is allocated twice.")) >>
            printSimpleError
                allocSourceLines "The previous allocation was done here:" allocFileName
                prevAlloc Nothing
    EMovedWithoutAlloc ident prevMove@(Position moveStart _moveEnd) ->
        let title = "\x1b[31merror [AE-013]\x1b[0m: Option-box moved but not allocated"
            moveFileName = sourceName moveStart
            moveSourceLines = toModuleAST M.! moveFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is moved but not allocated.")) >>
            printSimpleError
                moveSourceLines "The variable was moved here:" moveFileName
                prevMove Nothing
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
