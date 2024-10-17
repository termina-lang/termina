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
    EBoxMovedTwice ident prevMove@(Position moveStart _moveEnd) ->
        let title = "\x1b[31merror [AE-002]\x1b[0m: Duplicated channel connection"
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
        let title = "\x1b[31merror [AE-003]\x1b[0m: Mismatched box source"
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
    EDifferentOptionBoxUse ident otherPos@(Position otherStart _otherEnd) ->
        let title = "\x1b[31merror [AE-004]\x1b[0m: Option-box final state mismatch"
            -- | We can safely assume that the other position is in the same file
            otherFileName = sourceName otherStart
            otherSourceLines = toModuleAST M.! otherFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Option-box variable \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has a different final state in this branch.")) >>
            printSimpleError
                otherSourceLines "On another branch the option-box was used differently:" otherFileName
                otherPos Nothing

    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
