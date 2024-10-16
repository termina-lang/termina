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
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
