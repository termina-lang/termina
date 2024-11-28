-- | Semantic Error Printing
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Command.Errors.PPrinting where

import Command.Errors.Errors

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Parsec.Pos
import qualified Data.Map as M
import Utils.Annotations
    ( AnnotatedError(AnnotatedError), Location(Position) )
import Utils.Errors

ppError :: M.Map FilePath TL.Text ->
    CommandErrors -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position start _end)) =
  let fileName = sourceName start
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    (EImportedFileNotFound qname) -> 
        let title = "\x1b[31merror [CE-001]\x1b[0m: imported file not found"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Imported file invalid or not found: " <> T.pack (show qname)))
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e