{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ControlFlow.Architecture.Errors.PPrinting where

import ControlFlow.Architecture.Errors.Errors

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Parsec.Pos
import qualified Data.Map as M
import Utils.Annotations
import Utils.Errors

ppError :: M.Map FilePath TL.Text ->
    ArchitectureError -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position startPos _endPos)) =
  let fileName = sourceName startPos
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    EDuplicatedEmitterConnection emitter procPos@(Position procStart _procEnd) ->
        let title = "\x1b[31merror [AE-001]\x1b[0m: Duplicated emitter connection"
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Emitter \x1b[31m" <> T.pack emitter <>
                    "\x1b[0m is already connected to an sink port.")) >>
            printSimpleError
                procSourceLines "The previous connection was done here:" procFileName
                procPos Nothing
    EDuplicatedChannelConnection channel procPos@(Position procStart _procEnd) ->
        let title = "\x1b[31merror [AE-002]\x1b[0m: Duplicated channel connection"
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Channel \x1b[31m" <> T.pack channel <>
                    "\x1b[0m is already connected to an input port.")) >>
            printSimpleError
                procSourceLines "The previous connection was done here:" procFileName
                procPos Nothing
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
