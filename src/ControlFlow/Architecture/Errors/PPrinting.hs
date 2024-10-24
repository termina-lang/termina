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
import ControlFlow.BasicBlocks.AST

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
                    "\x1b[0m is already connected to a sink port. " <>
                    "Only one target is allowed per event source.")) >>
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
                    "\x1b[0m is already connected to an input port. " <>
                    "Only one target is allowed per channel.")) >>
            printSimpleError
                procSourceLines "The previous connection was done here:" procFileName
                procPos Nothing
    EMismatchedBoxSource expectedSource actualSource boxTrace ->
        let title = "\x1b[31merror [AE-003]\x1b[0m: Mismatched box source" in
        printSimpleError
            sourceLines title fileName pos
            (Just ("Expected allocation from \x1b[31m" <> T.pack expectedSource <>
                "\x1b[0m but the box is being allocated from \x1b[31m" <> T.pack actualSource <>
                "\x1b[0m.")) >>
        printBoxTrace expectedSource (reverse boxTrace)
    _ -> putStrLn $ show pos ++ ": " ++ show e


    where

        -- | Prints a trace of box allocations 
        printBoxTrace :: Identifier -> [TLocation] -> IO ()
        printBoxTrace _ [] = return ()
        printBoxTrace expectedSource [tracePos@(Position traceStartPos _)] =
            let title = "The box is being freed here to allocator \x1b[31m" <> T.pack expectedSource <> "\x1b[0m:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError 
                    traceSourceLines title traceFileName tracePos Nothing
        printBoxTrace expectedSource (tracePos@(Position traceStartPos _) : xr) =
            let title = "The box is first moved here:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError
                    traceSourceLines title traceFileName tracePos Nothing >> printBoxTrace' expectedSource xr
        printBoxTrace _ _ = error "Internal error: invalid error position"

        printBoxTrace' :: Identifier -> [TLocation] -> IO ()
        printBoxTrace' _ [] = return ()
        printBoxTrace' expectedSource [tracePos@(Position traceStartPos _)] =
            let title = "Finally, box is being freed here to allocator \x1b[31m" <> T.pack expectedSource <> "\x1b[0m:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError 
                    traceSourceLines title traceFileName tracePos Nothing
        printBoxTrace' expectedSource (tracePos@(Position traceStartPos _) : xr) =
            let title = "The box is moved again here:"
                traceFileName = sourceName traceStartPos
                traceSourceLines = toModuleAST M.! traceFileName
            in
                printSimpleError
                    traceSourceLines title traceFileName tracePos Nothing >> printBoxTrace' expectedSource xr
        printBoxTrace' _ _ = error "Internal error: invalid error position"
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
