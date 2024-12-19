{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.Architecture.Errors (
    ArchitectureError, Error(..)
) where

import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import Utils.Errors
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

data Error = 
    EUnboxingObject -- ^ Error when trying to unbox an object (Internal)
    | EUnboxingExpression -- ^ Error when trying to unbox an expression (Internal)
    | EUnboxingPort -- ^ Error when trying to unbox a port (Internal)
    | EUnboxingBox -- ^ Error when trying to unbox a box (Internal)
    | EUnboxingOptionBox -- ^ Error when trying to unbox an option box (Internal)
    | EUnboxingMatchCase -- ^ Error when trying to unbox a match case (Internal)
    | EUnboxingClassField -- ^ Error when trying to unbox a class field (Internal)
    | EUnboxingChannel -- ^ Error when trying to unbox a channel (Internal)
    | EUnboxingTask -- ^ Error when trying to unbox a task (Internal)
    | EUnboxingHandler -- ^ Error when trying to unbox a handler (Internal)
    | EUnboxingResource -- ^ Error when trying to unbox a resource (Internal)
    | EUnboxingPool -- ^ Error when trying to unbox a pool (Internal)
    | EUnboxingProcedureCall -- ^ Error when trying to unbox a procedure call (Internal)
    | EUnsupportedEmitterClass -- ^ Unsupported emitter class (Internal)
    | EUnboxingFree -- ^ Error when trying to unbox a free (Internal)
    | EDuplicatedEmitterConnection Identifier Location -- ^ Duplicated emitter connection (AE-001)
    | EDuplicatedChannelConnection Identifier Location -- ^ Duplicated channel connection (AE-002)
    | EMismatchedBoxSource Identifier Identifier [Location] -- ^ Mismatched box source (AE-003)
    | EDisconnectedEmitter Identifier -- ^ Disconnected emitter (AE-004)
    | EChannelWithoutSources Identifier -- ^ Channel without sources (AE-005)
    | EChannelWithoutTarget Identifier -- ^ Channel without target (AE-006)
    | EUnusedResource Identifier -- ^ Unused resource (AE-007)
    | EUnusedPool Identifier -- ^ Unused pool (AE-008)
    deriving Show

type ArchitectureError = AnnotatedError Error Location

instance ErrorMessage ArchitectureError where

    errorIdent (AnnotatedError (EDuplicatedEmitterConnection _ident _prev) _pos) = "AE-001"
    errorIdent (AnnotatedError e _pos) = T.pack $ show e

    errorTitle (AnnotatedError (EDuplicatedEmitterConnection _ident _prev) _pos) = "duplicated emitter connection"
    errorTitle _ = "Unknown"

    toText e@(AnnotatedError (EDuplicatedEmitterConnection emitter procPos@(Position procStart _procEnd)) pos@(Position start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
            procFileName = sourceName procStart
            procSourceLines = files M.! procFileName
        in
            pprintSimpleError
                sourceLines title fileName pos
                (Just ("Emitter \x1b[31m" <> T.pack emitter <>
                    "\x1b[0m is already connected to a sink port. " <>
                    "Only one target is allowed per event source.")) <>
            pprintSimpleError
                procSourceLines "The previous connection was done here:" procFileName
                procPos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostic e@(AnnotatedError (EDuplicatedEmitterConnection _ident _prev) pos) _files =
        LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        
        where 
            text = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
    toDiagnostic _ _files = 
        LSP.Diagnostic 
            emptyRange
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        where 
            text = T.pack "\x1b[31mUknown\x1b[0m."