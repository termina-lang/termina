{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module ControlFlow.Architecture.Errors (
    ArchitectureError, Error(..)
) where

import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import Utils.Errors
import Text.Parsec
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

data Error = 
    EInvalidObjectTypeAnnotation -- ^ Error when the semantic annotation of an object does not contain the expected type information (Internal)
    | EInvalidExprTypeAnnotation -- ^ Error when the semantic annotation of an expression does not contain the expected type information (Internal)
    | EExpectedPort -- ^ Error when trying to access a port field (Internal)
    | EInvalidPortAccessExpression -- ^ Error when trying to access a port access expression (Internal)
    | EExpectedBoxSubtype -- ^ Error when trying to access a box-typed variable (Internal)
    | EExpectedOptionBoxType -- ^ Error when trying to  access an option-box variable (Internal)
    | EInvalidMatchCase -- ^ Error when trying to access a match case (Internal)
    | EInvalidChannelConnection -- ^ Invalid channel connection (Internal)
    | EInvalidTaskConnection -- ^ Invalid task connection (Internal)
    | EInvalidHandlerConnection -- ^ Invalid handler connection (Internal)
    | EInvalidResourceConnection -- ^ Invalid resource connection (Internal)
    | EInvalidPoolConnection -- ^ Invalid pool connection (Internal)
    | EUnsupportedEmitterClass -- ^ Unsupported emitter class (Internal)
    | EMissingPeriodicTimerInitializer -- ^ Missing initializer expression for periodic timer emitter (Internal)
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

    errorIdent :: ArchitectureError -> T.Text
    errorIdent (AnnotatedError (EDuplicatedEmitterConnection _ident _prev) _pos) = "AE-001"
    errorIdent (AnnotatedError (EDuplicatedChannelConnection _channel _prev) _pos) = "AE-002"
    errorIdent (AnnotatedError (EMismatchedBoxSource _expectedSource _actualSource _boxTrace) _pos) = "AE-003"
    errorIdent (AnnotatedError (EDisconnectedEmitter _emitter) _pos) = "AE-004"
    errorIdent (AnnotatedError (EChannelWithoutSources _channel) _pos) = "AE-005"
    errorIdent (AnnotatedError (EChannelWithoutTarget _channel) _pos) = "AE-006"
    errorIdent (AnnotatedError (EUnusedResource _ident) _pos) = "AE-007"
    errorIdent (AnnotatedError (EUnusedPool _ident) _pos) = "AE-008"
    errorIdent (AnnotatedError _err _pos) = "Internal"

    errorTitle (AnnotatedError (EDuplicatedEmitterConnection _ident _prev) _pos) = "duplicated emitter connection"
    errorTitle (AnnotatedError (EDuplicatedChannelConnection _channel _prev) _pos) = "duplicated channel connection"
    errorTitle (AnnotatedError (EMismatchedBoxSource _expectedSource _actualSource _boxTrace) _pos) = "mismatched box source"
    errorTitle (AnnotatedError (EDisconnectedEmitter _emitter) _pos) = "disconnected emitter"
    errorTitle (AnnotatedError (EChannelWithoutSources _channel) _pos) = "channel without sources"
    errorTitle (AnnotatedError (EChannelWithoutTarget _channel) _pos) = "channel without target"
    errorTitle (AnnotatedError (EUnusedResource _ident) _pos) = "unused resource"
    errorTitle (AnnotatedError (EUnusedPool _ident) _pos) = "unused pool"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
        case err of
            EDuplicatedEmitterConnection emitter prevPos@(Position _ prevStart _prevEnd) ->
                let prevFileName = sourceName prevStart
                    prevSourceLines = files M.! prevFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Emitter \x1b[31m" <> T.pack emitter <>
                            "\x1b[0m is already connected to a sink port. " <>
                            "Only one target is allowed per event source.\n")) <>
                    pprintSimpleError
                        prevSourceLines "The previous connection was done here:" prevFileName
                        prevPos Nothing
            EDuplicatedChannelConnection channel prevPos@(Position _ procStart _procEnd) ->
                let prevFileName = sourceName procStart
                    prevSourceLines = files M.! prevFileName
                in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Channel \x1b[31m" <> T.pack channel <>
                            "\x1b[0m is already connected to an input port. " <>
                            "Only one target is allowed per channel.\n")) <>
                    pprintSimpleError
                        prevSourceLines "The previous connection was done here:" prevFileName
                        prevPos Nothing
            EMismatchedBoxSource expectedSource actualSource boxTrace ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Expected allocation from \x1b[31m" <> T.pack expectedSource <>
                        "\x1b[0m but the box is being allocated from \x1b[31m" <> T.pack actualSource <>
                        "\x1b[0m.")) <>
                printBoxTrace expectedSource (reverse boxTrace)
            EDisconnectedEmitter emitter ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Emitter \x1b[31m" <> T.pack emitter <>
                        "\x1b[0m is not connected to any sink port. " <>
                        "All event sources must be connected to a target.")) 
            EChannelWithoutSources channel ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Channel \x1b[31m" <> T.pack channel <>
                        "\x1b[0m is not connected to any outbound port. " <>
                        "All channels must have at least one source.")) 
            EChannelWithoutTarget channel -> 
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Channel \x1b[31m" <> T.pack channel <>
                        "\x1b[0m is not connected to any inbound port. " <>
                        "All channels must be connected to a target.")) 
            EUnusedResource ident ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Resource \x1b[31m" <> T.pack ident <>
                        "\x1b[0m is not being used by any element. " <>
                        "All resources must be connected to at least one access port."))
            EUnusedPool poolId ->
                pprintSimpleError
                    sourceLines title fileName pos
                    (Just ("Pool \x1b[31m" <> T.pack poolId <>
                        "\x1b[0m is not being used by any element. " <>
                        "All pools must be connected to at least one access port."))
            _ -> T.pack $ show pos ++ ": " ++ show e

            where

                -- | Prints a trace of box allocations 
                printBoxTrace :: Identifier -> [Location] -> T.Text
                printBoxTrace _ [] = ""
                printBoxTrace expectedSource [tracePos@(Position _ traceStartPos _)] =
                    let title = "\nThe box is being freed here to allocator \x1b[31m" <> T.pack expectedSource <> "\x1b[0m:"
                        traceFileName = sourceName traceStartPos
                        traceSourceLines = files M.! traceFileName
                    in
                        pprintSimpleError 
                            traceSourceLines title traceFileName tracePos Nothing
                printBoxTrace expectedSource (tracePos@(Position _ traceStartPos _) : xr) =
                    let title = "\nThe box is first moved here:"
                        traceFileName = sourceName traceStartPos
                        traceSourceLines = files M.! traceFileName
                    in
                        pprintSimpleError
                            traceSourceLines title traceFileName tracePos Nothing <> printBoxTrace' expectedSource xr
                printBoxTrace _ _ = error "Internal error: invalid error position"

                printBoxTrace' :: Identifier -> [Location] -> T.Text
                printBoxTrace' _ [] = ""
                printBoxTrace' expectedSource [tracePos@(Position _ traceStartPos _)] =
                    let title = "\nFinally, box is being freed here to allocator \x1b[31m" <> T.pack expectedSource <> "\x1b[0m:"
                        traceFileName = sourceName traceStartPos
                        traceSourceLines = files M.! traceFileName
                    in
                        pprintSimpleError 
                            traceSourceLines title traceFileName tracePos Nothing
                printBoxTrace' expectedSource (tracePos@(Position _ traceStartPos _) : xr) =
                    let title = "\nThe box is moved again here:"
                        traceFileName = sourceName traceStartPos
                        traceSourceLines = files M.! traceFileName
                    in
                        pprintSimpleError
                            traceSourceLines title traceFileName tracePos Nothing <> printBoxTrace' expectedSource xr
                printBoxTrace' _ _ = error "Internal error: invalid error position"

    toText e@(AnnotatedError err Internal) _files =
        let title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."             
        in
            -- | Check the errors are related to the internal resources/emitters
            case err of
                EDisconnectedEmitter emitter -> title <>
                    "\nEmitter \x1b[31m" <> T.pack emitter <>
                    "\x1b[0m is not connected to any sink port. " <>
                    "All event sources must be connected to a target."

                EUnusedResource ident -> title <> 
                    "\nResource \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is not being used by any element. " <>
                    "All resources must be connected to at least one access port."
                _ -> title

    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."