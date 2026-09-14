{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.Architecture.Errors (
    ArchitectureError, Error(..)
) where

import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import Utils.Errors
import Text.Parsec
import qualified Data.Map.Strict as M
import qualified Data.Text as T

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
    | EMissingPeriodicTimerInitializer -- ^ Missing initializer expression for periodic timer emitter (Internal)
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

instance Diagnosable Error where

    describe (EDuplicatedEmitterConnection emitter prevPos) =
        relatedTo prevPos "the previous connection" $
            diagnostic "AE-001" "duplicated emitter connection"
                ("Emitter " <> emph (T.pack emitter) <>
                    " is already connected to a sink port. " <>
                    "Only one target is allowed per event source.")
    describe (EDuplicatedChannelConnection channel prevPos) =
        relatedTo prevPos "the previous connection" $
            diagnostic "AE-002" "duplicated channel connection"
                ("Channel " <> emph (T.pack channel) <>
                    " is already connected to an input port. " <>
                    "Only one target is allowed per channel.")
    describe (EMismatchedBoxSource expectedSource actualSource _boxTrace) =
        diagnostic "AE-003" "mismatched box source"
            ("Expected allocation from " <> emph (T.pack expectedSource) <>
                " but the box is being allocated from " <> emph (T.pack actualSource) <> ".")
    describe (EDisconnectedEmitter emitter) =
        diagnostic "AE-004" "disconnected emitter"
            ("Emitter " <> emph (T.pack emitter) <>
                " is not connected to any sink port. " <>
                "All event sources must be connected to a target.")
    describe (EChannelWithoutSources channel) =
        diagnostic "AE-005" "channel without sources"
            ("Channel " <> emph (T.pack channel) <>
                " is not connected to any outbound port. " <>
                "All channels must have at least one source.")
    describe (EChannelWithoutTarget channel) =
        diagnostic "AE-006" "channel without target"
            ("Channel " <> emph (T.pack channel) <>
                " is not connected to any inbound port. " <>
                "All channels must be connected to a target.")
    describe (EUnusedResource ident) =
        diagnostic "AE-007" "unused resource"
            ("Resource " <> emph (T.pack ident) <>
                " is not being used by any element. " <>
                "All resources must be connected to at least one access port.")
    describe (EUnusedPool poolId) =
        diagnostic "AE-008" "unused pool"
            ("Pool " <> emph (T.pack poolId) <>
                " is not being used by any element. " <>
                "All pools must be connected to at least one access port.")
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage ArchitectureError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError

    -- | The trail a box follows walks several files, so its message is a block
    -- per step instead of one block with pointers.
    toText e@(AnnotatedError (EMismatchedBoxSource expectedSource _actualSource boxTrace) _pos) files =
        errorToText e files <> printBoxTrace expectedSource (reverse boxTrace)

        where

            -- | Prints a trace of box allocations
            printBoxTrace :: Identifier -> [Location] -> T.Text
            printBoxTrace _ [] = ""
            printBoxTrace source [tracePos@(Position _ traceStartPos _)] =
                let title = "\nThe box is being freed here to allocator " <> emph (T.pack source) <> ":"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing
            printBoxTrace source (tracePos@(Position _ traceStartPos _) : xr) =
                let title = "\nThe box is first moved here:"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing <> printBoxTrace' source xr
            printBoxTrace _ _ = error "Internal error: invalid error position"

            printBoxTrace' :: Identifier -> [Location] -> T.Text
            printBoxTrace' _ [] = ""
            printBoxTrace' source [tracePos@(Position _ traceStartPos _)] =
                let title = "\nFinally, box is being freed here to allocator " <> emph (T.pack source) <> ":"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing
            printBoxTrace' source (tracePos@(Position _ traceStartPos _) : xr) =
                let title = "\nThe box is moved again here:"
                    traceFileName = sourceName traceStartPos
                    traceSourceLines = files M.! traceFileName
                in
                    pprintSimpleError
                        traceSourceLines title traceFileName tracePos Nothing <> printBoxTrace' source xr
            printBoxTrace' _ _ = error "Internal error: invalid error position"

    toText e files = errorToText e files

    toDiagnostics = errorToDiagnostics
