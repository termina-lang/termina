{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.Architecture.Errors (
    ArchitectureError, Error(..), TestLevel(..)
) where

import Lowering.AST
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
    | EDuplicatedTaskPriority Identifier Integer Identifier Location -- ^ Two tasks with the same priority (AE-009)
    | EExitInSystemExceptAction Identifier Identifier -- ^ Reboot or noreturn call in the action that attends system_except (AE-010)
    | ESendInSystemExceptAction Identifier Identifier -- ^ Send in the action that attends system_except (AE-011)
    | EUnusedDeque Identifier -- ^ Unused deque (AE-012)
    | ESharedDeque Identifier Location -- ^ Deque connected to more than one access port (AE-013)
    | EUnfilledDeque Identifier Identifier Identifier -- ^ Deque that only gets back its own boxes (AE-014)
    | EActivationCycle Identifier [(Identifier, Identifier)] -- ^ Response to an event that reaches an action it has already run (AE-015)
    | EDuplicatedChannelSource Identifier Location -- ^ Channel connected to more than one out port (AE-016)
    | ETestComponentWithoutTest String Identifier -- ^ Component of a unit test that no test instance reaches (AE-017)
    | ETestPoolWithoutAllocator Identifier -- ^ Test pool that the unit does not reach through an allocator (AE-018)
    | EEmitterInUnitTest Identifier -- ^ Emitter other than a test emitter in a unit test (AE-019)
    | ETestOfOtherLevel TestLevel String Identifier -- ^ Element of one level of test in a deployment of the other (AE-020)
    deriving Show

-- | The level of a test, which its deployment decides: a unit test has test
-- instances and a concurrent test has none
data TestLevel = UnitTest | ConcurrentTest
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
    describe (EDuplicatedTaskPriority task priority prevTask prevPos) =
        relatedTo prevPos "the other task" $
            diagnostic "AE-009" "duplicated task priority"
                ("Task " <> emph (T.pack task) <> " has priority " <>
                    emph (T.pack (show priority)) <> ", the same as task " <>
                    emph (T.pack prevTask) <> ". " <>
                    "No two tasks may share a priority.")
    describe (EExitInSystemExceptAction cls action) =
        diagnostic "AE-010" "reboot or noreturn call in the system exception action"
            ("The action " <> emph (T.pack action) <> " of the handler class " <>
                emph (T.pack cls) <> " attends system_except, and the system reboots once it has run. " <>
                "It cannot reboot nor call a noreturn function.")
    describe (ESendInSystemExceptAction cls action) =
        diagnostic "AE-011" "send in the system exception action"
            ("The action " <> emph (T.pack action) <> " of the handler class " <>
                emph (T.pack cls) <> " attends system_except, and the system reboots once it has run. " <>
                "A message sent from it would reach a task that never runs.")
    describe (EUnusedDeque dequeId) =
        diagnostic "AE-012" "unused deque"
            ("Deque " <> emph (T.pack dequeId) <>
                " is not being used by any element. " <>
                "Every deque must be connected to one access port.")
    describe (ESharedDeque dequeId prevPos) =
        relatedTo prevPos "the other connection" $
            diagnostic "AE-013" "shared deque"
                ("Deque " <> emph (T.pack dequeId) <>
                    " is already connected to an access port. " <>
                    "A deque belongs to the one element connected to it.")
    describe (EUnfilledDeque dequeId owner port) =
        diagnostic "AE-014" "deque never filled"
            ("Deque " <> emph (T.pack dequeId) <> " only gets back the boxes that " <>
                emph (T.pack owner) <> " takes from it through " <> emph (T.pack port) <>
                ", so it is always empty. " <>
                "A deque has to get boxes from a pool, a channel or a procedure.")
    describe (EActivationCycle emitterId cycleSteps) =
        diagnostic "AE-015" "cycle in the response to an event"
            ("The response to " <> emph (T.pack emitterId) <> " reaches an action that is already in its chain of activations: " <>
                T.intercalate " -> " [emph (T.pack (entity ++ "." ++ action)) | (entity, action) <- cycleSteps] <>
                ". The chain of activations triggered by an event must be acyclic.")
    describe (EDuplicatedChannelSource channel prevPos) =
        relatedTo prevPos "the previous connection" $
            diagnostic "AE-016" "duplicated channel source"
                ("Channel " <> emph (T.pack channel) <>
                    " is already connected to an outbound port. " <>
                    "Only one source is allowed per channel.")
    describe (ETestComponentWithoutTest kind ident) =
        diagnostic "AE-017" "test component without test"
            (T.pack kind <> " " <> emph (T.pack ident) <>
                " is not connected to an access port of any test instance. " <>
                "Every component of a unit test must be reached by at least one case.")
    describe (ETestPoolWithoutAllocator poolId) =
        diagnostic "AE-018" "test pool without allocator"
            ("Test pool " <> emph (T.pack poolId) <>
                " is not connected to any allocator port of the unit. " <>
                "A test pool takes the place of a pool of the unit.")
    describe (EEmitterInUnitTest "system_except") =
        diagnostic "AE-019" "emitter in a unit test"
            ("In a unit test the harness attends " <> emph "system_except" <> " to record the exceptions of each case. " <>
                "A handler of system_except is tested by connecting it to a TestEmitter<Exception>.")
    describe (EEmitterInUnitTest emitter) =
        diagnostic "AE-019" "emitter in a unit test"
            ("Emitter " <> emph (T.pack emitter) <> " is not a test emitter, and the deployment has test instances. " <>
                "The events of a unit test come only from the cases, through test emitters.")
    describe (ETestOfOtherLevel ConcurrentTest kind ident) =
        diagnostic "AE-020" "element of a unit test in a concurrent test"
            (T.pack kind <> " " <> emph (T.pack ident) <>
                " belongs to a unit test, and the deployment has no test instances. " <>
                "A test without test instances is a concurrent test, which runs the application with its own emitters, channels and pools on the real clock.")
    describe (ETestOfOtherLevel UnitTest kind ident) =
        diagnostic "AE-020" "element of a concurrent test in a unit test"
            (T.pack kind <> " " <> emph (T.pack ident) <>
                " belongs to a concurrent test, and the deployment has test instances. " <>
                "A test with test instances is a unit test, where each case ends at its return.")
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ =diagnosticWithoutDetail "Internal" "internal error"

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
