{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.RT.Errors where
import Utils.Annotations
import EFP.Schedulability.RT.AST
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
import Utils.Errors
import EFP.Schedulability.RT.Types

--------------------------------------------------
-- Real-time type checker error handling
--------------------------------------------------

data Error
  = 
    EUnknownOutputPort Identifier -- ^ Unknown output port (internal)
    | EChannelNotConnected Identifier -- ^ Channel not connected (internal)
    | EInvalidTransactionMap -- ^ Invalid transaction map (internal)
    | EInvalidRTElementDefinition -- ^ Invalid RT element definition (internal)
    | EInvalidTask Identifier -- ^ Invalid task (internal)
    | EInvalidTaskClass Identifier -- ^ Invalid task class (internal)
    | EInvalidTargetPort Identifier -- ^ Invalid target port (internal)
    | EUnknownComponent Identifier -- ^ Unknown component
    | EDuplicatedStepName Identifier Location -- ^ Duplicated step name
    | EUnknownAction Identifier (Identifier, Location) -- ^ Unknown action in a component class
    | EUnknownTransPath Identifier Identifier Identifier -- ^ Unknown transactional path
    | EActionMustContinue Identifier Identifier Identifier [Continuation] -- ^ Action must continue
    | EActionMustNotContinue Identifier Identifier Identifier Location -- ^ Action must not continue
    | EExpectedStepActionContinuation -- ^ Expected step action continuation
    | EUnknownConstant Identifier -- ^ Unknown constant
    | EConditionalComponentMismatch (Identifier, Identifier) (Identifier, Identifier) -- ^ Conditional branches refer to different components
    | EInvalidInitialStepMulticast -- ^ Initial step cannot be a multicast
    | EInvalidMulticastSingleContinuation Continuation -- ^ Multicast with single continuation
    | EUnknownTask Identifier -- ^ Unknown task
    | EInvalidContinuationTaskMismatch Identifier Continuation -- ^ Continuation task mismatch
    | EInvalidContinuationActionMismatch Identifier Continuation -- ^ Continuation action mismatch
    | EDuplicatedMulticastContinuation Continuation Location -- ^ Duplicated multicast continuation
    | EInvalidMulticastContinuation Continuation [Continuation] -- ^ Invalid multicast continuation
    | EExpectedMulticastContinuation [Continuation] -- ^ Expected multicast continuation
    | EPreviousTransactionWithSameName Identifier Location -- ^ Previous transaction with same name
    | EPreviousSituationWithSameName Identifier Location -- ^ Previous situation with same name
    deriving Show

type RTErrors = AnnotatedError Error Location

instance ErrorMessage RTErrors where


    errorIdent (AnnotatedError (EUnknownComponent {}) _pos) = "RTE-001"
    errorIdent (AnnotatedError (EDuplicatedStepName {}) _pos) = "RTE-002"
    errorIdent (AnnotatedError (EUnknownAction {}) _pos) = "RTE-003"
    errorIdent (AnnotatedError (EUnknownTransPath {}) _pos) = "RTE-004"
    errorIdent (AnnotatedError (EActionMustContinue {}) _pos) = "RTE-005"
    errorIdent (AnnotatedError (EActionMustNotContinue {}) _pos) = "RTE-006"
    errorIdent (AnnotatedError (EExpectedStepActionContinuation {}) _pos) = "RTE-007"
    errorIdent (AnnotatedError (EUnknownConstant {}) _pos) = "RTE-008"
    errorIdent (AnnotatedError (EConditionalComponentMismatch {}) _pos) = "RTE-009"
    errorIdent (AnnotatedError (EInvalidInitialStepMulticast {}) _pos) = "RTE-010"
    errorIdent (AnnotatedError (EInvalidMulticastSingleContinuation {}) _pos) = "RTE-011"
    errorIdent (AnnotatedError (EUnknownTask {}) _pos) = "RTE-012"
    errorIdent (AnnotatedError (EInvalidContinuationTaskMismatch {}) _pos) = "RTE-013"
    errorIdent (AnnotatedError (EInvalidContinuationActionMismatch {}) _pos) = "RTE-014"
    errorIdent (AnnotatedError (EDuplicatedMulticastContinuation {}) _pos) = "RTE-015"
    errorIdent (AnnotatedError (EInvalidMulticastContinuation {}) _pos) = "RTE-016"
    errorIdent (AnnotatedError (EExpectedMulticastContinuation {}) _pos) = "RTE-017"
    errorIdent (AnnotatedError (EPreviousTransactionWithSameName {}) _pos) = "RTE-018"
    errorIdent (AnnotatedError (EPreviousSituationWithSameName {}) _pos) = "RTE-019"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (EUnknownComponent {}) _pos) = "unknown component"
    errorTitle (AnnotatedError (EDuplicatedStepName {}) _pos) = "duplicated step name"
    errorTitle (AnnotatedError (EUnknownAction {}) _pos) = "unknown action"
    errorTitle (AnnotatedError (EUnknownTransPath {}) _pos) = "unknown transactional path"
    errorTitle (AnnotatedError (EActionMustContinue {}) _pos) = "action must continue"
    errorTitle (AnnotatedError (EActionMustNotContinue {}) _pos) = "action must not continue"
    errorTitle (AnnotatedError (EExpectedStepActionContinuation {}) _pos) = "expected step action continuation"
    errorTitle (AnnotatedError (EUnknownConstant {}) _pos) = "unknown constant"
    errorTitle (AnnotatedError (EConditionalComponentMismatch {}) _pos) = "conditional component mismatch"
    errorTitle (AnnotatedError (EInvalidInitialStepMulticast {}) _pos) = "invalid initial step multicast"
    errorTitle (AnnotatedError (EInvalidMulticastSingleContinuation {}) _pos) = "invalid multicast single continuation"
    errorTitle (AnnotatedError (EUnknownTask {}) _pos) = "unknown task"
    errorTitle (AnnotatedError (EInvalidContinuationTaskMismatch {}) _pos) = "invalid continuation task mismatch"
    errorTitle (AnnotatedError (EInvalidContinuationActionMismatch {}) _pos) = "invalid continuation action mismatch"
    errorTitle (AnnotatedError (EDuplicatedMulticastContinuation {}) _pos) = "duplicated multicast continuation"
    errorTitle (AnnotatedError (EInvalidMulticastContinuation {}) _pos) = "invalid multicast continuation"
    errorTitle (AnnotatedError (EExpectedMulticastContinuation {}) _pos) = "expected multicast continuation"
    errorTitle (AnnotatedError (EPreviousTransactionWithSameName {}) _pos) = "previous transaction with same name"
    errorTitle (AnnotatedError (EPreviousSituationWithSameName {}) _pos) = "previous situation with same name"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of 
                EUnknownComponent ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown component \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EDuplicatedStepName ident stepPos@(Position _ stepStart _stepEnd) ->
                    let stepFileName = sourceName stepStart
                        stepSourceLines = files M.! stepFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("There is a step with the name \x1b[31m" <> T.pack ident <> "\x1b[0m in the current transaction.\n")) <>
                        pprintSimpleError
                            stepSourceLines "The step was previoulsy defined here:" stepFileName
                            stepPos Nothing
                EUnknownAction actionId (clsId, clsLoc@(Position _ clsStart _clsEnd)) ->
                    let clsFileName = sourceName clsStart
                        clsSourceLines = files M.! clsFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Unknown action \x1b[31m" <> T.pack actionId <> "\x1b[0m in component class \x1b[34m" <> T.pack clsId <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            clsSourceLines "The component class is defined here:" clsFileName
                            clsLoc Nothing
                EUnknownTransPath compId actionId pathId ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown transactional path \x1b[31m" <> T.pack compId <> "." <> T.pack actionId <> "::" <> T.pack pathId <> "\x1b[0m."))
                EActionMustContinue clsId actionId pathId continuations ->
                    let contText = T.intercalate ", " [ "\x1b[34m" <> T.pack tId <> "." <> T.pack aId <> "\x1b[0m" | (tId, aId) <- continuations ]
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The action \x1b[34m" <> T.pack actionId <> 
                            "\x1b[0m in component class \x1b[34m" <> T.pack clsId <>
                            "\x1b[0m must continue according to transactional path \x1b[34m" <> T.pack pathId <> "\x1b[0m.\n"
                            <> "Valid continuations are: " <> contText <> "."))
                EActionMustNotContinue clsId actionId pathId pathLoc@(Position _ pathStart _pathEnd) ->
                    let pathFileName = sourceName pathStart
                        pathSourceLines = files M.! pathFileName
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The action \x1b[34m" <> T.pack actionId <> 
                            "\x1b[0m in component class \x1b[34m" <> T.pack clsId <>
                            "\x1b[0m must not continue according to transactional path \x1b[34m" <> T.pack pathId <> "\x1b[0m.\n")) <>
                    pprintSimpleError
                        pathSourceLines "The transactional path is defined here:" pathFileName
                        pathLoc Nothing
                EExpectedStepActionContinuation ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Invalid continuation: expected a step action continuation.")
                EUnknownConstant ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown constant \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EConditionalComponentMismatch (compA, actA) (compB, actB) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Conditional branches refer to different components/actions: " <>
                            "\x1b[34m" <> T.pack compA <> "." <> T.pack actA <> "\x1b[0m and " <>
                            "\x1b[34m" <> T.pack compB <> "." <> T.pack actB <> "\x1b[0m."))
                EInvalidInitialStepMulticast ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The initial step of a transaction cannot be a multicast.")
                EInvalidMulticastSingleContinuation (compId, actId) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("A multicast step must have multiple continuations. This step has a single continuation: " <>
                            "\x1b[34m" <> T.pack compId <> "." <> T.pack actId <> "\x1b[0m."))
                EUnknownTask ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown task \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EInvalidContinuationTaskMismatch targetTask (taskId, actionId) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Continuation task mismatch: the target task is \x1b[31m" <> T.pack targetTask <> 
                            "\x1b[0m, but the only valid continuation is \x1b[31m" <> T.pack taskId <> "." <> T.pack actionId <> "\x1b[0m."))
                EInvalidContinuationActionMismatch targetAction (taskId, actionId) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Continuation action mismatch: the target action is \x1b[31m" <> T.pack taskId <> "." <> T.pack targetAction <> 
                            "\x1b[0m, but the only valid continuation is \x1b[31m" <> T.pack taskId <> "." <> T.pack actionId <> "\x1b[0m."))
                EDuplicatedMulticastContinuation (compId, actId) stepPos@(Position _ stepStart _stepEnd) ->
                    let stepFileName = sourceName stepStart
                        stepSourceLines = files M.! stepFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The multicast continuation \x1b[34m" <> T.pack compId <> "::" <> T.pack actId <> "\x1b[0m is duplicated in the current multicast step.\n")) <>
                        pprintSimpleError
                            stepSourceLines "The continuation was previously selected here:" stepFileName
                            stepPos Nothing
                EInvalidMulticastContinuation (compId, actId) validContinuations ->
                    let validContText = T.intercalate ", " [ "\x1b[34m" <> T.pack tId <> "::" <> T.pack aId <> "\x1b[0m" | (tId, aId) <- validContinuations ]
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid multicast continuation \x1b[31m" <> T.pack compId <> "::" <> T.pack actId <> 
                            "\x1b[0m. Valid continuations are: " <> validContText <> "."))
                EExpectedMulticastContinuation validContinuations ->
                    let validContText = T.intercalate ", " [ "\x1b[34m" <> T.pack tId <> "::" <> T.pack aId <> "\x1b[0m" | (tId, aId) <- validContinuations ]
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Expected a multicast continuation. Valid continuations are: " <> validContText <> "."))
                EPreviousTransactionWithSameName ident prevLoc@(Position _ prevStart _prevEnd) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("There is a previous transaction with the name \x1b[31m" <> T.pack ident <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            prevSourceLines "The previous transaction is defined here:" prevFileName
                            prevLoc Nothing
                EPreviousSituationWithSameName ident prevLoc@(Position _ prevStart _prevEnd) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("There is a previous situation with the name \x1b[31m" <> T.pack ident <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            prevSourceLines "The previous situation is defined here:" prevFileName
                            prevLoc Nothing
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."