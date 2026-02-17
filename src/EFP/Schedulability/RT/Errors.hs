{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.RT.Errors where
import Utils.Annotations
import EFP.Schedulability.Core.AST
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
import Utils.Errors
import EFP.Schedulability.RT.Semantic.Types
import Utils.Printer

--------------------------------------------------
-- Real-time type checker error handling
--------------------------------------------------

data Error
  = 
    EUnknownOutputPort Identifier -- ^ Unknown output port (internal)
    | EChannelNotConnected Identifier -- ^ Channel not connected (internal)
    | EInvalidTransactionMap -- ^ Invalid transaction map (internal)
    | EInvalidRTElementDefinition -- ^ Invalid RT element definition (internal)
    | EInvalidTransaction -- ^ Invalid transaction (internal)
    | EInvalidTask Identifier -- ^ Invalid task (internal)
    | EInvalidTaskClass Identifier -- ^ Invalid task class (internal)
    | EInvalidHandlerClass Identifier -- ^ Invalid handler class (internal)
    | EInvalidTargetPort Identifier -- ^ Invalid target port (internal)
    | EInvalidConstExpressionOperandTypes -- ^ Invalid constant expression operand types (internal)
    | EInvalidEventEmitter Identifier -- ^ Invalid event emitter (internal)
    | EInvalidSinkPort Identifier -- ^ Invalid sink port (internal)
    | EInvalidConstExpressionOperand Op -- ^ Invalid constant expression operand (internal)
    | EInvalidArrivalExpressionType -- ^ Invalid arrival expression type (internal)
    | EInvalidSituationAnnotation -- ^ Invalid situation annotation (internal)
    | EFlatArrivalExpressionNegative -- ^ Arrival expression evaluated to negative in flattening (internal)
    | EInvalidDeadlineExpression -- ^ Invalid deadline expression (internal)
    | EInvalidArrivalsExpression -- ^ Invalid arrivals expression (internal)
    | EInvalidConditionalExpression -- ^ Invalid conditional expression (internal)
    | EUnexpectedEndStep -- ^ Unexpected end step (internal)
    | EInvalidConditionalExpressionType -- ^ Invalid conditional expression type
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
    | EConstExpressionTypeMismatch ConstExprType ConstExprType -- ^ Constant expression type mismatch
    | EConditionalStepsMustHaveMultipleBranches -- ^ Conditional steps must have multiple branches
    | EMissingEventField Identifier -- ^ Missing event field
    | EInvalidEventFieldType Identifier -- ^ Invalid event field type
    | EUnknownTransaction Identifier -- ^ Unknown transaction
    | EUnknownTransactionStep Identifier (Identifier, Location) -- ^ Unknown transaction step
    | EDuplicateEventField Identifier Location -- ^ Duplicate event field
    | EDuplicateEventDefinition Identifier Location -- ^ Duplicate event definition
    | EEmitterTargetMismatch Identifier Identifier Identifier -- ^ Emitter target mismatch
    | EUnknownEventEmitter Identifier -- ^ Unknown event emitter
    | EInvalidEventDefinitionType -- ^ Invalid event definition type
    | EInvalidTransactionFieldType -- ^ Invalid transaction field type
    | EUnsupportedEmitterType -- ^ Unsupported emitter type
    | EInvalidDeadlineFieldType -- ^ Invalid deadline field type
    | EInvalidEventField Identifier [Identifier] -- ^ Invalid event field name
    | EEmitterActionMismatch Identifier Identifier Identifier (Identifier, Identifier, Location) -- ^ Emitter action mismatch
    | EConditionalExpressionNotInteger -- ^ Conditional expression is not an integer
    | EConditionalExpressionOutOfRange Integer -- ^ Conditional expression out of range
    | EFlatConditionalExpressionOutOfRange Integer Location -- ^ Conditional expression out of range in flattening
    | EConstExpressionDivisionByZero -- ^ Division by zero in constant expression
    | EInvalidEmitterFieldType -- ^ Invalid emitter field type
    | EInvalidIntervalValue Integer -- ^ Invalid interval value
    | EInvalidIntervalExpression -- ^ Invalid interval expression
    | EInvalidArrivalsValue Integer -- ^ Invalid arrivals value
    | EInvalidDeadlineValue Double -- ^ Invalid deadline value
    | EInvalidInitialEndStep -- ^ Initial step cannot be an end step
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
    errorIdent (AnnotatedError (EConstExpressionTypeMismatch {}) _pos) = "RTE-020"
    errorIdent (AnnotatedError (EConditionalStepsMustHaveMultipleBranches {}) _pos) = "RTE-021"
    errorIdent (AnnotatedError (EMissingEventField {}) _pos) = "RTE-022"
    errorIdent (AnnotatedError (EInvalidEventFieldType {}) _pos) = "RTE-023"
    errorIdent (AnnotatedError (EUnknownTransaction {}) _pos) = "RTE-024"
    errorIdent (AnnotatedError (EUnknownTransactionStep {}) _pos) = "RTE-025"
    errorIdent (AnnotatedError (EDuplicateEventField {}) _pos) = "RTE-026"
    errorIdent (AnnotatedError (EDuplicateEventDefinition {}) _pos) = "RTE-027"
    errorIdent (AnnotatedError (EEmitterTargetMismatch {}) _pos) = "RTE-028"
    errorIdent (AnnotatedError (EUnknownEventEmitter {}) _pos) = "RTE-029"
    errorIdent (AnnotatedError (EInvalidEventDefinitionType {}) _pos) = "RTE-030"
    errorIdent (AnnotatedError (EInvalidTransactionFieldType {}) _pos) = "RTE-031"
    errorIdent (AnnotatedError (EUnsupportedEmitterType {}) _pos) = "RTE-032"
    errorIdent (AnnotatedError (EInvalidDeadlineFieldType {}) _pos) = "RTE-033"
    errorIdent (AnnotatedError (EInvalidEventField {}) _pos) = "RTE-034"
    errorIdent (AnnotatedError (EEmitterActionMismatch {}) _pos) = "RTE-035"
    errorIdent (AnnotatedError (EConditionalExpressionNotInteger {}) _pos) = "RTE-036"
    errorIdent (AnnotatedError (EConditionalExpressionOutOfRange {}) _pos) = "RTE-037"
    errorIdent (AnnotatedError (EFlatConditionalExpressionOutOfRange {}) _pos) = "RTE-038"
    errorIdent (AnnotatedError (EConstExpressionDivisionByZero {}) _pos) = "RTE-039"
    errorIdent (AnnotatedError (EInvalidEmitterFieldType {}) _pos) = "RTE-040"
    errorIdent (AnnotatedError (EInvalidIntervalValue {}) _pos) = "RTE-041"
    errorIdent (AnnotatedError (EInvalidArrivalsValue {}) _pos) = "RTE-042"
    errorIdent (AnnotatedError (EInvalidDeadlineValue {}) _pos) = "RTE-043"
    errorIdent (AnnotatedError (EInvalidInitialEndStep {}) _pos) = "RTE-044"
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
    errorTitle (AnnotatedError (EConstExpressionTypeMismatch {}) _pos) = "constant expression type mismatch"
    errorTitle (AnnotatedError (EConditionalStepsMustHaveMultipleBranches {}) _pos) = "conditional steps must have multiple branches"
    errorTitle (AnnotatedError (EMissingEventField {}) _pos) = "missing event field"
    errorTitle (AnnotatedError (EInvalidEventFieldType {}) _pos) = "invalid event field type"
    errorTitle (AnnotatedError (EUnknownTransaction {}) _pos) = "unknown transaction"
    errorTitle (AnnotatedError (EUnknownTransactionStep {}) _pos) = "unknown transaction step"
    errorTitle (AnnotatedError (EDuplicateEventField {}) _pos) = "duplicate event field"
    errorTitle (AnnotatedError (EDuplicateEventDefinition {}) _pos) = "duplicate event definition"
    errorTitle (AnnotatedError (EEmitterTargetMismatch {}) _pos) = "emitter target mismatch"
    errorTitle (AnnotatedError (EUnknownEventEmitter {}) _pos) = "unknown event emitter"
    errorTitle (AnnotatedError (EInvalidEventDefinitionType {}) _pos) = "invalid event definition type"
    errorTitle (AnnotatedError (EInvalidTransactionFieldType {}) _pos) = "invalid transaction field type"
    errorTitle (AnnotatedError (EUnsupportedEmitterType {}) _pos) = "unsupported emitter type"
    errorTitle (AnnotatedError (EInvalidDeadlineFieldType {}) _pos) = "invalid deadline field type"
    errorTitle (AnnotatedError (EInvalidEventField {}) _pos) = "invalid event field"
    errorTitle (AnnotatedError (EEmitterActionMismatch {}) _pos) = "emitter action mismatch"
    errorTitle (AnnotatedError (EConditionalExpressionNotInteger {}) _pos) = "conditional expression not integer"
    errorTitle (AnnotatedError (EConditionalExpressionOutOfRange {}) _pos) = "conditional expression out of range"
    errorTitle (AnnotatedError (EFlatConditionalExpressionOutOfRange {}) _pos) = "conditional expression out of range in flattening"
    errorTitle (AnnotatedError (EConstExpressionDivisionByZero {}) _pos) = "constant expression division by zero"
    errorTitle (AnnotatedError (EInvalidEmitterFieldType {}) _pos) = "invalid emitter field type"
    errorTitle (AnnotatedError (EInvalidIntervalValue {}) _pos) = "invalid interval value"
    errorTitle (AnnotatedError (EInvalidArrivalsValue {}) _pos) = "invalid arrivals value"
    errorTitle (AnnotatedError (EInvalidDeadlineValue {}) _pos) = "invalid deadline value"
    errorTitle (AnnotatedError (EInvalidInitialEndStep {}) _pos) = "invalid initial end step"
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
                EConstExpressionTypeMismatch t1 t2 ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Constant expression type mismatch: found \x1b[31m" <> showText t1 <> "\x1b[0m and \x1b[31m" <> showText t2 <> "\x1b[0m."))
                EConditionalStepsMustHaveMultipleBranches ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Conditional steps must have multiple branches.")
                EMissingEventField fieldName ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Missing event field \x1b[31m" <> T.pack fieldName <> "\x1b[0m."))
                EInvalidEventFieldType fieldName ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid type for field \x1b[31m" <> T.pack fieldName <> "\x1b[0m."))
                EUnknownTransaction ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown transaction \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EUnknownTransactionStep stepId (transId, transLoc@(Position _ transStart _transEnd)) ->
                    let transFileName = sourceName transStart
                        transSourceLines = files M.! transFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Unknown step \x1b[31m" <> T.pack stepId <> "\x1b[0m in transaction \x1b[34m" <> T.pack transId <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            transSourceLines "The transaction is defined here:" transFileName
                            transLoc Nothing
                EDuplicateEventField fieldName fieldLoc@(Position _ fieldStart _fieldEnd) ->
                    let fieldFileName = sourceName fieldStart
                        fieldSourceLines = files M.! fieldFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The event field \x1b[34m" <> T.pack fieldName <> "\x1b[0m is duplicated in the current event definition.\n")) <> 
                        pprintSimpleError
                            fieldSourceLines "The field was previously defined here:" fieldFileName
                            fieldLoc Nothing
                EDuplicateEventDefinition eventId eventLoc@(Position _ eventStart _eventEnd) ->
                    let eventFileName = sourceName eventStart
                        eventSourceLines = files M.! eventFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("An event with name \x1b[34m" <> T.pack eventId <> "\x1b[0m already exists in the current situation.\n")) <>
                        pprintSimpleError
                            eventSourceLines "The event was previously defined here:" eventFileName
                            eventLoc Nothing
                EEmitterTargetMismatch emitterId targetCmp actualCmp ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Emitter \x1b[31m" <> T.pack emitterId <> "\x1b[0m is connected to component \x1b[31m" <> T.pack actualCmp <>
                            "\x1b[0m, but the target component is \x1b[31m" <> T.pack targetCmp <> "\x1b[0m."))
                EUnknownEventEmitter emitterId ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown event emitter \x1b[31m" <> T.pack emitterId <> "\x1b[0m."))
                EInvalidEventDefinitionType ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Invalid event definition type. Expected a multiple field assignment.")
                EInvalidTransactionFieldType ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Invalid transaction field type. Expected a transaction identifier.")
                EUnsupportedEmitterType ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Unsupported emitter type. Only interrupt and periodic timer emitters are supported.")
                EInvalidDeadlineFieldType ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Invalid deadline field type. Expected a multiple field assignment.")
                EInvalidEventField fieldName validNames ->
                    let validNamesText = T.intercalate ", " [ "\x1b[34m" <> T.pack name <> "\x1b[0m" | name <- validNames ]
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid event field name \x1b[31m" <> T.pack fieldName <> "\x1b[0m. Valid field names are: " <> validNamesText <> "."))
                EEmitterActionMismatch emitterId targetCmp targetAction (port, act, clsLoc@(Position _ clsStart _clsEnd)) ->
                    let clsFileName = sourceName clsStart
                        clsSourceLines = files M.! clsFileName
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Emitter \x1b[31m" <> T.pack emitterId <> "\x1b[0m is connected to port \x1b[31m" <> T.pack port <>
                            "\x1b[0m that triggers action \x1b[31m" <> T.pack act <> "\x1b[0m in component \x1b[31m" <> T.pack targetCmp <>
                            "\x1b[0m, but the starting action of the transaction is \x1b[31m" <> T.pack targetAction <> "\x1b[0m.\n")) <>
                    pprintSimpleError
                        clsSourceLines "The component class is defined here:" clsFileName
                        clsLoc Nothing
                EConditionalExpressionNotInteger ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Conditional expression must evaluate to an integer between 1 and 100.")
                EConditionalExpressionOutOfRange value ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Conditional expression value \x1b[31m" <> T.pack (show value) <> "\x1b[0m is out of range. Expected an integer between 1 and 100."))
                EFlatConditionalExpressionOutOfRange value loc@(Position _ locStart _locEnd) ->
                    let locFileName = sourceName locStart
                        locSourceLines = files M.! locFileName
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Error when flattening conditional expression: the resulting value \x1b[31m" <> T.pack (show value) <> "\x1b[0m is out of range. Expected an integer between 1 and 100.")) <>
                    pprintSimpleError
                        locSourceLines "The inner conditional expression is defined here:" locFileName
                        loc Nothing
                EConstExpressionDivisionByZero ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Division by zero in constant expression.")
                EInvalidEmitterFieldType ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Invalid emitter field type. Expected a multiple field assignment.")
                EInvalidIntervalValue value ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid interval value \x1b[31m" <> T.pack (show value) <> "\x1b[0m. Interval must be a positive integer."))
                EInvalidArrivalsValue value ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid arrivals value \x1b[31m" <> T.pack (show value) <> "\x1b[0m. Arrivals must be a positive integer."))
                EInvalidDeadlineValue value ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid deadline value \x1b[31m" <> T.pack (show value) <> "\x1b[0m. Deadline must be a positive number."))
                EInvalidInitialEndStep ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The initial step of a transaction cannot be an end step.")
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."