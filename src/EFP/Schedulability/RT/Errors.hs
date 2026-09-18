{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.RT.Errors where
import Utils.Annotations
import EFP.Schedulability.Core.AST
import qualified Data.Text as T
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
    | EInvalidIntervalValue -- ^ Invalid interval value
    | EInvalidIntervalExpression -- ^ Invalid interval expression
    | EInvalidArrivalsValue Integer -- ^ Invalid arrivals value
    | EInvalidDeadlineValue Double -- ^ Invalid deadline value
    | EInvalidInitialEndStep -- ^ Initial step cannot be an end step
    deriving Show

type RTErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EUnknownComponent ident) =
        diagnostic "RTE-001" "unknown component"
            ("Unknown component \x1b[31m" <> T.pack ident <> "\x1b[0m.")
    describe (EDuplicatedStepName ident stepPos) =
        relatedTo stepPos "the step was previoulsy defined here" $
        diagnostic "RTE-002" "duplicated step name"
            ("There is a step with the name \x1b[31m" <> T.pack ident <> "\x1b[0m in the current transaction.\n")
    describe (EUnknownAction actionId (clsId, clsLoc)) =
        relatedTo clsLoc "the component class is defined here" $
        diagnostic "RTE-003" "unknown action"
            ("Unknown action \x1b[31m" <> T.pack actionId <> "\x1b[0m in component class \x1b[34m" <> T.pack clsId <> "\x1b[0m.\n")
    describe (EUnknownTransPath compId actionId pathId) =
        diagnostic "RTE-004" "unknown transactional path"
            ("Unknown transactional path \x1b[31m" <> T.pack compId <> "." <> T.pack actionId <> "::" <> T.pack pathId <> "\x1b[0m.")
    describe (EActionMustContinue clsId actionId pathId continuations) =
        let contText = T.intercalate ", " [ "\x1b[34m" <> T.pack tId <> "." <> T.pack aId <> "\x1b[0m" | (tId, aId) <- continuations ]
        in
            diagnostic "RTE-005" "action must continue"
                ("The action \x1b[34m" <> T.pack actionId <> "\x1b[0m in component class \x1b[34m" <> T.pack clsId <> "\x1b[0m must continue according to transactional path \x1b[34m" <> T.pack pathId <> "\x1b[0m.\n" <> "Valid continuations are: " <> contText <> ".")
    describe (EActionMustNotContinue clsId actionId pathId pathLoc) =
        relatedTo pathLoc "the transactional path is defined here" $
        diagnostic "RTE-006" "action must not continue"
            ("The action \x1b[34m" <> T.pack actionId <> "\x1b[0m in component class \x1b[34m" <> T.pack clsId <> "\x1b[0m must not continue according to transactional path \x1b[34m" <> T.pack pathId <> "\x1b[0m.\n")
    describe EExpectedStepActionContinuation =
        diagnostic "RTE-007" "expected step action continuation"
            ("Invalid continuation: expected a step action continuation.")
    describe (EUnknownConstant ident) =
        diagnostic "RTE-008" "unknown constant"
            ("Unknown constant \x1b[31m" <> T.pack ident <> "\x1b[0m.")
    describe (EConditionalComponentMismatch (compA, actA) (compB, actB)) =
        diagnostic "RTE-009" "conditional component mismatch"
            ("Conditional branches refer to different components/actions: " <> "\x1b[34m" <> T.pack compA <> "." <> T.pack actA <> "\x1b[0m and " <> "\x1b[34m" <> T.pack compB <> "." <> T.pack actB <> "\x1b[0m.")
    describe EInvalidInitialStepMulticast =
        diagnostic "RTE-010" "invalid initial step multicast"
            ("The initial step of a transaction cannot be a multicast.")
    describe (EInvalidMulticastSingleContinuation (compId, actId)) =
        diagnostic "RTE-011" "invalid multicast single continuation"
            ("A multicast step must have multiple continuations. This step has a single continuation: " <> "\x1b[34m" <> T.pack compId <> "." <> T.pack actId <> "\x1b[0m.")
    describe (EUnknownTask ident) =
        diagnostic "RTE-012" "unknown task"
            ("Unknown task \x1b[31m" <> T.pack ident <> "\x1b[0m.")
    describe (EInvalidContinuationTaskMismatch targetTask (taskId, actionId)) =
        diagnostic "RTE-013" "invalid continuation task mismatch"
            ("Continuation task mismatch: the target task is \x1b[31m" <> T.pack targetTask <> "\x1b[0m, but the only valid continuation is \x1b[31m" <> T.pack taskId <> "." <> T.pack actionId <> "\x1b[0m.")
    describe (EInvalidContinuationActionMismatch targetAction (taskId, actionId)) =
        diagnostic "RTE-014" "invalid continuation action mismatch"
            ("Continuation action mismatch: the target action is \x1b[31m" <> T.pack taskId <> "." <> T.pack targetAction <> "\x1b[0m, but the only valid continuation is \x1b[31m" <> T.pack taskId <> "." <> T.pack actionId <> "\x1b[0m.")
    describe (EDuplicatedMulticastContinuation (compId, actId) stepPos) =
        relatedTo stepPos "the continuation was previously selected here" $
        diagnostic "RTE-015" "duplicated multicast continuation"
            ("The multicast continuation \x1b[34m" <> T.pack compId <> "::" <> T.pack actId <> "\x1b[0m is duplicated in the current multicast step.\n")
    describe (EInvalidMulticastContinuation (compId, actId) validContinuations) =
        let validContText = T.intercalate ", " [ "\x1b[34m" <> T.pack tId <> "::" <> T.pack aId <> "\x1b[0m" | (tId, aId) <- validContinuations ]
        in
            diagnostic "RTE-016" "invalid multicast continuation"
                ("Invalid multicast continuation \x1b[31m" <> T.pack compId <> "::" <> T.pack actId <> "\x1b[0m. Valid continuations are: " <> validContText <> ".")
    describe (EExpectedMulticastContinuation validContinuations) =
        let validContText = T.intercalate ", " [ "\x1b[34m" <> T.pack tId <> "::" <> T.pack aId <> "\x1b[0m" | (tId, aId) <- validContinuations ]
        in
            diagnostic "RTE-017" "expected multicast continuation"
                ("Expected a multicast continuation. Valid continuations are: " <> validContText <> ".")
    describe (EPreviousTransactionWithSameName ident prevLoc) =
        relatedTo prevLoc "the previous transaction is defined here" $
        diagnostic "RTE-018" "previous transaction with same name"
            ("There is a previous transaction with the name \x1b[31m" <> T.pack ident <> "\x1b[0m.\n")
    describe (EPreviousSituationWithSameName ident prevLoc) =
        relatedTo prevLoc "the previous situation is defined here" $
        diagnostic "RTE-019" "previous situation with same name"
            ("There is a previous situation with the name \x1b[31m" <> T.pack ident <> "\x1b[0m.\n")
    describe (EConstExpressionTypeMismatch t1 t2) =
        diagnostic "RTE-020" "constant expression type mismatch"
            ("Constant expression type mismatch: found \x1b[31m" <> showText t1 <> "\x1b[0m and \x1b[31m" <> showText t2 <> "\x1b[0m.")
    describe EConditionalStepsMustHaveMultipleBranches =
        diagnostic "RTE-021" "conditional steps must have multiple branches"
            ("Conditional steps must have multiple branches.")
    describe (EMissingEventField fieldName) =
        diagnostic "RTE-022" "missing event field"
            ("Missing event field \x1b[31m" <> T.pack fieldName <> "\x1b[0m.")
    describe (EInvalidEventFieldType fieldName) =
        diagnostic "RTE-023" "invalid event field type"
            ("Invalid type for field \x1b[31m" <> T.pack fieldName <> "\x1b[0m.")
    describe (EUnknownTransaction ident) =
        diagnostic "RTE-024" "unknown transaction"
            ("Unknown transaction \x1b[31m" <> T.pack ident <> "\x1b[0m.")
    describe (EUnknownTransactionStep stepId (transId, transLoc)) =
        relatedTo transLoc "the transaction is defined here" $
        diagnostic "RTE-025" "unknown transaction step"
            ("Unknown step \x1b[31m" <> T.pack stepId <> "\x1b[0m in transaction \x1b[34m" <> T.pack transId <> "\x1b[0m.\n")
    describe (EDuplicateEventField fieldName fieldLoc) =
        relatedTo fieldLoc "the field was previously defined here" $
        diagnostic "RTE-026" "duplicate event field"
            ("The event field \x1b[34m" <> T.pack fieldName <> "\x1b[0m is duplicated in the current event definition.\n")
    describe (EDuplicateEventDefinition eventId eventLoc) =
        relatedTo eventLoc "the event was previously defined here" $
        diagnostic "RTE-027" "duplicate event definition"
            ("An event with name \x1b[34m" <> T.pack eventId <> "\x1b[0m already exists in the current situation.\n")
    describe (EEmitterTargetMismatch emitterId targetCmp actualCmp) =
        diagnostic "RTE-028" "emitter target mismatch"
            ("Emitter \x1b[31m" <> T.pack emitterId <> "\x1b[0m is connected to component \x1b[31m" <> T.pack actualCmp <> "\x1b[0m, but the target component is \x1b[31m" <> T.pack targetCmp <> "\x1b[0m.")
    describe (EUnknownEventEmitter emitterId) =
        diagnostic "RTE-029" "unknown event emitter"
            ("Unknown event emitter \x1b[31m" <> T.pack emitterId <> "\x1b[0m.")
    describe EInvalidEventDefinitionType =
        diagnostic "RTE-030" "invalid event definition type"
            ("Invalid event definition type. Expected a multiple field assignment.")
    describe EInvalidTransactionFieldType =
        diagnostic "RTE-031" "invalid transaction field type"
            ("Invalid transaction field type. Expected a transaction identifier.")
    describe EUnsupportedEmitterType =
        diagnostic "RTE-032" "unsupported emitter type"
            ("Unsupported emitter type. Only interrupt and periodic timer emitters are supported.")
    describe EInvalidDeadlineFieldType =
        diagnostic "RTE-033" "invalid deadline field type"
            ("Invalid deadline field type. Expected a multiple field assignment.")
    describe (EInvalidEventField fieldName validNames) =
        let validNamesText = T.intercalate ", " [ "\x1b[34m" <> T.pack name <> "\x1b[0m" | name <- validNames ]
        in
            diagnostic "RTE-034" "invalid event field"
                ("Invalid event field name \x1b[31m" <> T.pack fieldName <> "\x1b[0m. Valid field names are: " <> validNamesText <> ".")
    describe (EEmitterActionMismatch emitterId targetCmp targetAction (port, act, clsLoc)) =
        relatedTo clsLoc "the component class is defined here" $
        diagnostic "RTE-035" "emitter action mismatch"
            ("Emitter \x1b[31m" <> T.pack emitterId <> "\x1b[0m is connected to port \x1b[31m" <> T.pack port <> "\x1b[0m that triggers action \x1b[31m" <> T.pack act <> "\x1b[0m in component \x1b[31m" <> T.pack targetCmp <> "\x1b[0m, but the starting action of the transaction is \x1b[31m" <> T.pack targetAction <> "\x1b[0m.\n")
    describe EConditionalExpressionNotInteger =
        diagnostic "RTE-036" "conditional expression not integer"
            ("Conditional expression must evaluate to an integer between 1 and 100.")
    describe (EConditionalExpressionOutOfRange value) =
        diagnostic "RTE-037" "conditional expression out of range"
            ("Conditional expression value \x1b[31m" <> T.pack (show value) <> "\x1b[0m is out of range. Expected an integer between 1 and 100.")
    describe (EFlatConditionalExpressionOutOfRange value loc) =
        relatedTo loc "the inner conditional expression is defined here" $
        diagnostic "RTE-038" "conditional expression out of range in flattening"
            ("Error when flattening conditional expression: the resulting value \x1b[31m" <> T.pack (show value) <> "\x1b[0m is out of range. Expected an integer between 1 and 100.")
    describe EConstExpressionDivisionByZero =
        diagnostic "RTE-039" "constant expression division by zero"
            ("Division by zero in constant expression.")
    describe EInvalidEmitterFieldType =
        diagnostic "RTE-040" "invalid emitter field type"
            ("Invalid emitter field type. Expected a multiple field assignment.")
    describe EInvalidIntervalValue =
        diagnostic "RTE-041" "invalid interval value"
            ("Invalid interval value. Interval must be a positive number.")
    describe (EInvalidArrivalsValue value) =
        diagnostic "RTE-042" "invalid arrivals value"
            ("Invalid arrivals value \x1b[31m" <> T.pack (show value) <> "\x1b[0m. Arrivals must be a positive integer.")
    describe (EInvalidDeadlineValue value) =
        diagnostic "RTE-043" "invalid deadline value"
            ("Invalid deadline value \x1b[31m" <> T.pack (show value) <> "\x1b[0m. Deadline must be a positive number.")
    describe EInvalidInitialEndStep =
        diagnostic "RTE-044" "invalid initial end step"
            ("The initial step of a transaction cannot be an end step.")

    -- | Everything else is an error of the compiler, not of the model.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage RTErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
