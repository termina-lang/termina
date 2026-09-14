{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.BasicBlocks.Checks.ExitPaths.Errors where
import Utils.Annotations
import Utils.Errors

data ExitCheckError =
    EEInvalidCheckState -- ^ Invalid check state (Internal)
    | EEInvalidReturn -- ^ Invalid return statement (EE-001)
    | EEInvalidContinue -- ^ Invalid continue statement (EE-002)
    | EEBlockShallExit -- ^ Missing return statement (EE-003)
    | EEActionShallExit -- ^ Missing exit point on an action (EE-004)
    | EEActionInvalidContinue -- ^ Invalid continue statement on an action (EE-005)
    | EEActionInvalidSend -- ^ Invalid send statement on an action (EE-006)
    | EEActionIfBlockShallExit -- ^ Missing continue statement on an action if block (EE-007)
    | EEActionMatchBlockShallExit -- ^ Missing continue statement on an action match block (EE-008)
    | EEActionIfBlockShallNotExit -- ^ If block shall not exit (EE-009)
    | EEActionMatchBlockShallNotExit -- ^ Match block shall not exit (EE-010)
    | EEActionIfBlockMissingElseExit -- ^ Missing else exit on an action if block (EE-011)
    | EEInvalidReboot -- ^ Invalid reboot statement (EE-012)
    | EEActionInvalidReboot -- ^ Invalid reboot statement on an action (EE-013)
    deriving (Show)

type PathsCheckError = AnnotatedError ExitCheckError Location

instance Diagnosable ExitCheckError where

    describe EEInvalidReturn =
        diagnostic "EE-001" "invalid return statement"
            ("Invalid return statement.\n" <>
                "Return statements are only allowed as the last statement of a function.")
    describe EEInvalidContinue =
        diagnostic "EE-002" "invalid continue statement"
            ("Invalid continue statement.\n" <>
                "Continue statements are only allowed inside actions.")
    describe EEBlockShallExit =
        diagnostic "EE-003" "missing return statement"
            ("Missing return statement.\n" <>
                "All functions must have a return statement, even if they return nothing.")
    describe EEActionShallExit =
        diagnostic "EE-004" "missing exit point on an action"
            ("Missing exit point on an action.\n" <>
                "All the possible execution paths of an action must have an exit point (return or continue)")
    describe EEActionInvalidContinue =
        diagnostic "EE-005" "invalid continue statement on an action"
            ("Invalid continue statement.\n" <>
                "Continue statements are only allowed as the last statement of an execution path of an action.")
    describe EEActionInvalidSend =
        diagnostic "EE-006" "invalid send statement on an action"
            ("Invalid send statement.\n" <>
                "Send statements are only allowed at the end of an execution path of an action.")
    describe EEActionIfBlockShallExit =
        diagnostic "EE-007" "missing continue statement on an action if block"
            ("Missing continue statement.\n" <>
                "This if block is the last statement of an action and all its branches must have an exit point in the form of a continue statement.")
    describe EEActionMatchBlockShallExit =
        diagnostic "EE-008" "missing continue statement on an action match block"
            ("Missing continue statement.\n" <>
                "This match block is the last statement of an action and all its cases must have an exit point in the form of a continue statement.")
    describe EEActionIfBlockShallNotExit =
        diagnostic "EE-009" "if block shall not exit"
            ("Invalid continue statement.\n" <>
                "This if block is not the last statement of an action and thus at least one of its branches must not have an exit point.")
    describe EEActionMatchBlockShallNotExit =
        diagnostic "EE-010" "match block shall not exit"
            ("Invalid continue statement.\n" <>
                "This match block is not the last statement of an action and thus at least one of its cases must not have an exit point.")
    describe EEActionIfBlockMissingElseExit =
        diagnostic "EE-011" "missing else exit on an action if block"
            ("Missing continue statement.\n" <>
                "This if block is the last statement of an action and all its branches must have an exit point in the form of a continue statement. Thus, it must have an else branch.")
    describe EEInvalidReboot =
        diagnostic "EE-012" "invalid reboot statement"
            ("Invalid reboot statement.\n" <>
                "Reboot statements are only allowed inside actions.")
    describe EEActionInvalidReboot =
        diagnostic "EE-013" "invalid reboot statement on an action"
            ("Invalid reboot statement.\n" <>
                "Reboot statements are only allowed as the last statement of an execution path of an action.")
    describe EEInvalidCheckState = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage PathsCheckError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
