{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.BasicBlocks.Checks.ExitPaths.Errors where
import Utils.Annotations
import Utils.Errors
import Text.Parsec
import qualified Data.Map as M
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Text as T

data ExitCheckError =
    EEInvalidCheckState -- ^ Invalid check state (Internal)
    | EEInvalidReturn -- ^ Invalid return statement (EE-001)
    | EEInvalidContinue -- ^ Invalid continue statement (EE-002)
    | EEInvalidSend -- ^ Invalid send statement (EE-003)
    | EEBlockShallExit -- ^ Missing return statement (EE-004)
    | EEActionShallExit -- ^ Missing exit point on an action (EE-005)
    | EEActionInvalidContinue -- ^ Invalid continue statement on an action (EE-006)
    | EEActionInvalidSend -- ^ Invalid send statement on an action (EE-007)
    | EEActionIfBlockShallExit -- ^ Missing continue statement on an action if block (EE-008)
    | EEActionMatchBlockShallExit -- ^ Missing continue statement on an action match block (EE-009)
    | EEActionIfBlockShallNotExit -- ^ If block shall not exit (EE-010)
    | EEActionMatchBlockShallNotExit -- ^ Match block shall not exit (EE-011)
    | EEActionIfBlockMissingElseExit -- ^ Missing else exit on an action if block (EE-012)
    | EEInvalidReboot -- ^ Invalid reboot statement (EE-013)
    | EEActionInvalidReboot -- ^ Invalid reboot statement on an action (EE-014)
    deriving (Show)

type PathsCheckError = AnnotatedError ExitCheckError Location

instance ErrorMessage PathsCheckError where

    errorIdent (AnnotatedError EEInvalidReturn _pos) = "EE-001"
    errorIdent (AnnotatedError EEInvalidContinue _pos) = "EE-002"
    errorIdent (AnnotatedError EEInvalidSend _pos) = "EE-003"
    errorIdent (AnnotatedError EEBlockShallExit _pos) = "EE-004"
    errorIdent (AnnotatedError EEActionShallExit _pos) = "EE-005"
    errorIdent (AnnotatedError EEActionInvalidContinue _pos) = "EE-006"
    errorIdent (AnnotatedError EEActionInvalidSend _pos) = "EE-007"
    errorIdent (AnnotatedError EEActionIfBlockShallExit _pos) = "EE-008"
    errorIdent (AnnotatedError EEActionMatchBlockShallExit _pos) = "EE-009"
    errorIdent (AnnotatedError EEActionIfBlockShallNotExit _pos) = "EE-010"
    errorIdent (AnnotatedError EEActionMatchBlockShallNotExit _pos) = "EE-011"
    errorIdent (AnnotatedError EEActionIfBlockMissingElseExit _pos) = "EE-012"
    errorIdent (AnnotatedError EEInvalidReboot _pos) = "EE-013"
    errorIdent (AnnotatedError EEActionInvalidReboot _pos) = "EE-014"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError EEInvalidReturn _pos) = "invalid return statement"
    errorTitle (AnnotatedError EEInvalidContinue _pos) = "invalid continue statement"
    errorTitle (AnnotatedError EEInvalidSend _pos) = "invalid send statement"
    errorTitle (AnnotatedError EEBlockShallExit _pos) = "missing return statement"
    errorTitle (AnnotatedError EEActionShallExit _pos) = "missing exit point on an action"
    errorTitle (AnnotatedError EEActionInvalidContinue _pos) = "invalid continue statement on an action"
    errorTitle (AnnotatedError EEActionInvalidSend _pos) = "invalid send statement on an action"
    errorTitle (AnnotatedError EEActionIfBlockShallExit _pos) = "missing continue statement on an action if block"
    errorTitle (AnnotatedError EEActionMatchBlockShallExit _pos) = "missing continue statement on an action match block"
    errorTitle (AnnotatedError EEActionIfBlockShallNotExit _pos) = "if block shall not exit"
    errorTitle (AnnotatedError EEActionMatchBlockShallNotExit _pos) = "match block shall not exit"
    errorTitle (AnnotatedError EEActionIfBlockMissingElseExit _pos) = "missing else exit on an action if block"
    errorTitle (AnnotatedError EEInvalidReboot _pos) = "invalid reboot statement"
    errorTitle (AnnotatedError EEActionInvalidReboot _pos) = "invalid reboot statement on an action"
    errorTitle _ = "internal error"

    toText e@(AnnotatedError err pos@(Position start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of
                EEInvalidReturn ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid return statement.\n" <>
                            "Return statements are only allowed as the last statement of a function."))
                EEInvalidContinue ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid continue statement.\n" <>
                            "Continue statements are only allowed inside actions."))
                EEInvalidSend ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid send statement.\n" <>
                            "Send statements are only allowed inside actions."))
                EEBlockShallExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Missing return statement.\n" <>
                            "All functions must have a return statement, even if they return nothing."))
                EEActionShallExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Missing exit point on an action.\n" <>
                            "All the possible execution paths of an action must have an exit point (return or continue)"))
                EEActionInvalidContinue ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid continue statement.\n" <>
                            "Continue statements are only allowed as the last statement of an execution path of an action."))
                EEActionInvalidSend ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid send statement.\n" <>
                            "Send statements are only allowed at the end of an execution path of an action."))
                EEActionIfBlockShallExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Missing continue statement.\n" <>
                            "This if block is the last statement of an action and all its branches must have an exit point in the form of a continue statement."))
                EEActionMatchBlockShallExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Missing continue statement.\n" <>
                            "This match block is the last statement of an action and all its cases must have an exit point in the form of a continue statement."))
                EEActionIfBlockShallNotExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid continue statement.\n" <>
                            "This if block is not the last statement of an action and thus at least one of its branches must not have an exit point."))
                EEActionMatchBlockShallNotExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid continue statement.\n" <>
                            "This match block is not the last statement of an action and thus at least one of its cases must not have an exit point."))
                EEActionIfBlockMissingElseExit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Missing continue statement.\n" <>
                            "This if block is the last statement of an action and all its branches must have an exit point in the form of a continue statement. Thus, it must have an else branch."))
                EEInvalidReboot ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid reboot statement.\n" <>
                            "Reboot statements are only allowed inside actions."))
                EEActionInvalidReboot ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid reboot statement.\n" <>
                            "Reboot statements are only allowed as the last statement of an execution path of an action."))
                _ -> T.pack $ show pos ++ ": " ++ show e

    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."
