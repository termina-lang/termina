{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.BasicBlocks.Checks.ExitPaths.Errors where
import Utils.Annotations
import Core.AST
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
    deriving (Show)

type PathsCheckError = AnnotatedError ExitCheckError Location

instance ErrorMessage PathsCheckError where

    errorIdent (AnnotatedError EEInvalidReturn _pos) = "EE-001"
    errorIdent _ = "Unkonwn"

    errorTitle (AnnotatedError EEInvalidReturn _pos) = "invalid return statement"
    errorTitle _ = "Unknown"

    toText e@(AnnotatedError EEInvalidReturn pos@(Position start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            pprintSimpleError
                sourceLines title fileName pos
                (Just ("Invalid return statement.\n" <>
                       "Return statements are only allowed as the last statement of a function."))
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostic e@(AnnotatedError EEInvalidReturn pos) _files =
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

data TaskBoxAllocationError = 
    TBAEInvalidFree Identifier -- ^ Freeing a box into a different port (TBAE-001)
    | TBAEBoxSendBoxDifferentAllocators Identifier -- ^ Sending boxes from different allocators (TBAE-002)

type TBACheckError = AnnotatedError TaskBoxAllocationError Location
