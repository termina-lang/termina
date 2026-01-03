{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.TransPath.Errors where
import Utils.Annotations
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Utils.Errors
import EFP.Schedulability.Core.AST
import Text.Parsec
import qualified Data.Map as M

--------------------------------------------------
-- Transactional Path Generator error handling
--------------------------------------------------

data Error
  = 
    EInvalidTransStepType -- ^ Invalid transactional step type (internal)
    | EUnknownComponent Identifier -- ^ Unknown component referenced in transactional step (internal)
    | EUnknownAction -- ^ Unknown action referenced in transactional step (internal)
    | EInvalidForLoop -- ^ Invalid for-loop structure in transactional step (internal)
    | EInvalidArgumentPassing -- ^ Invalid argument passing to transactional step (internal)
    | EUnknownAccessPort Identifier Identifier -- ^ Unknown access port referenced in worst-case execution path (internal)
    | EInvalidWCETExpression -- ^ Invalid worst-case execution time expression (internal)
    | EConstExpressionDivisionByZero -- ^ Division by zero in constant expression (internal)
    | EInvalidConstExpressionOperand Op -- ^ Invalid operand for constant expression (internal)
    | EInvalidRTElementForTransPath -- ^ Invalid RT element provided for transactional path generation (internal)
    | EConstExpressionTypeMismatch ConstExprType ConstExprType -- ^ Type mismatch in constant expression (internal)
    | EInvalidConstExpressionOperandTypes -- ^ Invalid operand types for constant expression (internal)
    | EUnknownConstant Identifier -- ^ Unknown constant in constant expression (internal)
    | ENoPathsFound Identifier Identifier -- ^ No worst-case paths found for the given component and member names
    | ENoWCETForPath Identifier Identifier Identifier Identifier -- ^ No worst-case execution time found
    deriving Show

type TRPGenErrors = AnnotatedError Error Location

instance ErrorMessage TRPGenErrors where

    errorIdent (AnnotatedError (ENoPathsFound {} ) _pos) = "TPE-001"
    errorIdent (AnnotatedError (ENoWCETForPath {} ) _pos) = "TPE-002"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (ENoPathsFound {} ) _pos) = "no worst-case execution paths found"
    errorTitle (AnnotatedError (ENoWCETForPath {} ) _pos) = "no worst-case execution time found"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of 
                ENoPathsFound componentClass memberName ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("No worst-case paths found for member function \x1b[31m" <> T.pack componentClass <> "::" <> T.pack memberName <> "\x1b[0m.")) 
                ENoWCETForPath componentClass funcName pathId plt ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("No worst-case execution time found for path \x1b[31m" <> T.pack componentClass 
                        <> "::"  <> T.pack funcName <> "::" <> T.pack pathId <> "\x1b[0m on platform \x1b[31m" <> T.pack plt <> "\x1b[0m.")) 
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."