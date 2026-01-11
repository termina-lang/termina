{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.MAST.Errors where
import Utils.Annotations
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Utils.Errors
import EFP.Schedulability.Core.AST

--------------------------------------------------
-- Transactional Path Generator error handling
--------------------------------------------------

data Error
  = 
    EUnknownComponent Identifier -- ^ Unknown component referenced in transactional step (internal)
    | EUnknownPlatform Identifier -- ^ Unknown platform specified in configuration (internal)
    | EUnknownEmitter Identifier -- ^ Unknown emitter ID (internal)
    | EUnknownTask Identifier -- ^ Unknown task referenced in transactional step (internal)
    | EUnknownResource Identifier -- ^ Unknown resource referenced in transactional step (internal)
    | EUnknownSinkPort Identifier Identifier -- ^ Unknown sink port referenced in transactional step (internal)
    | EUnknownStep Identifier -- ^ Unknown step referenced in transactional step (internal)
    | EInvalidInitialStep Identifier -- ^ Invalid initial step identifier for MAST generation (internal)
    | EInvalidStepType Identifier -- ^ Invalid step type for transactional step (internal)
    | EUnsupportedConstExpression -- ^ Unsupported constant expression in operation postfix (internal)
    | EUnsupportedSystemCall Identifier -- ^ Unsupported system call in transactional step (internal)
    | EInvalidTimerPeriodExpression -- ^ Invalid timer period expression in periodic timer emitter (internal)
    | EInvalidEmitterType -- ^ Invalid emitter type for MAST code generation (internal)
    | EInvalidTransactionStructure -- ^ Invalid structure of MAST transaction (internal)
    | EUnsupportedPlatform Identifier -- ^ Unsupported platform for MAST code generation
    | EUnsupportedSystemInitEmitter -- ^ System initialization emitter is not supported for MAST code generation
    | EUnsupportedSystemExceptEmitter -- ^ System exception emitter is not supported for MAST code generation
    deriving Show

type MASTGenErrors = AnnotatedError Error Location

instance ErrorMessage MASTGenErrors where

    errorIdent (AnnotatedError (EUnsupportedPlatform {} ) _pos) = "MSTE-001"
    errorIdent (AnnotatedError (EUnsupportedSystemInitEmitter {}) _pos) = "MSTE-002"
    errorIdent (AnnotatedError (EUnsupportedSystemExceptEmitter {}) _pos) = "MSTE-003"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (EUnsupportedPlatform {} ) _pos) = "unsupported platform for MAST code generation"
    errorTitle (AnnotatedError (EUnsupportedSystemInitEmitter {}) _pos) = "system initialization emitter is not supported for MAST code generation"
    errorTitle (AnnotatedError (EUnsupportedSystemExceptEmitter {}) _pos) = "system exception emitter is not supported for MAST code generation"
    errorTitle (AnnotatedError _err _pos) = "internal error"


    toText e@(AnnotatedError (EUnsupportedPlatform pltName) _) _files =
        let title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "." in
        title <> "\nThe platform \x1b[31m" <> T.pack pltName <> "\x1b[0m is not supported for MAST code generation."
    toText e@(AnnotatedError EUnsupportedSystemInitEmitter _) _files =
        let title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "." in
        title <> "\nMAST code generation does not support system initialization emitters."
    toText e@(AnnotatedError EUnsupportedSystemExceptEmitter _) _files =
        let title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "." in
        title <> "\nMAST code generation does not support system exception emitters."
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."