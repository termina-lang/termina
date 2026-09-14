{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.MAST.Errors where
import Utils.Annotations
import qualified Data.Text as T
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
    | EUnsupportedSystemCall Identifier -- ^ Unsupported system call in transactional step (internal)
    | EInvalidTimerPeriodExpression -- ^ Invalid timer period expression in periodic timer emitter (internal)
    | EInvalidEmitterType -- ^ Invalid emitter type for MAST code generation (internal)
    | EInvalidTransactionStructure -- ^ Invalid structure of MAST transaction (internal)
    | EUnsupportedPlatform Identifier -- ^ Unsupported platform for MAST code generation
    | EUnsupportedSystemInitEmitter -- ^ System initialization emitter is not supported for MAST code generation
    | EUnsupportedSystemExceptEmitter -- ^ System exception emitter is not supported for MAST code generation
    deriving Show

type MASTGenErrors = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EUnsupportedPlatform pltName) =
        diagnostic "MSTE-001" "unsupported platform for MAST code generation"
            ("The platform " <> emph (T.pack pltName) <>
                " is not supported for MAST code generation.")
    describe EUnsupportedSystemInitEmitter =
        diagnostic "MSTE-002" "system initialization emitter is not supported for MAST code generation"
            "MAST code generation does not support system initialization emitters."
    describe EUnsupportedSystemExceptEmitter =
        diagnostic "MSTE-003" "system exception emitter is not supported for MAST code generation"
            "MAST code generation does not support system exception emitters."
    -- | Everything else is a broken invariant of the compiler, which has no code
    -- of its own.
    describe _ = diagnosticWithoutDetail "Internal" "internal error"

instance ErrorMessage MASTGenErrors where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
