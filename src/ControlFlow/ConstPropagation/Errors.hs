{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.ConstPropagation.Errors where

import Semantic.AST
import Utils.Annotations
import Utils.Errors
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
import Semantic.Types


data Error =
  EUnboxingEmitter -- ^ Unboxing emitter (Internal)
  | EUnboxingObject -- ^ Unboxing object (Internal)
  | EUnboxingExpression -- ^ Unboxing expression (Internal)
  | EUnknownIdentifier Identifier -- ^ Unknown identifier (Internal)
  | EInvalidObject -- ^ Invalid object (Internal)
  | EInvalidExpression -- ^ Invalid expression (Internal)
  | EInvalidConstantEvaluation -- ^ Invalid constant evaluation (Internal)
  | ENotConstant -- ^ Not constant (Internal)
  | EAtomicArrayConnectionSizeMismatch Integer Integer -- ^ Atomic array connection size mismatch
  | EArrayInitializerSizeMismatch Integer Integer -- ^ Array initializer size mismatch
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ Array expression list array initializer size mismatch
  | EStringInitializerSizeMismatch Integer Integer -- ^ String initializer size mismatch
  | EConstIntegerOverflow Integer (TerminaType SemanticAnn) -- ^ Constant integer overflow
  | EConstIntegerUnderflow Integer (TerminaType SemanticAnn) -- ^ Constant integer overflow
  | EConstDivisionByZero -- ^ Constant division by zero
  deriving Show

type ConstPropError = AnnotatedError Error Location

instance ErrorMessage ConstPropError where

    errorIdent (AnnotatedError (EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize) _pos) = "CPE-001"
    errorIdent (AnnotatedError (EArrayInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "CPE-002"
    errorIdent (AnnotatedError (EArrayExprListInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "CPE-003"
    errorIdent (AnnotatedError (EStringInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "CPE-004"
    errorIdent _ = "Internal"

    errorTitle (AnnotatedError (EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize) _pos) = "atomic array connection size mismatch"
    errorTitle (AnnotatedError (EArrayInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "array initializer size mismatch"
    errorTitle (AnnotatedError (EArrayExprListInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "array expression list initializer size mismatch"
    errorTitle (AnnotatedError (EStringInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "string initializer size mismatch"
    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText e@(AnnotatedError err pos@(Position start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of 
                EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> -- showText expectedSize <>
                            "\x1b[0m but the array has size ...")) -- \x1b[31m" <> showText actualSize <> "\x1b[0m."))
                EArrayInitializerSizeMismatch _expectedSize _initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the array initializer is \x1b[31m" <> -- showText initializerSize <>
                            "\x1b[0m but the expected size is ...")) -- \x1b[31m" <> showText expectedSize <> "\x1b[0m."))
                EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                            "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                EStringInitializerSizeMismatch expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the string initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                            "\x1b[0m but the array size is of \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                _ -> pprintSimpleError sourceLines title fileName pos Nothing

    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."