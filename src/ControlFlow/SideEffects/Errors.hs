{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.SideEffects.Errors where

import Utils.Annotations
import Utils.Errors
import Text.Parsec (sourceName)
import qualified Data.Map as M
import qualified Language.LSP.Protocol.Types as LSP


data Error =
    EPreviousMutableBorrow Location -- ^ Second mutable reference to the same object (SEF-001)
  | EMultipleSideEffects Location -- ^ Multiple side effects in one expression (SEF-002)
  | EInterferingSideEffect Location -- ^ Side effect and an access to the same object (SEF-003)
  | ESideEffectInInitializerList -- ^ Side effect in an initializer list (SEF-004)
  | ESideEffectInRHSLogicalAnd -- ^ Side effect in the right operand of && (SEF-005)
  | ESideEffectInRHSLogicalOr -- ^ Side effect in the right operand of || (SEF-006)
  deriving Show

type SideEffectsError = AnnotatedError Error Location

instance ErrorMessage SideEffectsError where

    errorIdent (AnnotatedError (EPreviousMutableBorrow _) _)    = "SEF-001"
    errorIdent (AnnotatedError (EMultipleSideEffects _) _)      = "SEF-002"
    errorIdent (AnnotatedError (EInterferingSideEffect _) _)    = "SEF-003"
    errorIdent (AnnotatedError ESideEffectInInitializerList _)  = "SEF-004"
    errorIdent (AnnotatedError ESideEffectInRHSLogicalAnd _)    = "SEF-005"
    errorIdent (AnnotatedError ESideEffectInRHSLogicalOr _)     = "SEF-006"

    errorTitle (AnnotatedError (EPreviousMutableBorrow _) _)    = "second mutable reference to the same object"
    errorTitle (AnnotatedError (EMultipleSideEffects _) _)      = "multiple side effects in one expression"
    errorTitle (AnnotatedError (EInterferingSideEffect _) _)    = "a side effect and an access to the same object"
    errorTitle (AnnotatedError ESideEffectInInitializerList _)  = "side effect in an initializer list"
    errorTitle (AnnotatedError ESideEffectInRHSLogicalAnd _)    = "side effect in the right operand of &&"
    errorTitle (AnnotatedError ESideEffectInRHSLogicalOr _)     = "side effect in the right operand of ||"

    toText e@(AnnotatedError err pos@(Position _ start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of
                EPreviousMutableBorrow prevPos@(Position _ prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("A mutable reference to this object is taken while another mutable reference to it is still live in the same expression.\n" <>
                            "At most one mutable reference to a given object may appear in a single expression.\n")) <>
                    pprintSimpleError
                        prevSourceLines "The other mutable reference was taken here:" prevFileName prevPos Nothing
                EMultipleSideEffects prevPos@(Position _ prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("This expression has a persistent side effect, and another side effect appears unordered in the same expression.\n" <>
                            "Their evaluation order is unspecified.")) <>
                    pprintSimpleError
                        prevSourceLines "The other side effect is here:" prevFileName prevPos Nothing
                EInterferingSideEffect prevPos@(Position _ prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("This accesses an object that is modified, unordered, elsewhere in the same expression.\n" <>
                            "Its value would depend on the evaluation order.")) <>
                    pprintSimpleError
                        prevSourceLines "The object is modified here:" prevFileName prevPos Nothing
                ESideEffectInInitializerList ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "An initializer list must not contain a persistent side effect.")
                ESideEffectInRHSLogicalAnd ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The right-hand operand of && must not contain a persistent side effect; short-circuit evaluation may skip it.")
                ESideEffectInRHSLogicalOr ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The right-hand operand of || must not contain a persistent side effect; short-circuit evaluation may skip it.")
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError _ pos) _files =
        error $ "Internal error: invalid error position: " ++ show pos

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."