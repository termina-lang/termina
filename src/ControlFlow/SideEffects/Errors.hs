{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.SideEffects.Errors where

import Utils.Annotations
import Utils.Errors

data Error =
    EPreviousMutableBorrow Location -- ^ Second mutable reference to the same object (SEF-001)
  | EMultipleSideEffects Location -- ^ Multiple side effects in one expression (SEF-002)
  | EInterferingSideEffect Location -- ^ Side effect and an access to the same object (SEF-003)
  | ESideEffectInInitializerList -- ^ Side effect in an initializer list (SEF-004)
  | ESideEffectInRHSLogicalAnd -- ^ Side effect in the right operand of && (SEF-005)
  | ESideEffectInRHSLogicalOr -- ^ Side effect in the right operand of || (SEF-006)
  deriving Show

type SideEffectsError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EPreviousMutableBorrow prevPos) =
        relatedTo prevPos "the other mutable reference" $
            diagnostic "SEF-001" "second mutable reference to the same object"
                ("A mutable reference to this object is taken while another mutable reference to it is still live in the same expression.\n" <>
                    "At most one mutable reference to a given object may appear in a single expression.\n")
    describe (EMultipleSideEffects prevPos) =
        relatedTo prevPos "the other side effect" $
            diagnostic "SEF-002" "multiple side effects in one expression"
                ("This expression has a persistent side effect, and another side effect appears unordered in the same expression.\n" <>
                    "Their evaluation order is unspecified.")
    describe (EInterferingSideEffect prevPos) =
        relatedTo prevPos "the object is modified here" $
            diagnostic "SEF-003" "a side effect and an access to the same object"
                ("This accesses an object that is modified, unordered, elsewhere in the same expression.\n" <>
                    "Its value would depend on the evaluation order.")
    describe ESideEffectInInitializerList =
        diagnostic "SEF-004" "side effect in an initializer list"
            "An initializer list must not contain a persistent side effect."
    describe ESideEffectInRHSLogicalAnd =
        diagnostic "SEF-005" "side effect in the right operand of &&"
            "The right-hand operand of && must not contain a persistent side effect; short-circuit evaluation may skip it."
    describe ESideEffectInRHSLogicalOr =
        diagnostic "SEF-006" "side effect in the right operand of ||"
            "The right-hand operand of || must not contain a persistent side effect; short-circuit evaluation may skip it."

instance ErrorMessage SideEffectsError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
