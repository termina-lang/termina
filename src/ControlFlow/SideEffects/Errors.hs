{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.SideEffects.Errors where

import Core.AST (Identifier)
import Utils.Annotations
import Utils.Errors
import Data.Char (toLower)
import qualified Data.Text as T

-- | What makes an expression carry an effect that outlives it, and where that
-- effect is. Two of the three are written as a read, so the message has to say
-- what the generated code does with them.
data Effect =
    MutatesThroughCall Location -- ^ A call that takes a mutable reference
  | MutatesReceiver Location -- ^ A call to a method that takes a mutable self
  | ReadsLocation Location -- ^ An access that goes through a field declared loc
  | ChecksIndex Location -- ^ An array access with an index that is not constant
  | CallsEffectful Location Identifier Effect -- ^ A call to something whose body carries one
  deriving Show

data Error =
    EPreviousMutableBorrow Location -- ^ Second mutable reference to the same object (SEF-001)
  | EMultipleSideEffects Location -- ^ Multiple side effects in one expression (SEF-002)
  | EInterferingSideEffect Location -- ^ Side effect and an access to the same object (SEF-003)
  | ESideEffectInInitializerList Effect -- ^ Side effect in an initializer list (SEF-004)
  | ESideEffectInRHSLogicalAnd Effect -- ^ Side effect in the right operand of && (SEF-005)
  | ESideEffectInRHSLogicalOr Effect -- ^ Side effect in the right operand of || (SEF-006)
  | ESideEffectInLoopGuard Effect -- ^ Side effect in the guard of a for loop (SEF-007)
  deriving Show

type SideEffectsError = AnnotatedError Error Location

-- | What the effect is, said before the rule, since the reader is looking at
-- something that in Termina is written as a call or as a read and has to be
-- told what the generated code does with it.
saysEffect :: Effect -> T.Text
saysEffect (MutatesThroughCall _) =
    "This call takes a mutable reference, so it writes state that outlives the expression. "
saysEffect (MutatesReceiver _) =
    "This method takes self mutably, so it writes the state of the class, which outlives the expression. "
saysEffect (ReadsLocation _) =
    "This access goes through a field declared loc, which lives at a fixed address and which the generated code reaches through a pointer to volatile, so the access is kept where it is written and two reads of it may give different values. "
saysEffect (ChecksIndex _) =
    "The index of this array access is not a constant, so the generated code checks it against the size of the array while the program runs, and the check raises an exception when the index falls outside. "
saysEffect (CallsEffectful _ name inner) =
    "This call reaches " <> emph (T.pack name) <> ", and " <> untitle (saysEffect inner)

-- | The effect of what a call reaches is said as part of the sentence that
-- names the call, so its own sentence starts in lower case.
untitle :: T.Text -> T.Text
untitle text = case T.uncons text of
    Just (c, rest) -> T.cons (toLower c) rest
    Nothing        -> text

-- | Adds the pointer to the place the effect is, which is inside the operand
-- the error already points at.
pointsAtEffect :: Effect -> Diagnostic -> Diagnostic
pointsAtEffect (MutatesThroughCall loc) = relatedTo loc "this call writes through a mutable reference"
pointsAtEffect (MutatesReceiver loc) = relatedTo loc "this method writes the state of the class"
pointsAtEffect (ReadsLocation loc) = relatedTo loc "this access goes through a field declared loc"
pointsAtEffect (ChecksIndex loc) = relatedTo loc "this index is checked while the program runs"
pointsAtEffect (CallsEffectful loc _ inner) =
    pointsAtEffect inner . relatedTo loc "this call reaches the effect"

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
    describe (ESideEffectInInitializerList effect) =
        pointsAtEffect effect $
            diagnostic "SEF-004" "side effect in an initializer list"
                (saysEffect effect <>
                    "An element of an initializer list must not carry an effect that outlives the expression.")
    describe (ESideEffectInRHSLogicalAnd effect) =
        pointsAtEffect effect $
            diagnostic "SEF-005" "side effect in the right operand of &&"
                (saysEffect effect <>
                    "The right-hand operand of && is evaluated only when the left one is true, so the effect happens or not depending on its value.")
    describe (ESideEffectInRHSLogicalOr effect) =
        pointsAtEffect effect $
            diagnostic "SEF-006" "side effect in the right operand of ||"
                (saysEffect effect <>
                    "The right-hand operand of || is evaluated only when the left one is false, so the effect happens or not depending on its value.")
    describe (ESideEffectInLoopGuard effect) =
        pointsAtEffect effect $
            diagnostic "SEF-007" "side effect in the guard of a loop"
                (saysEffect effect <>
                    "The generated loop tests the range of the iterator first and this guard after it, joined by an &&, so the guard is evaluated only while the iterator stays within its range and the effect happens or not depending on it.")

instance ErrorMessage SideEffectsError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
