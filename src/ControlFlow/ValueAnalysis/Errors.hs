{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.ValueAnalysis.Errors where

import qualified Data.Text as T

import Semantic.AST
import Semantic.Types
import Utils.Annotations
import Utils.Errors
import Utils.Printer

-- | Where a name got the value it holds where the condition is reported, which
-- is the place the message sends the reader to.
data Origin =
    -- | A declaration with an initializer, or an assignment of the whole
    -- variable.
    Assigned Location
    -- | The condition of a branch the report sits inside of, which fixed the
    -- variable at a value on the way in.
  | Refined Location
    -- | The same, for a condition that bounds the variable instead of fixing
    -- it, which is what an order comparison does.
  | Bounded Location
    -- | The loop that declares the variable and runs it over a range.
  | Iterated Location
  deriving (Eq, Ord, Show)

-- | What the pass knows a name holds, in the three shapes a message can say
-- it. The pass fills this in from its own lattice, which the message does not
-- need to know about.
data Holds =
    -- | A single value, which is what a condition read outright gives.
    OneValue (Const SemanticAnn)
    -- | A handful of values, none of which the paths rule out. Never empty and
    -- never of one element, which is 'OneValue'.
  | OneOf [Integer]
    -- | Both ends included.
  | Between Integer Integer
  deriving Show

-- | Why the pass knows what one of the names a condition reads may hold. The
-- origins are empty when the name is a constant of the module, since a
-- constant has no place in the body to point at.
data Reason = Reason
  {
    reasonName :: Identifier
  , reasonHolds :: Holds
  , reasonOrigins :: [Origin]
  }
  deriving Show

data Error =
    -- | Condition with the same value on every evaluation, with what the pass
    -- knows of each name it reads (VAE-001)
    EInvariantCondition (Const SemanticAnn) [Reason]
  deriving Show

type ValueAnalysisError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EInvariantCondition value reasons) =
        foldl point explained reasons

      where

        explained =
            diagnostic "VAE-001" "invariant control expression" detail

        -- | The values come first and the verdict after them, since the values
        -- are what the reader has to check to agree with the verdict.
        detail = holds <> "This condition evaluates to " <> emph (showText value)
            <> " every time it is reached, so one of the paths it guards is"
            <> " never taken."

        holds = case reasons of
            [] -> ""
            _ -> "Here " <> T.intercalate ", " (map says reasons) <> ". "

        says reason = emph (T.pack (reasonName reason)) <> " holds "
            <> saysHolds (reasonHolds reason)

        saysHolds (OneValue value) = emph (bareValue value)
        saysHolds (OneOf values) = listed (map (emph . number) values)
        saysHolds (Between lo hi) = "a value between " <> emph (number lo)
            <> " and " <> emph (number hi)

        -- | The value without the type annotation a literal carries, since the
        -- declaration the message points at already gives the type.
        bareValue (I i _) = showText i
        bareValue other = showText other

        number = T.pack . show

        -- | The last of several values is joined with "or", since what the
        -- reader has to take in is that any of them is possible.
        listed [] = ""
        listed [one] = one
        listed values = T.intercalate ", " (init values) <> " or " <> last values

        point diag reason =
            foldl (sends (reasonName reason)) diag (reasonOrigins reason)

        sends name diag (Assigned loc) =
            relatedTo loc (T.pack name <> " takes that value here") diag
        sends name diag (Refined loc) =
            relatedTo loc (T.pack name <> " is fixed at that value by this condition") diag
        sends name diag (Bounded loc) =
            relatedTo loc (T.pack name <> " is bounded by this condition") diag
        sends name diag (Iterated loc) =
            relatedTo loc (T.pack name <> " takes its values from this loop") diag

instance ErrorMessage ValueAnalysisError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
