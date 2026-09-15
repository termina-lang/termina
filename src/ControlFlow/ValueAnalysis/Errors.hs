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
    -- variable on the way in.
  | Refined Location
  deriving (Eq, Ord, Show)

-- | Why the pass knows the value of one of the names a condition reads. The
-- origins are empty when the name is a constant of the module, since a
-- constant has no place in the body to point at.
data Reason = Reason
  {
    reasonName :: Identifier
  , reasonValue :: Const SemanticAnn
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
            <> emph (bareValue (reasonValue reason))

        -- | The value without the type annotation a literal carries, since the
        -- declaration the message points at already gives the type.
        bareValue (I i _) = showText i
        bareValue other = showText other

        point diag reason =
            foldl (sends (reasonName reason)) diag (reasonOrigins reason)

        sends name diag (Assigned loc) =
            relatedTo loc (T.pack name <> " takes that value here") diag
        sends name diag (Refined loc) =
            relatedTo loc (T.pack name <> " is fixed at that value by this condition") diag

instance ErrorMessage ValueAnalysisError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
