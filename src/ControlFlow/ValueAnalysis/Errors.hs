{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.ValueAnalysis.Errors where

import Semantic.AST
import Semantic.Types
import Utils.Annotations
import Utils.Errors
import Utils.Printer

data Error =
    EInvariantCondition (Const SemanticAnn) -- ^ Condition with the same value on every evaluation (VAE-001)
  deriving Show

type ValueAnalysisError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EInvariantCondition value) =
        diagnostic "VAE-001" "invariant control expression"
            ("This condition evaluates to " <> emph (showText value) <>
                " every time it is reached, so one of the paths it guards is never taken.")

instance ErrorMessage ValueAnalysisError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
